#!/usr/bin/env bash
set -euo pipefail

# 静默运行：不打印到终端，但默认写入日志文件，便于排查 5 小时后的提交/推送是否成功
# 修复：默认日志路径改为 $HOME 目录，避免 /tmp 权限冲突
# 如仍想彻底丢弃日志：export LOG_FILE=/dev/null
LOG_FILE="${LOG_FILE:-${HOME}/.claude-cabal-autoloop.log}"

# 尝试创建/触碰日志文件，检查写入权限。如果失败，则回退到 /dev/null
if ! touch "$LOG_FILE" 2>/dev/null || [[ ! -w "$LOG_FILE" ]]; then
  echo "Warning: Cannot write to log file '$LOG_FILE'. Falling back to /dev/null." >&2
  LOG_FILE="/dev/null"
fi

exec >>"$LOG_FILE" 2>&1

###############################################################################
# claude-cabal-autoloop.sh (Direct Push Mode)
# - 单文件融合版：等价于 claude-cabal-loop.yml + scripts/typus_cabal_loop.sh
# - 非 GitHub Actions 环境运行
# - 使用 Claude Code CLI (@anthropic-ai/claude-code)
# - 已移除 watchdog/heartbeat 机制
#
# 工作模式变更：
# - 移除了 Pull Request (PR) 创建逻辑。
# - 直接在 WORK_BRANCH (默认 master) 上进行提交和推送。
# - 下一轮循环开始前会自动拉取最新代码，确保基于最新的代码继续工作。
#
# 修复点（继承自原 iflow 版本）：
# A) derive_github_repo：修复 GitHub remote URL 正则，兼容 https/ssh/scp 风格
# B) ps_children_of：移除不可靠的 `ps ... -ppid` 分支，改为失败即回退到通用枚举过滤
# C) set -e 模式下的 git 操作保护：关键 git 失败 return 而非 exit，保护外层重试
#
# 新增/修改（Claude 版本）：
# - 移除 NVIDIA OpenAI 接口配置，改用 Anthropic 原生 API Key (ANTHROPIC_API_KEY)
# - 移除 IFLOW 相关逻辑，替换为 `claude -p` 命令调用
# - 支持通过 CLAUDE_CMD 变量切换命令（默认为 claude，若使用 ccr 包装器可修改为 ccr）
# - ✅ 新增：支持 Claude Code Router (ccr)，自动管理服务和配置
# - ✅ 新增：修复日志文件默认路径，避免 /tmp 权限错误
# - ✅ 修改：将无头模式从 --non-interactive 改为 -p 参数触发
###############################################################################

############################
# 0) 基本参数（可用环境变量覆盖）
############################
RUN_HOURS="${RUN_HOURS:-5}"
WORK_BRANCH="${WORK_BRANCH:-master}"
GIT_REMOTE="${GIT_REMOTE:-origin}"

# Claude Code 配置
# - CLAUDE_CMD: 默认使用官方 claude 命令。
# - 如果你使用 ccr (Claude Code Router) 等工具，可设置为 "ccr"
CLAUDE_CMD="${CLAUDE_CMD:-claude}"

############################
# 0.5) Claude Code Router 配置
############################
# 是否启用 Router 模式（0=禁用，1=启用）
# 如果 CLAUDE_CMD=ccr，则自动启用
USE_CLAUDE_CODE_ROUTER="${USE_CLAUDE_CODE_ROUTER:-0}"

# Router 监听地址
CCR_HOST="${CCR_HOST:-127.0.0.1}"
CCR_PORT="${CCR_PORT:-3456}"

# Router 配置目录
CCR_CONFIG_DIR="${HOME}/.claude-code-router"
CCR_CONFIG_FILE="${CCR_CONFIG_DIR}/config.json"

# Router 日志文件
CCR_LOG_FILE="${CCR_LOG_FILE:-${HOME}/.claude-code-router.log}"

# Router 所需的 OpenAI 兼容 API 配置
# 可以从 ANTHROPIC_API_KEY 继承，也可以单独设置
OPENAI_API_KEY="${OPENAI_API_KEY:-${ANTHROPIC_API_KEY:-}}"
OPENAI_BASE_URL="${OPENAI_BASE_URL:-https://api.deepseek.com}"
OPENAI_MODEL="${OPENAI_MODEL:-deepseek-chat}"

# GitHub 远端 URL 配置
GITHUB_REMOTE_URL="${GITHUB_REMOTE_URL:-}"

# Gitee 推送支持
GITEE_REMOTE="${GITEE_REMOTE:-gitee}"
GITEE_REMOTE_URL="${GITEE_REMOTE_URL:-}"

# 推送的远端列表（空格分隔）。默认：GitHub + Gitee
PUSH_REMOTES="${PUSH_REMOTES:-$GIT_REMOTE $GITEE_REMOTE}"

# 推送失败重试策略
PUSH_RETRY_INTERVAL="${PUSH_RETRY_INTERVAL:-60}"  # 秒
PUSH_RETRY_FOREVER="${PUSH_RETRY_FOREVER:-1}"     # 1=一直重试；0=失败就放过

GIT_USER_NAME="${GIT_USER_NAME:-claude-bot}"
GIT_USER_EMAIL="${GIT_USER_EMAIL:-claude-bot@users.noreply.github.com}"

# 是否启用"自动 bump + GitHub Release"
ENABLE_RELEASE="${ENABLE_RELEASE:-0}"   # 0/1

# timeout 结束时是否把未提交变更自动提交（WIP autosave）
AUTO_COMMIT_ON_TIMEOUT="${AUTO_COMMIT_ON_TIMEOUT:-1}"  # 0/1

############################
# 1) Claude Code 配置
############################
# Claude CLI 默认读取 ANTHROPIC_API_KEY
: "${ANTHROPIC_API_KEY:?Missing ANTHROPIC_API_KEY. Please export ANTHROPIC_API_KEY before running.}"

# 如果使用 Router，ANTHROPIC_API_KEY 可以是任意值（dummy key）
# 实际的 API key 在 Router 配置中

############################
# 2) 工具函数：日志/依赖/timeout 兼容
############################
log() { printf '[%s] %s\n' "$(date '+%F %T')" "$*"; }

need_cmd() {
  command -v "$1" >/dev/null 2>&1 || { log "ERROR: missing command: $1"; exit 1; }
}

timeout_bin() {
  if command -v timeout >/dev/null 2>&1; then
    echo "timeout"
  elif command -v gtimeout >/dev/null 2>&1; then
    echo "gtimeout"   # macOS coreutils
  else
    log "ERROR: need GNU timeout (timeout/gtimeout)."
    exit 1
  fi
}

run_cmd() {
  # 让输出尽量行缓冲
  local had_errexit=0
  [[ $- == *e* ]] && had_errexit=1
  set +e

  local status=0
  if command -v stdbuf >/dev/null 2>&1; then
    stdbuf -oL -eL "$@"
    status=$?
  else
    "$@"
    status=$?
  fi

  ((had_errexit)) && set -e
  return "$status"
}

############################
# 2.5) 进程清理
############################
ps_children_of() {
  local ppid="$1"
  local out=""
  out="$(ps -o pid= --ppid "$ppid" 2>/dev/null || true)"
  if [[ -z "${out//[[:space:]]/}" ]]; then
    out="$(ps -axo pid=,ppid= 2>/dev/null | awk -v P="$ppid" '$2==P{print $1}' || true)"
  fi
  echo "$out" | awk '{print $1}' | sed '/^$/d' || true
}

kill_descendants() {
  local parent="$1"
  local kids
  kids="$(ps_children_of "$parent" || true)"
  if [[ -n "${kids:-}" ]]; then
    local k
    while IFS= read -r k; do
      [[ -n "${k:-}" ]] || continue
      kill_descendants "$k" || true
      kill "$k" 2>/dev/null || true
    done <<< "$kids"
  fi
}

try_kill_process_group_if_safe() {
  local pid pgid
  pid="$$"
  pgid="$(ps -o pgid= -p "$pid" 2>/dev/null | tr -d ' ' || true)"
  if [[ -z "${pgid:-}" ]]; then
    pgid="$(ps -o pgid= "$pid" 2>/dev/null | tr -d ' ' || true)"
  fi
  if [[ -n "${pgid:-}" && "$pgid" =~ ^[0-9]+$ && "$pgid" == "$pid" ]]; then
    kill -- "-$pgid" 2>/dev/null || true
  fi
}

############################
# 2.6) Claude Code Router 管理函数
############################
ensure_claude_code_router_config() {
  [[ "$USE_CLAUDE_CODE_ROUTER" == "1" ]] || return 0

  # 确保 API key 已设置
  if [[ -z "${OPENAI_API_KEY:-}" ]]; then
    log "ERROR: OPENAI_API_KEY (or ANTHROPIC_API_KEY) is required for Claude Code Router"
    log "Please set OPENAI_API_KEY or ANTHROPIC_API_KEY environment variable"
    exit 1
  fi

  # 创建配置目录
  if [[ ! -d "$CCR_CONFIG_DIR" ]]; then
    log "Creating Claude Code Router config directory: $CCR_CONFIG_DIR"
    mkdir -p "$CCR_CONFIG_DIR"
  fi

  # 如果配置文件不存在或强制重新生成，创建配置
  if [[ ! -f "$CCR_CONFIG_FILE" ]] || [[ "${CCR_FORCE_RECONFIG:-0}" == "1" ]]; then
    log "Creating Claude Code Router config: $CCR_CONFIG_FILE"

    # 解析提供商名称和模型
    local provider_name="default"
    local model_name="${OPENAI_MODEL}"

    # 检查是否为 openrouter
    if [[ "$OPENAI_BASE_URL" == *"openrouter"* ]]; then
      provider_name="openrouter"
      # 从 URL 中提取模型
      if [[ "$OPENAI_MODEL" == */* ]]; then
        model_name="${OPENAI_MODEL}"
      else
        model_name="anthropic/${OPENAI_MODEL}"
      fi
    fi

    # 处理 transformer 配置
    local transformer="[\"${provider_name}\"]"
    if [[ "$provider_name" == "openrouter" ]]; then
      transformer="[\"openrouter\"]"
    fi

    # 创建配置
    cat > "$CCR_CONFIG_FILE" <<EOF
{
  "Providers": [
    {
      "name": "${provider_name}",
      "api_base_url": "${OPENAI_BASE_URL}/chat/completions",
      "api_key": "${OPENAI_API_KEY}",
      "models": ["${model_name}"],
      "transformer": {
        "use": ${transformer}
      }
    }
  ],
  "Router": {
    "default": "${provider_name},${model_name}"
  },
  "LOG": true,
  "HOST": "${CCR_HOST}",
  "PORT": ${CCR_PORT}
}
EOF

    log "Config created with provider: ${provider_name}, model: ${model_name}"
  else
    log "Using existing Claude Code Router config: $CCR_CONFIG_FILE"
  fi
}

start_claude_code_router() {
  [[ "$USE_CLAUDE_CODE_ROUTER" == "1" ]] || return 0

  # 检查服务是否已在运行
  if curl -s "http://${CCR_HOST}:${CCR_PORT}/health" >/dev/null 2>&1; then
    log "Claude Code Router is already running on ${CCR_HOST}:${CCR_PORT}"
    return 0
  fi

  # 检查端口是否被其他进程占用
  if command -v lsof >/dev/null 2>&1 && lsof -i :${CCR_PORT} >/dev/null 2>&1; then
    log "WARN: Port ${CCR_PORT} is in use by another process"
    return 1
  fi

  log "Starting Claude Code Router on ${CCR_HOST}:${CCR_PORT}..."

  # 确保配置存在
  ensure_claude_code_router_config

  # 启动服务
  nohup ccr start >> "$CCR_LOG_FILE" 2>&1 &
  local router_pid=$!

  # 等待服务启动
  local wait_time=0
  local max_wait=30

  while [[ $wait_time -lt $max_wait ]]; do
    if curl -s "http://${CCR_HOST}:${CCR_PORT}/health" >/dev/null 2>&1; then
      log "Claude Code Router started successfully (PID: $router_pid)"
      return 0
    fi

    # 检查进程是否还在运行
    if ! kill -0 $router_pid 2>/dev/null; then
      log "ERROR: Claude Code Router process died unexpectedly"
      log "Last 50 lines of router log:"
      tail -n 50 "$CCR_LOG_FILE" 2>/dev/null || log "No log file found"
      return 1
    fi

    sleep 1
    wait_time=$((wait_time + 1))
  done

  log "ERROR: Claude Code Router failed to start after ${max_wait} seconds"
  kill $router_pid 2>/dev/null || true
  log "Last 50 lines of router log:"
  tail -n 50 "$CCR_LOG_FILE" 2>/dev/null || log "No log file found"
  return 1
}

stop_claude_code_router() {
  local pid="${1:-}"

  log "Stopping Claude Code Router..."

  # 尝试使用 ccr stop 命令
  ccr stop >/dev/null 2>&1 || true

  # 如果提供了 PID，尝试杀死进程
  if [[ -n "$pid" ]]; then
    if kill -0 "$pid" 2>/dev/null; then
      kill "$pid" 2>/dev/null || true
      sleep 1

      # 强制杀死
      if kill -0 "$pid" 2>/dev/null; then
        kill -9 "$pid" 2>/dev/null || true
      fi
    fi
  fi

  # 清理可能残留的进程
  pkill -f "claude-code-router" 2>/dev/null || true

  log "Claude Code Router stopped"
}

############################
# 3) 依赖准备：git / node / claude / moon
############################
ensure_git() {
  need_cmd git
  git rev-parse --is-inside-work-tree >/dev/null 2>&1 || { log "ERROR: not a git repo."; exit 1; }
  git config user.name  "$GIT_USER_NAME"
  git config user.email "$GIT_USER_EMAIL"
}

ensure_claude() {
  # 如果使用 ccr，确保 router 已安装并配置
  if [[ "$CLAUDE_CMD" == "ccr" ]]; then
    # 检查 ccr 命令
    if ! command -v ccr >/dev/null 2>&1; then
      log "Installing Claude Code Router..."
      npm i -g @musistudio/claude-code-router@latest
    fi

    # 启用 router 模式
    USE_CLAUDE_CODE_ROUTER=1

    # 确保 API key 可用
    if [[ -z "${OPENAI_API_KEY:-}" ]]; then
      if [[ -n "${ANTHROPIC_API_KEY:-}" ]]; then
        OPENAI_API_KEY="${ANTHROPIC_API_KEY}"
        log "Using ANTHROPIC_API_KEY as OPENAI_API_KEY for router"
      else
        log "ERROR: OPENAI_API_KEY is required for Claude Code Router"
        exit 1
      fi
    fi

    # 生成配置并启动服务
    ensure_claude_code_router_config
    start_claude_code_router || exit 1

    return 0
  fi

  # 默认安装官方 claude CLI
  need_cmd npm
  if ! command -v claude >/dev/null 2>&1; then
    log "Installing Claude Code CLI..."
    npm i -g @anthropic-ai/claude-code@latest
  fi
  claude --version >/dev/null 2>&1 || true
}

ensure_moon() {
  if command -v moon >/dev/null 2>&1; then
    moon version || true
    return 0
  fi

  need_cmd curl
  log "Installing MoonBit toolchain..."
  curl -fsSL https://cli.moonbitlang.com/install/unix.sh | bash
  export PATH="$HOME/.moon/bin:$PATH"
  need_cmd moon
  moon version
}

############################
# 4) git 分支就位 & 同步 (直接推送模式)
############################
ensure_github_remote() {
  local url="${GITHUB_REMOTE_URL:-}"
  if [[ -z "${url:-}" ]]; then
    remote_exists "$GIT_REMOTE" || { log "ERROR: $GIT_REMOTE remote missing and GITHUB_REMOTE_URL not set."; return 1; }
    return 0
  fi
  if remote_exists "$GIT_REMOTE"; then
    local current_url
    current_url="$(git remote get-url "$GIT_REMOTE" 2>/dev/null || true)"
    if [[ "$current_url" != "$url" ]]; then
      log "Updating GitHub remote URL: ${GIT_REMOTE} -> ${url}"
      git remote set-url "$GIT_REMOTE" "$url" || { log "WARN: failed to update GitHub remote."; return 1; }
    fi
  else
    log "Adding GitHub remote: ${GIT_REMOTE} -> ${url}"
    git remote add "$GIT_REMOTE" "$url" || { log "WARN: failed to add GitHub remote."; return 1; }
  fi
}

ensure_gitee_remote() {
  local url="${GITEE_REMOTE_URL:-}"
  if [[ -z "${url:-}" ]]; then
    if remote_exists "$GITEE_REMOTE"; then
      return 0
    fi
    url="$(infer_gitee_url_from_github || true)"
  fi
  if [[ -z "${url:-}" ]]; then
    log "WARN: ${GITEE_REMOTE} remote missing and cannot infer url. Skip Gitee push."
    return 1
  fi
  if remote_exists "$GITEE_REMOTE"; then
    local current_url
    current_url="$(git remote get-url "$GITEE_REMOTE" 2>/dev/null || true)"
    if [[ "$current_url" != "$url" ]]; then
      log "Updating Gitee remote URL: ${GITEE_REMOTE} -> ${url}"
      git remote set-url "$GITEE_REMOTE" "$url" || { log "WARN: failed to update Gitee remote."; return 1; }
    fi
  else
    log "Adding Gitee remote: ${GITEE_REMOTE} -> ${url}"
    git remote add "$GITEE_REMOTE" "$url" || { log "WARN: failed to add Gitee remote."; return 1; }
  fi
}

ensure_branch() {
  log "Ensuring branch: $WORK_BRANCH (Direct Push Mode)"
  git fetch "$GIT_REMOTE" --prune >/dev/null 2>&1 || true

  if git show-ref --verify --quiet "refs/remotes/${GIT_REMOTE}/${WORK_BRANCH}"; then
    if git show-ref --verify --quiet "refs/heads/${WORK_BRANCH}"; then
      git checkout "$WORK_BRANCH" || { log "WARN: git checkout ${WORK_BRANCH} failed."; return 1; }

      # 在直接推送模式下，如果远程有更新，我们需要将本地修改 rebase 到远程之上
      # 这样可以确保我们的提交是线性的，并且基于最新的代码
      log "Syncing with ${GIT_REMOTE}/${WORK_BRANCH}..."
      if ! git pull --rebase "${GIT_REMOTE}" "${WORK_BRANCH}" 2>/dev/null; then
         log "WARN: Rebase failed. Trying merge..."
         if ! git merge "${GIT_REMOTE}/${WORK_BRANCH}" 2>/dev/null; then
            log "ERROR: Cannot fast-forward or merge ${WORK_BRANCH}. Manual intervention needed."
            # 尝试放弃本地更改以恢复自动运行（可选，视需求而定，这里选择保守策略：中断）
            # git reset --hard "${GIT_REMOTE}/${WORK_BRANCH}" || true
            return 1
         fi
      fi
    else
      git checkout -b "$WORK_BRANCH" "${GIT_REMOTE}/${WORK_BRANCH}" || {
        log "WARN: git checkout -b ${WORK_BRANCH} from ${GIT_REMOTE}/${WORK_BRANCH} failed."
        return 1
      }
    fi
    git branch --set-upstream-to="${GIT_REMOTE}/${WORK_BRANCH}" "$WORK_BRANCH" >/dev/null 2>&1 || true
  else
    if git show-ref --verify --quiet "refs/heads/${WORK_BRANCH}"; then
      git checkout "$WORK_BRANCH" || { log "WARN: git checkout ${WORK_BRANCH} failed."; return 1; }
    else
      git checkout -b "$WORK_BRANCH" || { log "WARN: git checkout -b ${WORK_BRANCH} failed."; return 1; }
    fi
  fi
}

push_if_ahead() {
  local remote="${1:-$GIT_REMOTE}"
  git fetch "$remote" --prune >/dev/null 2>&1 || true
  if ! git show-ref --verify --quiet "refs/remotes/${remote}/${WORK_BRANCH}"; then
    log "Remote branch ${remote}/${WORK_BRANCH} missing; pushing HEAD:${WORK_BRANCH}..."
    git push "$remote" "HEAD:${WORK_BRANCH}" || return 1
    return 0
  fi
  local ahead
  ahead="$(git rev-list --count "${remote}/${WORK_BRANCH}..HEAD" 2>/dev/null || echo 0)"
  if [[ ! "$ahead" =~ ^[0-9]+$ ]]; then
    ahead="0"
  fi
  if [[ "$ahead" -gt 0 ]]; then
    log "Pushing ${ahead} commit(s) to ${remote}/${WORK_BRANCH} (Direct Push)..."
    git push "$remote" "HEAD:${WORK_BRANCH}" || return 1
  else
    log "No commits ahead of ${remote}/${WORK_BRANCH}. Skip push."
  fi
}

remote_exists() {
  local r="$1"
  git remote get-url "$r" >/dev/null 2>&1
}

infer_gitee_url_from_github() {
  local gh_repo
  gh_repo="$(derive_github_repo 2>/dev/null || true)"
  [[ -n "${gh_repo:-}" ]] || return 1
  printf 'https://gitee.com/%s.git\n' "$gh_repo"
}

commit_worktree_if_dirty() {
  local msg="$1"
  if git diff --quiet && git diff --cached --quiet; then
    return 0
  fi
  git add -A
  if git diff --cached --quiet; then
    return 0
  fi
  git commit -m "$msg" || true
}

push_all_remotes() {
  local primary_status=0
  ensure_gitee_remote || true
  local r
  for r in $PUSH_REMOTES; do
    remote_exists "$r" || { log "WARN: remote not found: $r, skip."; continue; }
    if [[ "$r" == "$GIT_REMOTE" ]]; then
      push_if_ahead "$r" || primary_status=1
      git push "$r" --tags >/dev/null 2>&1 || true
    else
      push_if_ahead "$r" || true
      git push "$r" --tags >/dev/null 2>&1 || true
    fi
  done
  return "$primary_status"
}

push_all_remotes_with_retry() {
  local attempt=0
  while true; do
    attempt=$(( attempt + 1 ))
    if push_all_remotes; then
      log "Push ok."
      return 0
    fi
    log "WARN: push to primary remote failed (attempt=${attempt})."
    if [[ "$PUSH_RETRY_FOREVER" != "1" ]]; then
      log "WARN: PUSH_RETRY_FOREVER!=1, giving up retry."
      return 1
    fi
    sleep "$PUSH_RETRY_INTERVAL"
  done
}

############################
# 5) Release 相关工具
############################
extract_moon_version() {
  local f="./moon.mod.json"
  if [[ ! -f "$f" ]]; then
    f="$(find . -name 'moon.mod.json' -print 2>/dev/null | head -n1 || true)"
  fi
  [[ -n "${f:-}" && -f "$f" ]] || return 1
  if command -v jq >/dev/null 2>&1; then
    jq -r '.version // empty' "$f"
    return 0
  fi
  sed -nE 's/.*"version"[[:space:]]*:[[:space:]]*"([^"]+)".*/\1/p' "$f" | head -n1
}

has_error_in_log() {
  local logf="$1"
  [[ -f "$logf" ]] || return 1
  grep -Eiq '(^|[^[:alpha:]])(error:|fatal:|panic:|exception:|segmentation fault)([^[:alpha:]]|$)' "$logf"
}

derive_github_repo() {
  local url owner repo
  url="$(git config --get "remote.${GIT_REMOTE}.url" || true)"
  [[ -n "$url" ]] || return 1
  if [[ "$url" =~ github\.com[/:]+([^/]+)/([^/]+)$ ]]; then
    owner="${BASH_REMATCH[1]}"
    repo="${BASH_REMATCH[2]}"
    repo="${repo%.git}"
    echo "${owner}/${repo}"
    return 0
  fi
  return 1
}

iso_to_epoch() {
  local iso="$1"
  if command -v python3 >/dev/null 2>&1; then
    python3 - "$iso" <<'PY'
import sys, datetime, re
s = sys.argv[1].strip()
if s.endswith('Z'):
    s = s[:-1] + '+00:00'
try:
    dt = datetime.datetime.fromisoformat(s)
except ValueError:
    m = re.match(r'^(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2})(\.\d+)?(Z|[+-]\d{2}:\d{2})?$', sys.argv[1].strip())
    if not m:
        sys.exit(1)
    base = m.group(1)
    tz = m.group(3) or 'Z'
    if tz == 'Z':
        tz = '+00:00'
    dt = datetime.datetime.fromisoformat(base + tz)
print(int(dt.timestamp()))
PY
    return $?
  fi
  if date -d "$iso" +%s >/dev/null 2>&1; then
    date -d "$iso" +%s
    return 0
  fi
  if command -v gdate >/dev/null 2>&1 && gdate -d "$iso" +%s >/dev/null 2>&1; then
    gdate -d "$iso" +%s
    return 0
  fi
  if date -j -f '%Y-%m-%dT%H:%M:%SZ' "$iso" '+%s' >/dev/null 2>&1; then
    date -j -f '%Y-%m-%dT%H:%M:%SZ' "$iso" '+%s'
    return 0
  fi
  return 1
}

latest_release_age_ok() {
  command -v gh >/dev/null 2>&1 || return 1
  local repo="${GITHUB_REPOSITORY:-}"
  if [[ -z "$repo" ]]; then
    repo="$(derive_github_repo || true)"
  fi
  [[ -n "$repo" ]] || return 1
  if [[ -z "${GH_TOKEN:-}" && -z "${GITHUB_TOKEN:-}" ]]; then
    return 1
  fi
  local published_at pub_ts now_ts delta
  published_at="$(gh api "/repos/${repo}/releases/latest" --jq '.published_at' 2>/dev/null || true)"
  if [[ -z "$published_at" || "$published_at" == "null" ]]; then
    return 0
  fi
  pub_ts="$(iso_to_epoch "$published_at" 2>/dev/null || echo 0)"
  now_ts="$(date +%s)"
  [[ "$pub_ts" -gt 0 ]] || return 1
  delta=$(( now_ts - pub_ts ))
  local release_window_seconds=604800
  (( delta >= release_window_seconds )) && return 0 || return 1
}

############################
# 6) bump + release (直接推送模式)
############################
attempt_bump_and_release() {
  if [[ "$ENABLE_RELEASE" != "1" ]]; then
    log "INFO: ENABLE_RELEASE=0, skip bump+release."
    return 0
  fi
  if ! latest_release_age_ok; then
    log "INFO: release in last 7 days (or cannot check). skip release."
    return 0
  fi
  local old_ver new_ver tag repo
  old_ver="$(extract_moon_version || true)"
  log "INFO: current version: ${old_ver:-<unknown>}"

  # 使用 claude 替代 iflow，使用 -p 参数触发无头模式
  # 提示词：修改 moon.mod.json 版本号
  log "INFO: bump patch version in moon.mod.json via ${CLAUDE_CMD}..."
  run_cmd "$CLAUDE_CMD" -p "把moon.mod.json里的version增加一个patch版本(例如0.9.1变成0.9.2)，只改版本号本身" || {
    log "WARN: bump failed, skip release."
    return 0
  }

  git add -A
  new_ver="$(extract_moon_version || true)"
  log "INFO: new version: ${new_ver:-<unknown>}"

  [[ -n "$new_ver" ]] || { log "WARN: cannot parse version, skip."; return 0; }
  [[ -z "$old_ver" || "$new_ver" != "$old_ver" ]] || { log "WARN: version unchanged, skip."; return 0; }

  if git diff --cached --quiet; then
    log "WARN: no staged changes after bump, skip."
    return 0
  fi

  git commit -m "chore(release): v${new_ver}" || { log "WARN: commit failed, skip."; return 0; }
  # 直接推送到当前分支，而不是 PR
  push_if_ahead "$GIT_REMOTE" || { log "WARN: push failed, skip release creation."; return 0; }
  push_all_remotes || true

  tag="v${new_ver}"
  command -v gh >/dev/null 2>&1 || { log "WARN: gh missing, cannot create release."; return 0; }
  repo="${GITHUB_REPOSITORY:-}"
  [[ -n "$repo" ]] || repo="$(derive_github_repo || true)"
  [[ -n "$repo" ]] || { log "WARN: cannot derive repo, skip release."; return 0; }
  if gh release view "${tag}" >/dev/null 2>&1; then
    log "INFO: release ${tag} already exists, skip create."
    return 0
  fi
  log "INFO: creating GitHub Release ${tag}..."
  gh release create "${tag}" --target "$WORK_BRANCH" --generate-notes || {
    log "WARN: release create failed."
    return 0
  }
  log "INFO: released ${tag}"
}

############################
# 7) 内层循环（Claude Code）
############################
run_inner_loop_forever() {
  # 设置指向 router 的环境变量
  if [[ "$USE_CLAUDE_CODE_ROUTER" == "1" ]]; then
    export ANTHROPIC_BASE_URL="http://${CCR_HOST}:${CCR_PORT}"
    # API key 可以是任意值，因为实际 key 在 router 配置中
    export ANTHROPIC_API_KEY="claude-code-router"
    export ANTHROPIC_AUTH_TOKEN="claude-code-router"
    log "Using Claude Code Router at ${ANTHROPIC_BASE_URL}"
  fi

  terminate_inner() {
    echo
    log "terminated."
    kill_descendants "$$" || true
    try_kill_process_group_if_safe || true
    exit 0
  }
  trap terminate_inner INT TERM

  while true; do
    # 检查 MoonBit 必要配置文件
    if [[ ! -f "moon.mod.json" ]]; then
      log "MoonBit config missing. Fixing via ${CLAUDE_CMD}..."
      # 使用 claude 替代 iflow，使用 -p 参数触发无头模式
      run_cmd "$CLAUDE_CMD" -p "如果PLAN.md里的特性都实现了(如果没有没有都实现就实现这些特性，给项目命名为Feather)就解决moon test显示的所有问题（除了warning），除非测试用例本身有编译错误，否则只修改测试用例以外的代码，debug时可通过加日志和打断点，尽量不要消耗大量CPU/内存资源" || true
    fi

    log "Running: moon test"
    : > "$MOON_TEST_LOG"

    local had_errexit=0
    [[ $- == *e* ]] && had_errexit=1
    set +e

    if command -v stdbuf >/dev/null 2>&1; then
      stdbuf -oL -eL moon test 2>&1 \
        | stdbuf -oL -eL tee "$MOON_TEST_LOG"
    else
      moon test 2>&1 | tee "$MOON_TEST_LOG"
    fi

    local moon_status="${PIPESTATUS[0]:-255}"
    ((had_errexit)) && set -e

    local has_warnings=0
    if grep -Eiq '(warn(ing)?|警告)' "$MOON_TEST_LOG"; then
      has_warnings=1
    fi

    local has_error=0
    if has_error_in_log "$MOON_TEST_LOG"; then
      has_error=1
    fi

    if [[ "$moon_status" -eq 0 ]]; then
      # 测试通过：增加测试用例，使用 -p 参数触发无头模式
      run_cmd "$CLAUDE_CMD" -p "给这个项目增加一些moon test测试用例，不要超过10个" || true

      git add -A
      if git diff --cached --quiet; then
        log "INFO: nothing to commit."
      else
        git commit -m "测试通过" || true
      fi

      if [[ "$has_error" -eq 0 ]]; then
        attempt_bump_and_release || true
      else
        log "INFO: moon test exit 0 but log contains error keywords; skip release."
      fi

      if [[ "$has_warnings" -eq 1 ]]; then
        log "INFO: warnings detected."
      fi
    else
      # 测试失败：修复代码，使用 -p 参数触发无头模式
      log "Fixing via ${CLAUDE_CMD}..."
      run_cmd "$CLAUDE_CMD" -p "如果PLAN.md里的特性都实现了(如果没有没有都实现就实现这些特性，给项目命名为Feather)就解决moon test显示的所有问题（除了warning），除非测试用例本身有编译错误，否则只修改测试用例以外的代码，debug时可通过加日志和打断点，尽量不要消耗大量CPU/内存资源" || true
    fi

    log "Looping..."
    sleep 1
  done
}

############################
# 8) inner / outer main
############################
inner_main() {
  # 修复：将临时测试日志放在 $HOME 下，避免 /tmp 权限问题
  MOON_TEST_LOG="${HOME}/.typus_moon_test_last_$$.log"
  run_inner_loop_forever
}

outer_main() {
  need_cmd curl

  [[ "$RUN_HOURS" =~ ^[0-9]+$ ]] || { log "ERROR: RUN_HOURS must be an integer (got: $RUN_HOURS)"; exit 1; }

  ensure_git

  ensure_github_remote || log "WARN: Failed to ensure GitHub remote config."
  ensure_gitee_remote || log "WARN: Failed to ensure Gitee remote config."

  # 初始确保分支
  ensure_branch

  # 如果使用 ccr 命令，自动启用 router 模式
  if [[ "$CLAUDE_CMD" == "ccr" ]]; then
    USE_CLAUDE_CODE_ROUTER=1
  fi

  # 如果使用 router，确保配置并启动
  if [[ "$USE_CLAUDE_CODE_ROUTER" == "1" ]]; then
    # 确保 API key 可用
    if [[ -z "${OPENAI_API_KEY:-}" ]]; then
      if [[ -n "${ANTHROPIC_API_KEY:-}" ]]; then
        OPENAI_API_KEY="${ANTHROPIC_API_KEY}"
        log "Using ANTHROPIC_API_KEY as OPENAI_API_KEY for router"
      else
        log "ERROR: OPENAI_API_KEY (or ANTHROPIC_API_KEY) is required for Claude Code Router"
        exit 1
      fi
    fi

    # 安装和启动 router
    ensure_claude
  else
    # 原始逻辑
    ensure_claude
    # 检查 ANTHROPIC_API_KEY
    : "${ANTHROPIC_API_KEY:?Missing ANTHROPIC_API_KEY. Please export ANTHROPIC_API_KEY before running.}"
  fi

  ensure_moon

  log "CLAUDE_CMD=$CLAUDE_CMD"
  log "LOG_FILE=$LOG_FILE"

  if [[ "$USE_CLAUDE_CODE_ROUTER" == "1" ]]; then
    log "Claude Code Router enabled: http://${CCR_HOST}:${CCR_PORT}"
    log "Router config: $CCR_CONFIG_FILE"
    log "Router log: $CCR_LOG_FILE"
  fi

  local tbin
  tbin="$(timeout_bin)"

  local script
  script="${BASH_SOURCE[0]}"
  script="$(cd -- "$(dirname -- "$script")" && pwd)/$(basename -- "$script")"

  # 设置退出时的清理
  cleanup_on_exit() {
    log "Cleaning up..."

    # 停止 router 服务
    if [[ "$USE_CLAUDE_CODE_ROUTER" == "1" ]]; then
      stop_claude_code_router
    fi

    # 清理临时文件
    kill_descendants "$$" || true
    try_kill_process_group_if_safe || true
  }
  trap cleanup_on_exit EXIT INT TERM

  while true; do
    log "Run loop for ${RUN_HOURS} hour(s)..."

    if command -v setsid >/dev/null 2>&1; then
      "$tbin" --signal=TERM --kill-after=60s $(( RUN_HOURS * 3600 )) setsid bash "$script" __inner__ || true
    else
      "$tbin" --signal=TERM --kill-after=60s $(( RUN_HOURS * 3600 )) bash "$script" __inner__ || true
    fi

    # 每轮结束后，先同步最新代码，再提交剩余工作，再推送
    # 这样确保下一轮是从最新的代码开始
    ensure_branch || true

    if [[ "$AUTO_COMMIT_ON_TIMEOUT" == "1" ]]; then
      commit_worktree_if_dirty "chore: autosave after ${RUN_HOURS}h ($(date '+%F %T'))"
    fi

    push_all_remotes_with_retry

    # 再次确保分支是最新的，准备进入下一轮
    ensure_branch || true
  done
}

############################
# 9) 入口分发
############################
if [[ "${1:-}" == "__inner__" ]]; then
  shift
  inner_main "$@"
else
  outer_main "$@"
fi