#!/usr/bin/env bash
set -u
set -o pipefail

RELEASE_WINDOW_SECONDS=604800
STACK_LOG="/tmp/typus_stack_last.log"

WORK_BRANCH="${WORK_BRANCH:-master}"

# marker 文件放在真实的 git dir 下，确保不会被 git add/commit/push
# 兼容：.git 可能是目录，也可能是一个指向实际 gitdir 的文件（worktree/submodule）
GIT_DIR_REAL="$(git rev-parse --git-dir 2>/dev/null || echo ".git")"
RELEASE_MARKER_FILE="${RELEASE_MARKER_FILE:-${GIT_DIR_REAL%/}/typus_release_tag}"

extract_cabal_version() {
  local f ver

  # 避免从 .stack-work/.git 等目录误选到 cabal 文件
  f="$(find . -name '*.cabal' \
        -not -path './.stack-work/*' \
        -not -path './.git/*' \
        -print -quit 2>/dev/null || true)"
  [[ -n "${f:-}" ]] || return 1

  ver="$(sed -nE 's/^[[:space:]]*[Vv]ersion[[:space:]]*:[[:space:]]*([0-9]+(\.[0-9]+)*)[[:space:]]*.*$/\1/p' "$f" | head -n1 || true)"
  [[ -n "${ver:-}" ]] || return 1
  printf '%s\n' "$ver"
}

has_error_in_log() {
  local log="$1"
  [[ -f "$log" ]] || return 1
  grep -Eiq '(^|[^[:alpha:]])(error:|fatal:|panic:|exception:|segmentation fault)([^[:alpha:]]|$)' "$log"
}

latest_release_age_ok() {
  command -v gh >/dev/null 2>&1 || return 1
  [[ -n "${GITHUB_REPOSITORY:-}" ]] || return 1
  if [[ -z "${GH_TOKEN:-}" && -z "${GITHUB_TOKEN:-}" ]]; then
    return 1
  fi

  local published_at pub_ts now_ts delta
  published_at="$(gh api "/repos/${GITHUB_REPOSITORY}/releases/latest" --jq '.published_at' 2>/dev/null || true)"
  if [[ -z "${published_at:-}" || "${published_at}" == "null" ]]; then
    # 没有 release（或取不到 latest），视为允许
    return 0
  fi

  pub_ts="$(date -d "$published_at" +%s 2>/dev/null || echo 0)"
  now_ts="$(date +%s)"
  [[ "$pub_ts" -gt 0 ]] || return 1

  delta=$(( now_ts - pub_ts ))
  (( delta >= RELEASE_WINDOW_SECONDS ))
}

attempt_bump_and_tag() {
  if [[ "${GITHUB_ACTIONS:-}" != "true" ]]; then
    echo "非 GitHub Actions 环境，跳过自动发布准备。"
    return 0
  fi

  # 本轮只准备一次 release
  if [[ -f "$RELEASE_MARKER_FILE" ]]; then
    echo "已存在 release marker（$(cat "$RELEASE_MARKER_FILE" 2>/dev/null || true)），跳过。"
    return 0
  fi

  if ! latest_release_age_ok; then
    echo "最近 7 天内已有 release（或无法判断），跳过自动发布准备。"
    return 0
  fi

  # 确保本地 tags 与远端同步，避免远端已存在同名 tag 但本地没看到导致 push 失败
  git fetch --tags --force >/dev/null 2>&1 || true

  local old_ver new_ver tag
  old_ver="$(extract_cabal_version || true)"
  echo "当前版本：${old_ver:-<unknown>}"

  echo "满足发布条件：开始 bump 版本号..."
  iflow '增加版本号(例如0.9.1变成0.9.2) think:high' --yolo || {
    echo "bump 版本号失败，跳过本次发布准备。"
    return 0
  }

  git add -A

  new_ver="$(extract_cabal_version || true)"
  echo "bump 后版本：${new_ver:-<unknown>}"
  [[ -n "${new_ver:-}" ]] || { echo "无法提取版本号，跳过。"; return 0; }

  if [[ -n "${old_ver:-}" && "${new_ver}" == "${old_ver}" ]]; then
    echo "版本号未变化（${old_ver} -> ${new_ver}），跳过。"
    return 0
  fi

  if git diff --cached --quiet; then
    echo "bump 后没有 staged 变更，跳过。"
    return 0
  fi

  git commit -m "chore(release): v${new_ver}" || {
    echo "提交 bump commit 失败，跳过。"
    return 0
  }

  tag="v${new_ver}"

  # 必须用 annotated tag，这样 workflow 里的 --follow-tags 才会自动 push
  if git rev-parse -q --verify "refs/tags/${tag}" >/dev/null; then
    echo "本地 tag ${tag} 已存在，跳过打 tag。"
  else
    git tag -a "${tag}" -m "${tag}" || {
      echo "打 tag 失败，跳过。"
      return 0
    }
  fi

  mkdir -p "$(dirname -- "$RELEASE_MARKER_FILE")"
  printf '%s\n' "${tag}" > "$RELEASE_MARKER_FILE"
  echo "已准备发布：${tag}"
}

trap 'echo; echo "已终止."; exit 0' INT TERM

# iflow "确保测试用例不会消耗大量内存，尽量不要删除测试用例 think:high" --yolo || true

while true; do
  echo "===================="
  echo "$(date '+%F %T') 运行测试：stack test --flag "*:fast" --flag "*:-production" ..."
  echo "===================="

  : > "$STACK_LOG"

  # Stack 对应的测试命令
  # 注意：stack 的 flag 语法是 --flag "package:flag"，这里用通配符 "*"
  # --test-arguments 对应 cabal 的 --test-options
  GHCRTS="-M2G -A16m" stack test \
    --flag "*:fast" \
    --flag "*:-production" \
    --ghc-options="-O0 -rtsopts" \
    --test-arguments="+RTS -M1024m -A16m -RTS" \
    --jobs=1 2>&1 | tee "$STACK_LOG"
  ps=("${PIPESTATUS[@]}")
  STACK_STATUS="${ps[0]:-255}"

  HAS_ERROR=0
  if has_error_in_log "$STACK_LOG"; then
    HAS_ERROR=1
  fi

  if [[ "$STACK_STATUS" -eq 0 ]]; then
    iflow '确保所有测试用例都符合README.md的内容 think:high' --yolo || true
    iflow "给这个项目增加一些stack test测试用例，不要超过200个，在这些新测试用例里面，如果需要添加使用QuickCheck的测试用例就添加，确保所有新测试用例都符合README.md的内容 think:high" --yolo || true
    iflow '删除项目根目录多余的.md文件或者.txt文件（像TEST_ENHANCEMENT_SUMMARY.md和test_wall_production.txt这样的） think:high' --yolo || true
    iflow '确保所有测试用的文件都在专门用来存放测试文件的目录 think:high' --yolo || true

    git add -A
    if git diff --cached --quiet; then
      echo "没有文件变化可提交"
    else
      git commit -m "测试通过" || true
    fi

    if [[ "$HAS_ERROR" -eq 0 ]]; then
      attempt_bump_and_tag || true
    else
      echo "stack 退出码为 0，但日志检测到 error 关键词，跳过发布准备。"
    fi
  else
    echo "调用 iflow 修复..."

    # 尝试提取具体的报错行，格式通常为: path/file.hs:line:col: error:
    # 如果能提取到，则拆分为多次调用，每次修复一个具体的错误
    local error_logs
    error_logs=$(grep -E '^[^[:space:]]+\.hs:[0-9]+:[0-9]+: error:' "$STACK_LOG" 2>/dev/null | sort -u)

    if [[ -n "$error_logs" ]]; then
      echo "发现 ${#error_logs} 个具体编译错误，将逐个修复..."
      while IFS= read -r err; do
        if [[ -n "$err" ]]; then
          echo ">>> 正在修复问题: $err"
          # 针对单行报错构造 iflow 指令
          iflow "修复这个具体的错误：$err
上下文：项目使用 GHCRTS=\"-M2G -A16m\" stack test --flag \"*:fast\" --flag \"*:-production\" --ghc-options=\"-O0 -rtsopts\" --test-arguments=\"+RTS -M1024m -A16m -RTS\" --jobs=1 进行测试。
除非该错误位于测试文件中（如 Spec.hs），否则只修改测试用例以外的代码。如果是在测试文件中，请修复测试代码。
debug时可通过加日志和打断点。think:high" --yolo || true
        fi
      done <<< "$error_logs"
    else
      # 如果没有匹配到标准的编译错误行（例如运行时错误），则回退到全量修复
      echo "未发现标准格式的编译错误，尝试全量修复..."
      iflow '解决GHCRTS="-M2G -A16m" stack test \
        --flag "*:fast" \
        --flag "*:-production" \
        --ghc-options="-O0 -rtsopts" \
        --test-arguments="+RTS -M1024m -A16m -RTS" \
        --jobs=1显示的所有问题（除了warning），除非测试用例本身有编译错误，否则只修改测试用例以外的代码，debug时可通过加日志和打断点 think:high' --yolo || true
    fi

    iflow '删除项目根目录多余的.md文件或者.txt文件（像TEST_ENHANCEMENT_SUMMARY.md和test_wall_production.txt这样的）' --yolo || true
  fi

  echo "回到第 1 步..."
  sleep 1
done