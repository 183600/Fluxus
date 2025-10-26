#!/usr/bin/env bash

set -euo pipefail

GHC_VERSION="${GHC_VERSION:-recommended}"
CABAL_VERSION="${CABAL_VERSION:-recommended}"
STACK_VERSION="${STACK_VERSION:-latest}"
INSTALL_HLS=0
SKIP_CABAL_UPDATE=0

usage() {
  cat <<'EOF'
用法: ./setup_haskell_env.sh [选项]

选项:
  --install-hls          同时安装推荐版本的 Haskell Language Server
  --skip-cabal-update    跳过执行 cabal update
  -h, --help             显示本帮助信息

可通过环境变量自定义安装版本:
  GHC_VERSION=9.4.8 CABAL_VERSION=3.10.2.0 STACK_VERSION=latest ./setup_haskell_env.sh
EOF
}

info() {
  printf '[INFO] %s\n' "$*"
}

warn() {
  printf '[WARN] %s\n' "$*" >&2
}

error_exit() {
  printf '[ERROR] %s\n' "$*" >&2
  exit 1
}

require_command() {
  local cmd="$1"
  if ! command -v "$cmd" > /dev/null 2>&1; then
    error_exit "缺少命令: ${cmd}，请先安装后再运行此脚本。"
  fi
}

ADDED_GHCUP_TO_PATH=0
ADDED_LOCAL_BIN_TO_PATH=0

ensure_path() {
  local dir="$1"
  if [[ ! -d "$dir" ]]; then
    return
  fi
  if [[ ":$PATH:" != *":$dir:"* ]]; then
    export PATH="$dir:$PATH"
    if [[ "$dir" == "$HOME/.ghcup/bin" ]]; then
      ADDED_GHCUP_TO_PATH=1
    elif [[ "$dir" == "$HOME/.local/bin" ]]; then
      ADDED_LOCAL_BIN_TO_PATH=1
    fi
  fi
}

install_ghcup() {
  if command -v ghcup > /dev/null 2>&1; then
    info "检测到 ghcup: $(ghcup --numeric-version 2>/dev/null || ghcup --version 2>/dev/null)"
    return
  fi

  info "正在安装 ghcup (Haskell 官方工具链安装器)..."
  require_command curl
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
    | env BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
          BOOTSTRAP_HASKELL_MINIMAL=1 \
          BOOTSTRAP_HASKELL_INSTALL_STACK=0 \
          BOOTSTRAP_HASKELL_INSTALL_CABAL=0 \
          BOOTSTRAP_HASKELL_INSTALL_HLS=0 \
          BOOTSTRAP_HASKELL_INSTALL_GHC=0 \
          sh

  ensure_path "$HOME/.ghcup/bin"

  if ! command -v ghcup > /dev/null 2>&1; then
    error_exit "ghcup 安装失败，请检查网络连接或稍后重试。"
  fi
}

install_component() {
  local component="$1"
  local version="$2"
  local set_default="${3:-0}"

  info "安装/更新 ${component} (${version})..."
  ghcup install "$component" "$version"

  if [[ "$set_default" == "1" ]]; then
    info "设置 ${component} 默认版本 -> ${version}"
    if ghcup set "$component" "$version"; then
      :
    else
      warn "无法设置 ${component} 默认版本，请手动执行: ghcup set ${component} ${version}"
    fi
  fi
}

verify_tool() {
  local tool="$1"
  local version_cmd=${2:-"--version"}
  if command -v "$tool" > /dev/null 2>&1; then
    local version_output
    version_output="$($tool $version_cmd 2>/dev/null | head -n 1)"
    if [[ -n "$version_output" ]]; then
      info "$tool: $version_output"
    else
      info "$tool 已安装。"
    fi
  else
    error_exit "${tool} 未出现在 PATH 中，请检查上方输出。"
  fi
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --install-hls)
      INSTALL_HLS=1
      shift
      ;;
    --skip-cabal-update)
      SKIP_CABAL_UPDATE=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      error_exit "未知参数: $1"
      ;;
  esac
done

case "$(uname -s)" in
  Linux|Darwin)
    ;;
  *)
    warn "当前系统为 $(uname -s)，脚本仅在 Linux/macOS 上测试过。"
    ;;
esac

ensure_path "$HOME/.ghcup/bin"
ensure_path "$HOME/.local/bin"

install_ghcup
ensure_path "$HOME/.ghcup/bin"

install_component ghc "$GHC_VERSION" 1
install_component cabal "$CABAL_VERSION" 1
install_component stack "$STACK_VERSION" 0

if [[ "$INSTALL_HLS" == "1" ]]; then
  install_component hls recommended 1
fi

# 确保 stack 在 PATH 中 (部分旧版本 ghcup 不会自动创建软链接)
if ! command -v stack > /dev/null 2>&1; then
  ensure_path "$HOME/.local/bin"
  mkdir -p "$HOME/.local/bin"

  stack_path="$(ghcup whereis stack "$STACK_VERSION" 2>/dev/null | head -n 1 || true)"
  if [[ -z "$stack_path" ]]; then
    stack_path="$(ghcup whereis stack recommended 2>/dev/null | head -n 1 || true)"
  fi

  if [[ -z "$stack_path" ]]; then
    stack_path="$(find "$HOME/.ghcup/bin" -maxdepth 1 -type f -name 'stack*' -perm -u+x 2>/dev/null | head -n 1 || true)"
  fi

  if [[ -n "$stack_path" && -x "$stack_path" ]]; then
    ln -sf "$stack_path" "$HOME/.local/bin/stack"
    ensure_path "$HOME/.local/bin"
  fi
fi

verify_tool ghc "--version"
verify_tool cabal "--version"
verify_tool stack "--version"

if [[ "$INSTALL_HLS" == "1" ]]; then
  verify_tool hls "--version"
fi

if [[ "$SKIP_CABAL_UPDATE" != "1" ]]; then
  info "更新 Cabal 包索引..."
  cabal update
fi

cat <<'EOF'

🎉 Haskell 开发环境已准备就绪！

接下来建议：
  • 运行 `stack setup` 或 `cabal update`（如果已跳过更新）以确保本地缓存最新
  • 将 Fluxus 仓库添加到 stack/cabal 项目中进行开发
EOF

if [[ "$ADDED_GHCUP_TO_PATH" -eq 1 ]]; then
  cat <<'EOF'
[提示] 已为当前会话临时将 ~/.ghcup/bin 添加到 PATH。
       如需永久生效，请在 shell 配置文件中加入：
         export PATH="$HOME/.ghcup/bin:$PATH"
EOF
fi

if [[ "$ADDED_LOCAL_BIN_TO_PATH" -eq 1 ]]; then
  cat <<'EOF'
[提示] 已为当前会话临时将 ~/.local/bin 添加到 PATH。
       如需永久生效，请在 shell 配置文件中加入：
         export PATH="$HOME/.local/bin:$PATH"
EOF
fi
