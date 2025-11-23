#!/usr/bin/env bash
set -euo pipefail

REQUIRED_GHC_VERSION="${REQUIRED_GHC_VERSION:-9.2.8}"
REQUIRED_CABAL_VERSION="${REQUIRED_CABAL_VERSION:-3.10.1.0}"

GHCUP_HOME="${GHCUP_HOME:-$HOME/.ghcup}"
GHCUP_BIN="${GHCUP_HOME}/bin"
CABAL_BIN_DIR="${CABAL_BIN_DIR:-$HOME/.cabal/bin}"

export PATH="${GHCUP_BIN}:${CABAL_BIN_DIR}:$PATH"

TMP_WORK_DIR=""
cleanup() {
  if [ -n "$TMP_WORK_DIR" ] && [ -d "$TMP_WORK_DIR" ]; then
    rm -rf "$TMP_WORK_DIR"
    TMP_WORK_DIR=""
  fi
}
trap cleanup EXIT

log() {
  printf '[ensure-haskell] %s\n' "$*"
}

version_ge() {
  # Returns 0 if $1 >= $2
  [ "$(printf '%s\n%s\n' "$2" "$1" | sort -V | head -n1)" = "$2" ]
}

ensure_ghcup_cli() {
  if command -v ghcup >/dev/null 2>&1; then
    log "ghcup 已安装: $(command -v ghcup)"
    return
  fi

  log "未检测到 ghcup，开始下载官方安装脚本..."
  if ! command -v curl >/dev/null 2>&1; then
    log "需要 curl 才能下载 ghcup 安装脚本" >&2
    exit 1
  fi

  TMP_WORK_DIR=$(mktemp -d)
  local installer="$TMP_WORK_DIR/ghcup-install.sh"

  if ! curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org -o "$installer"; then
    log "下载 ghcup 安装脚本失败" >&2
    exit 1
  fi

  chmod +x "$installer"
  log "安装 ghcup (非交互模式)..."
  BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    BOOTSTRAP_HASKELL_ADJUST_PATH=0 \
    BOOTSTRAP_HASKELL_GHC_VERSION="$REQUIRED_GHC_VERSION" \
    BOOTSTRAP_HASKELL_CABAL_VERSION="$REQUIRED_CABAL_VERSION" \
    BOOTSTRAP_HASKELL_INSTALL_STACK=0 \
    "$installer"

  cleanup
}

ensure_ghc() {
  local need_install=1
  if command -v ghc >/dev/null 2>&1; then
    local current
    current=$(ghc --numeric-version)
    if version_ge "$current" "$REQUIRED_GHC_VERSION"; then
      need_install=0
      log "检测到 ghc ${current} (满足 ${REQUIRED_GHC_VERSION}+ 的要求)"
    else
      log "检测到 ghc ${current}，将安装 ${REQUIRED_GHC_VERSION}"
    fi
  else
    log "未检测到 ghc，将安装 ${REQUIRED_GHC_VERSION}"
  fi

  if [ "$need_install" -eq 1 ]; then
    log "使用 ghcup 安装 ghc ${REQUIRED_GHC_VERSION}..."
    ghcup install ghc "$REQUIRED_GHC_VERSION" --set
  fi
}

get_cabal_version() {
  cabal --numeric-version 2>/dev/null || cabal --version | head -n1 | awk '{print $3}'
}

ensure_cabal() {
  local need_install=1
  if command -v cabal >/dev/null 2>&1; then
    local current
    current=$(get_cabal_version)
    if version_ge "$current" "$REQUIRED_CABAL_VERSION"; then
      need_install=0
      log "检测到 cabal ${current} (满足 ${REQUIRED_CABAL_VERSION}+ 的要求)"
    else
      log "检测到 cabal ${current}，将安装 ${REQUIRED_CABAL_VERSION}"
    fi
  else
    log "未检测到 cabal，将安装 ${REQUIRED_CABAL_VERSION}"
  fi

  if [ "$need_install" -eq 1 ]; then
    log "使用 ghcup 安装 cabal ${REQUIRED_CABAL_VERSION}..."
    ghcup install cabal "$REQUIRED_CABAL_VERSION" --set
  fi
}

log "正在准备 Haskell 工具链 (GHC ${REQUIRED_GHC_VERSION}, Cabal ${REQUIRED_CABAL_VERSION})..."
ensure_ghcup_cli
ensure_ghc
ensure_cabal

log "Haskell 工具链就绪:"
log "  ghc $(ghc --numeric-version)"
log "  cabal $(get_cabal_version)"
log "如需持久生效，请在 shell 配置文件中添加:"
log "  export PATH=\"$HOME/.ghcup/bin:$HOME/.cabal/bin:\$PATH\""
