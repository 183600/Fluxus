#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH"

ensure_haskell_toolchain() {
  if command -v cabal >/dev/null 2>&1 && command -v ghc >/dev/null 2>&1; then
    return
  fi

  if [ -x "$SCRIPT_DIR/ensure_haskell_toolchain.sh" ]; then
    echo "未检测到完整的 Haskell 工具链，正在运行 ensure_haskell_toolchain.sh ..." >&2
    bash "$SCRIPT_DIR/ensure_haskell_toolchain.sh"
  else
    echo "未检测到 cabal/ghc，且无法在 $SCRIPT_DIR 找到 ensure_haskell_toolchain.sh。请先安装 Haskell 工具链。" >&2
    exit 1
  fi
}

ensure_haskell_toolchain

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

PYTHON_CMD=${PYTHON_CMD:-python3}
GO_CMD=${GO_CMD:-go}
TIMEOUT_SECONDS=${FLUXUS_EQ_TIMEOUT:-12}
KEEP_ARTIFACTS=${KEEP_FLUXUS_EQ_ARTIFACTS:-0}
PY_OUTPUT_NAME=${FLUXUS_EQ_PY_OUTPUT:-fibonacci}
GO_OUTPUT_NAME=${FLUXUS_EQ_GO_OUTPUT:-fibonacci_go}

TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
SKIPPED_TESTS=0

declare -a FAILED_TEST_DETAILS=()
declare -a GENERATED_ARTIFACTS=()

usage() {
  cat <<'EOF'
用法: ./quick_equivalence_check.sh [files...]

  # 不带参数时，验证默认 Fibonacci 示例
  ./quick_equivalence_check.sh

  # 验证任意 Python 文件，命令完全镜像:
  ./quick_equivalence_check.sh path/to/xxx.py
  # 会执行: python3 xxx.py
  # 并比较: fluxus --python -O2 xxx.py -o fibonacci

  # 同时验证 Python 与 Go 文件
  ./quick_equivalence_check.sh foo.py bar.go

该脚本用于快速评估“运行 python/go 文件”和“运行 fluxus --python/--go -O2 file -o fibonacci(_go)”之间的输出一致性。
EOF
}

if command -v timeout >/dev/null 2>&1; then
  TIMEOUT_CMD=(timeout "${TIMEOUT_SECONDS}s")
else
  TIMEOUT_CMD=()
fi

run_and_capture() {
  local __result_var=$1
  shift
  set +e
  local __output
  __output=$("$@" 2>&1)
  local __status=$?
  set -e
  printf -v "${__result_var}" '%s' "$__output"
  return $__status
}

resolve_fluxus_binary() {
  if [ -n "${FLUXUS_BIN:-}" ]; then
    echo "$FLUXUS_BIN"
    return 0
  fi

  if command -v fluxus >/dev/null 2>&1; then
    command -v fluxus
    return 0
  fi

  if command -v cabal >/dev/null 2>&1; then
    local list_output candidate
    list_output=$(cabal list-bin fluxus 2>/dev/null || true)
    candidate=$(printf "%s" "$list_output" | tail -n 1)
    if [ -n "$candidate" ] && [ -x "$candidate" ]; then
      echo "$candidate"
      return 0
    fi
  fi

  if [ -x "./bin/fluxus" ]; then
    echo "./bin/fluxus"
    return 0
  fi

  echo ""
}

cleanup() {
  if [ "$KEEP_ARTIFACTS" = "1" ]; then
    echo -e "${YELLOW}保留生成的 fibonacci 可执行文件以便手动检查${NC}"
    return
  fi

  for artifact in "${GENERATED_ARTIFACTS[@]}"; do
    rm -f "$artifact"
  done
}
trap cleanup EXIT

compare_outputs() {
  local baseline="$1"
  local fluxus_run="$2"
  local file_label="$3"

  if [ "$baseline" = "$fluxus_run" ]; then
    echo -e "${GREEN}✓ 输出与 python ${file_label} 完全一致${NC}"
    ((PASSED_TESTS++))
  else
    echo -e "${RED}✗ 输出不一致${NC}"
    echo "python ${file_label} 输出:"
    printf '%s\n' "$baseline"
    echo "fluxus 生成的 ${file_label} (${PY_OUTPUT_NAME}) 输出:"
    printf '%s\n' "$fluxus_run"
    ((FAILED_TESTS++))
    FAILED_TEST_DETAILS+=("${file_label}: 输出不匹配")
  fi
}

test_python_file() {
  local py_file=$1
  ((TOTAL_TESTS++))
  echo "----------------------------------------"
  echo "测试 #${TOTAL_TESTS} (Python): ${py_file}"

  if [ ! -f "$py_file" ]; then
    echo -e "${RED}✗ 找不到 Python 文件${NC}"
    FAILED_TEST_DETAILS+=("${py_file}: 文件不存在")
    ((FAILED_TESTS++))
    return
  fi

  local python_output fluxus_output compile_log
  echo "运行基线: ${PYTHON_CMD} ${py_file}"
  if ! run_and_capture python_output "${TIMEOUT_CMD[@]}" "$PYTHON_CMD" "$py_file"; then
    echo -e "${RED}✗ python ${py_file} 执行失败${NC}"
    printf '%s\n' "$python_output"
    FAILED_TEST_DETAILS+=("${py_file}: python 执行失败")
    ((FAILED_TESTS++))
    return
  fi

  local compile_cmd=("$FLUXUS_BIN_PATH" "--python" "-O2" "$py_file" "-o" "$PY_OUTPUT_NAME")
  echo "运行 Fluxus: ${compile_cmd[*]}"
  if ! run_and_capture compile_log "${compile_cmd[@]}"; then
    echo -e "${RED}✗ fluxus 编译失败${NC}"
    printf '%s\n' "$compile_log"
    FAILED_TEST_DETAILS+=("${py_file}: fluxus 编译失败")
    ((FAILED_TESTS++))
    return
  fi
  GENERATED_ARTIFACTS+=("$PY_OUTPUT_NAME")

  echo "运行二进制: ./${PY_OUTPUT_NAME}"
  if ! run_and_capture fluxus_output "${TIMEOUT_CMD[@]}" "./${PY_OUTPUT_NAME}"; then
    echo -e "${RED}✗ 运行 ./${PY_OUTPUT_NAME} 失败${NC}"
    printf '%s\n' "$fluxus_output"
    FAILED_TEST_DETAILS+=("${py_file}: fluxus 二进制运行失败")
    ((FAILED_TESTS++))
    return
  fi

  compare_outputs "$python_output" "$fluxus_output" "$py_file"

  if [ "$KEEP_ARTIFACTS" != "1" ]; then
    rm -f "$PY_OUTPUT_NAME"
  fi
}

test_go_file() {
  local go_file=$1
  ((TOTAL_TESTS++))
  echo "----------------------------------------"
  echo "测试 #${TOTAL_TESTS} (Go): ${go_file}"

  if [ ! -f "$go_file" ]; then
    echo -e "${RED}✗ 找不到 Go 文件${NC}"
    FAILED_TEST_DETAILS+=("${go_file}: 文件不存在")
    ((FAILED_TESTS++))
    return
  fi

  local go_output fluxus_output compile_log
  echo "运行基线: ${GO_CMD} run ${go_file}"
  if ! run_and_capture go_output "${TIMEOUT_CMD[@]}" "$GO_CMD" run "$go_file"; then
    echo -e "${RED}✗ go run ${go_file} 执行失败${NC}"
    printf '%s\n' "$go_output"
    FAILED_TEST_DETAILS+=("${go_file}: go run 执行失败")
    ((FAILED_TESTS++))
    return
  fi

  local compile_cmd=("$FLUXUS_BIN_PATH" "--go" "-O2" "$go_file" "-o" "$GO_OUTPUT_NAME")
  echo "运行 Fluxus: ${compile_cmd[*]}"
  if ! run_and_capture compile_log "${compile_cmd[@]}"; then
    echo -e "${RED}✗ fluxus Go 编译失败${NC}"
    printf '%s\n' "$compile_log"
    FAILED_TEST_DETAILS+=("${go_file}: fluxus 编译失败")
    ((FAILED_TESTS++))
    return
  fi
  GENERATED_ARTIFACTS+=("$GO_OUTPUT_NAME")

  echo "运行二进制: ./${GO_OUTPUT_NAME}"
  if ! run_and_capture fluxus_output "${TIMEOUT_CMD[@]}" "./${GO_OUTPUT_NAME}"; then
    echo -e "${RED}✗ 运行 ./${GO_OUTPUT_NAME} 失败${NC}"
    printf '%s\n' "$fluxus_output"
    FAILED_TEST_DETAILS+=("${go_file}: fluxus 二进制运行失败")
    ((FAILED_TESTS++))
    return
  fi

  if [ "$go_output" = "$fluxus_output" ]; then
    echo -e "${GREEN}✓ 输出与 go run ${go_file} 完全一致${NC}"
    ((PASSED_TESTS++))
  else
    echo -e "${RED}✗ 输出不一致${NC}"
    echo "go run 输出:"
    printf '%s\n' "$go_output"
    echo "fluxus 生成的 ${GO_OUTPUT_NAME} 输出:"
    printf '%s\n' "$fluxus_output"
    FAILED_TEST_DETAILS+=("${go_file}: 输出不匹配")
    ((FAILED_TESTS++))
  fi

  if [ "$KEEP_ARTIFACTS" != "1" ]; then
    rm -f "$GO_OUTPUT_NAME"
  fi
}

PYTHON_TARGETS=()
GO_TARGETS=()

if [ $# -eq 0 ]; then
  PYTHON_TARGETS+=("examples/python/fibonacci.py")
  GO_TARGETS+=("examples/go/fibonacci.go")
else
  for arg in "$@"; do
    case "$arg" in
      -h|--help)
        usage
        exit 0
        ;;
      *.py)
        PYTHON_TARGETS+=("$arg")
        ;;
      *.go)
        GO_TARGETS+=("$arg")
        ;;
      *)
        echo -e "${RED}不支持的文件类型: ${arg}${NC}"
        usage
        exit 1
        ;;
    esac
  done
fi

if [ ${#PYTHON_TARGETS[@]} -eq 0 ] && [ ${#GO_TARGETS[@]} -eq 0 ]; then
  echo -e "${RED}未提供 .py 或 .go 文件用于验证${NC}"
  exit 1
fi

echo "=========================================="
echo "Fluxus Python/Go 输出一致性快速检测"
echo "=========================================="

echo "构建 Fluxus (cabal build)..."
BUILD_LOG=$(mktemp)
if ! cabal build >"$BUILD_LOG" 2>&1; then
  echo -e "${RED}构建失败！${NC}"
  tail -n 20 "$BUILD_LOG"
  rm -f "$BUILD_LOG"
  exit 1
fi
tail -n 5 "$BUILD_LOG"
rm -f "$BUILD_LOG"

echo
FLUXUS_BIN_PATH=$(resolve_fluxus_binary)
if [ -z "$FLUXUS_BIN_PATH" ]; then
  echo -e "${RED}无法找到 fluxus 可执行文件。请确保已成功构建。${NC}"
  exit 1
fi

echo "使用的 Fluxus 命令路径: ${FLUXUS_BIN_PATH}"

echo
if [ ${#PYTHON_TARGETS[@]} -gt 0 ]; then
  echo "=== Python: 验证 fluxus --python -O2 xxx.py -o ${PY_OUTPUT_NAME} vs python xxx.py ==="
  for py in "${PYTHON_TARGETS[@]}"; do
    test_python_file "$py"
    echo
  done
fi

if [ ${#GO_TARGETS[@]} -gt 0 ]; then
  if command -v "$GO_CMD" >/dev/null 2>&1; then
    echo "=== Go: 验证 fluxus --go -O2 foo.go -o ${GO_OUTPUT_NAME} vs go run foo.go ==="
    for go_file in "${GO_TARGETS[@]}"; do
      test_go_file "$go_file"
      echo
    done
  else
    local_skipped=${#GO_TARGETS[@]}
    echo -e "${YELLOW}⚠️  未检测到 Go 命令 (${GO_CMD})，跳过 ${local_skipped} 个 Go 文件${NC}"
    SKIPPED_TESTS=$((SKIPPED_TESTS + local_skipped))
  fi
fi

echo "=========================================="
echo "测试总结"
echo "=========================================="
echo "总测试数: ${TOTAL_TESTS}"
echo -e "${GREEN}通过: ${PASSED_TESTS}${NC}"
echo -e "${RED}失败: ${FAILED_TESTS}${NC}"
if [ "$SKIPPED_TESTS" -gt 0 ]; then
  echo -e "${YELLOW}跳过: ${SKIPPED_TESTS}${NC}"
fi

if [ "$FAILED_TESTS" -gt 0 ]; then
  echo
  echo "失败详情:"
  for detail in "${FAILED_TEST_DETAILS[@]}"; do
    echo -e "${RED}  - ${detail}${NC}"
  done
  exit 1
else
  echo
  echo -e "${GREEN}所有已执行的 fluxus --python/--go -O2 转译均与解释器输出一致！${NC}"
fi
