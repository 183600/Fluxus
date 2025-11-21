#!/bin/bash

set -euo pipefail

# Python/Go -> C++ equivalence verifier
# Ensures that running `fluxus --python/-O2 ... -o <name>` (and the Go variant)
# produces the same observable output as executing the original interpreter.

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

PYTHON_CMD=${PYTHON_CMD:-python3}
GO_CMD=${GO_CMD:-go}
TIMEOUT_SECONDS=${FLUXUS_VERIFY_TIMEOUT:-10}
KEEP_ARTIFACTS=${KEEP_FLUXUS_VERIFY_ARTIFACTS:-0}
WORK_ROOT=".verify_fluxus_work"

TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
SKIPPED_TESTS=0

declare -a FAILED_TEST_NAMES=()
declare -a GENERATED_BINARIES=()

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

make_artifact_prefix() {
  local source_path=$1
  local base_name=$2
  local digest short_hash
  digest=$(printf "%s" "$source_path" | md5sum | awk '{print $1}')
  short_hash=${digest:0:8}
  printf "%s_%s" "$base_name" "$short_hash"
}

cleanup() {
  if [ "$KEEP_ARTIFACTS" = "1" ]; then
    echo -e "${YELLOW}保留验证工件以便调试 (目录: ${WORK_ROOT})${NC}"
    return
  fi

  if [ -d "$WORK_ROOT" ]; then
    rm -rf "$WORK_ROOT"
  fi

  if [ "${#GENERATED_BINARIES[@]}" -gt 0 ]; then
    rm -f "${GENERATED_BINARIES[@]}"
  fi
}
trap cleanup EXIT

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

test_python_file() {
  local py_file=$1
  local base_name
  base_name=$(basename "$py_file")
  base_name=${base_name%.*}
  local artifact_prefix
  artifact_prefix=$(make_artifact_prefix "$py_file" "$base_name")
  local exe_file="${artifact_prefix}_fluxus"
  local work_dir="$WORK_ROOT/${artifact_prefix}_py"

  ((TOTAL_TESTS++))
  echo "----------------------------------------"
  echo "测试 #${TOTAL_TESTS} (Python): ${py_file}"

  if [ ! -f "$py_file" ]; then
    echo -e "${RED}✗ Python 文件不存在${NC}"
    FAILED_TEST_NAMES+=("${py_file}: 文件不存在")
    ((FAILED_TESTS++))
    return 1
  fi

  rm -rf "$work_dir"
  mkdir -p "$work_dir"
  GENERATED_BINARIES+=("$exe_file")

  local compile_cmd=("$FLUXUS_BIN_PATH" "--python" "-O2" "$py_file" "-o" "$exe_file" "--work-dir" "$work_dir")
  echo "运行: ${compile_cmd[*]}"

  local compile_output
  if ! run_and_capture compile_output "${compile_cmd[@]}"; then
    echo -e "${RED}✗ Fluxus 编译失败${NC}"
    printf '%s\n' "$compile_output"
    FAILED_TEST_NAMES+=("${py_file}: Fluxus 编译失败")
    ((FAILED_TESTS++))
    return 1
  fi
  echo -e "${GREEN}✓ 生成可执行文件: ${exe_file}${NC}"

  local python_output fluxus_output
  if ! run_and_capture python_output "${TIMEOUT_CMD[@]}" "$PYTHON_CMD" "$py_file"; then
    echo -e "${RED}✗ Python 执行失败${NC}"
    printf '%s\n' "$python_output"
    FAILED_TEST_NAMES+=("${py_file}: Python 执行失败")
    ((FAILED_TESTS++))
    return 1
  fi

  if ! run_and_capture fluxus_output "${TIMEOUT_CMD[@]}" "./$exe_file"; then
    echo -e "${RED}✗ Fluxus 可执行文件运行失败${NC}"
    printf '%s\n' "$fluxus_output"
    FAILED_TEST_NAMES+=("${py_file}: Fluxus 可执行文件运行失败")
    ((FAILED_TESTS++))
    return 1
  fi

  if [ "$python_output" = "$fluxus_output" ]; then
    echo -e "${GREEN}✓ 输出与 'python ${py_file}' 完全一致${NC}"
    ((PASSED_TESTS++))
  else
    echo -e "${RED}✗ 输出不一致${NC}"
    echo "Python 输出:"
    printf '%s\n' "$python_output"
    echo "Fluxus 输出:"
    printf '%s\n' "$fluxus_output"
    FAILED_TEST_NAMES+=("${py_file}: 输出不一致")
    ((FAILED_TESTS++))
  fi

  if [ "$KEEP_ARTIFACTS" != "1" ]; then
    rm -f "$exe_file"
    rm -rf "$work_dir"
  fi
}

test_go_file() {
  local go_file=$1
  local base_name
  base_name=$(basename "$go_file")
  base_name=${base_name%.*}
  local artifact_prefix
  artifact_prefix=$(make_artifact_prefix "$go_file" "$base_name")
  local exe_file="${artifact_prefix}_fluxus_go"
  local work_dir="$WORK_ROOT/${artifact_prefix}_go"

  ((TOTAL_TESTS++))
  echo "----------------------------------------"
  echo "测试 #${TOTAL_TESTS} (Go): ${go_file}"

  if [ ! -f "$go_file" ]; then
    echo -e "${RED}✗ Go 文件不存在${NC}"
    FAILED_TEST_NAMES+=("${go_file}: 文件不存在")
    ((FAILED_TESTS++))
    return 1
  fi

  rm -rf "$work_dir"
  mkdir -p "$work_dir"
  GENERATED_BINARIES+=("$exe_file")

  local compile_cmd=("$FLUXUS_BIN_PATH" "--go" "-O2" "$go_file" "-o" "$exe_file" "--work-dir" "$work_dir")
  echo "运行: ${compile_cmd[*]}"

  local compile_output
  if ! run_and_capture compile_output "${compile_cmd[@]}"; then
    echo -e "${RED}✗ Fluxus Go 编译失败${NC}"
    printf '%s\n' "$compile_output"
    FAILED_TEST_NAMES+=("${go_file}: Fluxus 编译失败")
    ((FAILED_TESTS++))
    return 1
  fi
  echo -e "${GREEN}✓ 生成可执行文件: ${exe_file}${NC}"

  local go_output fluxus_output
  if ! run_and_capture go_output "${TIMEOUT_CMD[@]}" "$GO_CMD" run "$go_file"; then
    echo -e "${RED}✗ go run 执行失败${NC}"
    printf '%s\n' "$go_output"
    FAILED_TEST_NAMES+=("${go_file}: go run 执行失败")
    ((FAILED_TESTS++))
    return 1
  fi

  if ! run_and_capture fluxus_output "${TIMEOUT_CMD[@]}" "./$exe_file"; then
    echo -e "${RED}✗ Fluxus 可执行文件运行失败${NC}"
    printf '%s\n' "$fluxus_output"
    FAILED_TEST_NAMES+=("${go_file}: Fluxus 可执行文件运行失败")
    ((FAILED_TESTS++))
    return 1
  fi

  if [ "$go_output" = "$fluxus_output" ]; then
    echo -e "${GREEN}✓ 输出与 'go run ${go_file}' 完全一致${NC}"
    ((PASSED_TESTS++))
  else
    echo -e "${RED}✗ 输出不一致${NC}"
    echo "go run 输出:"
    printf '%s\n' "$go_output"
    echo "Fluxus 输出:"
    printf '%s\n' "$fluxus_output"
    FAILED_TEST_NAMES+=("${go_file}: 输出不一致")
    ((FAILED_TESTS++))
  fi

  if [ "$KEEP_ARTIFACTS" != "1" ]; then
    rm -f "$exe_file"
    rm -rf "$work_dir"
  fi
}

PYTHON_TESTS=(
  "simple_test.py"
  "fibonacci.py"
  "factorial.py"
  "simple_math.py"
  "simple_function.py"
  "simple_loop.py"
  "simple_py.py"
  "print_test.py"
  "examples/python/fibonacci.py"
)

GO_TESTS=(
  "examples/go/simple_hello.go"
)

echo "=========================================="
echo "Fluxus Python/Go → C++ 输出等价性验证"
echo "=========================================="
echo

echo "清理旧的工作目录..."
rm -rf "$WORK_ROOT"
mkdir -p "$WORK_ROOT"

echo "构建 Fluxus 编译器..."
BUILD_LOG=$(mktemp)
if ! cabal build >"$BUILD_LOG" 2>&1; then
  echo -e "${RED}构建失败！${NC}"
  tail -n 20 "$BUILD_LOG"
  rm -f "$BUILD_LOG"
  exit 1
fi
tail -n 5 "$BUILD_LOG"
rm -f "$BUILD_LOG"
echo -e "${GREEN}构建成功${NC}"
echo

FLUXUS_BIN_PATH=$(resolve_fluxus_binary)
if [ -z "$FLUXUS_BIN_PATH" ]; then
  echo -e "${RED}无法定位 fluxus 可执行文件。请先运行 'cabal build' 或设置 FLUXUS_BIN.${NC}"
  exit 1
fi

echo "使用的 Fluxus 命令: ${FLUXUS_BIN_PATH}"
echo

echo "=== Python 等价性测试 ==="
for py_file in "${PYTHON_TESTS[@]}"; do
  test_python_file "$py_file"
  echo
done

if [ "${#GO_TESTS[@]}" -gt 0 ]; then
  if command -v "$GO_CMD" >/dev/null 2>&1; then
    echo "=== Go 等价性测试 ==="
    for go_file in "${GO_TESTS[@]}"; do
      test_go_file "$go_file"
      echo
    done
  else
    local_skipped=${#GO_TESTS[@]}
    echo -e "${YELLOW}⚠️  未检测到 Go 工具链 (${GO_CMD})，跳过 ${local_skipped} 个 Go 测试${NC}"
    SKIPPED_TESTS=$((SKIPPED_TESTS + local_skipped))
    echo
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
  for failed in "${FAILED_TEST_NAMES[@]}"; do
    echo -e "${RED}  - ${failed}${NC}"
  done
  exit 1
else
  echo
  echo -e "${GREEN}所有可执行测试均与原始解释器输出匹配！${NC}"
  exit 0
fi
