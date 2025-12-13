#!/usr/bin/env bash
set -u

# ==================== 配置参数 ====================
WATCHDOG_TIMEOUT=900  # 15分钟（秒）
CHECK_INTERVAL=30     # 检查间隔（秒）

# 心跳文件（使用脚本PID避免冲突）
HEARTBEAT_FILE="/tmp/typus_heartbeat_$$"

# ==================== 清理函数 ====================
cleanup() {
  rm -f "$HEARTBEAT_FILE"
}
trap cleanup EXIT INT TERM

# ==================== 工具：获取文件修改时间（兼容 Linux/macOS） ====================
get_mtime() {
  # Linux: stat -c%Y; macOS/BSD: stat -f %m
  if stat -c%Y "$1" >/dev/null 2>&1; then
    stat -c%Y "$1" 2>/dev/null || echo 0
  else
    stat -f %m "$1" 2>/dev/null || echo 0
  fi
}

# ==================== 监控函数 ====================
monitor_watchdog() {
  master_pid="$1"; timeout="$2"; hb_file="$3"; shift 3

  while [[ ! -f "$hb_file" ]]; do sleep 1; done
  last_heartbeat=$(date +%s)

  while true; do
    sleep $CHECK_INTERVAL

    if [[ -f "$hb_file" ]]; then
      current_time=$(get_mtime "$hb_file")
      (( current_time > last_heartbeat )) && last_heartbeat=$current_time
    fi

    now=$(date +%s)
    elapsed=$((now - last_heartbeat))

    if (( elapsed > timeout )); then
      echo "⚠️ [$(date '+%F %T')] 检测到${timeout}秒内无输出，正在重启..."
      kill -- -"$master_pid" 2>/dev/null || true
      sleep 1
      exec "$0" "$@"
    fi
  done
}

# ==================== 启动监控 ====================
if [[ ! -f "$HEARTBEAT_FILE" ]]; then
  monitor_watchdog "$$" "$WATCHDOG_TIMEOUT" "$HEARTBEAT_FILE" "$@" &
  touch "$HEARTBEAT_FILE"
  sleep 1
fi

# ==================== 工具：带心跳的命令执行（逐行刷新心跳） ====================
run_with_heartbeat() {
  stdbuf -oL -eL "$@" 2>&1 | awk -v hb="$HEARTBEAT_FILE" '{ print; fflush(); system("touch " hb) }'
  # 在 set -u 下安全读取 PIPESTATUS
  set +u
  local status=${PIPESTATUS[0]:-127}
  set -u
  return "$status"
}

# ==================== 主循环 ====================
trap 'echo; echo "已终止."; exit 0' INT TERM

while true; do
  touch "$HEARTBEAT_FILE"

  echo "===================="
  echo "$(date '+%F %T') 运行测试：cabal test --flags=\"-fast production\" --test-show-details=direct"
  echo "===================="

  # 运行测试：不写入日志文件，实时检测 warning，并按行刷新心跳
  stdbuf -oL -eL cabal test --flags="-fast production" --test-show-details=direct 2>&1 | \
    awk -v hb="$HEARTBEAT_FILE" '
      BEGIN { found=0 }
      {
        print
        fflush()
        # 每行刷新一次心跳，避免 watchdog 误杀
        system("touch " hb)
        l=tolower($0)
        if (l ~ /(warn(ing)?|警告)/) found=1
      }
      END {
        # 0=发现warning，1=未发现（用退出码传递给外层）
        exit found ? 0 : 1
      }
    '

  # 在 set -u 下安全读取 PIPESTATUS
  set +u
  ps0=${PIPESTATUS[0]:-255}  # cabal 的退出码，默认255表示未知
  ps1=${PIPESTATUS[1]:-255}  # awk 的退出码，默认255表示未知
  set -u

  CABAL_STATUS=$ps0
  AWK_STATUS=$ps1

  # 计算 HAS_WARNINGS（保守处理：拿不到 awk 退出码则认为有 warning）
  if [[ $AWK_STATUS -eq 0 ]]; then
    HAS_WARNINGS=1
  elif [[ $AWK_STATUS -eq 1 ]]; then
    HAS_WARNINGS=0
  else
    echo "⚠️ 未能获取 awk 退出码（AWK_STATUS=$AWK_STATUS），保守起见认为存在 warning"
    HAS_WARNINGS=1
  fi

  touch "$HEARTBEAT_FILE"

  if [[ $CABAL_STATUS -eq 0 && $HAS_WARNINGS -eq 0 ]]; then
    echo "✅ 未发现任何问题（包括 warning）——进行提交并增加测试用例"

    run_with_heartbeat iflow "给这个项目增加大量的测试用例，要使用QuickCheck think:high" --yolo || true

    # 在 git add . 之前，先用 iflow 清理根目录多余的 .md/.txt 文件
    echo "🧹 正在清理项目根目录多余的 .md/.txt 文件（如 TEST_ENHANCEMENT_SUMMARY.md、test_wall_production.txt）..."
    run_with_heartbeat iflow '删除项目根目录多余的.md文件或者.txt文件（像TEST_ENHANCEMENT_SUMMARY.md和test_wall_production.txt这样的）' --yolo || true

    git add .
    if git diff --cached --quiet; then
      echo "ℹ️ 没有文件变化可提交"
    else
      git commit -m "测试通过" || true
    fi
  else
    echo "⚠️ 发现问题或 warning（退出码=$CABAL_STATUS），调用 iflow 修复..."
    run_with_heartbeat iflow '解决cabal test --flags="-fast production" --test-show-details=direct显示的所有问题（包括warning），除非测试用例本身有编译错误，否则只修改测试用例以外的代码，debug时可通过加日志和打断点 think:high' --yolo || true
  fi

  echo "🔁 回到第 1 步..."

  touch "$HEARTBEAT_FILE"
  sleep 1
done