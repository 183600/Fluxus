#!/usr/bin/env bash
# 无限循环执行 iflow 命令，前一次跑完才启动下一次

counter=1
while :; do
    echo "[$(date '+%F %T')] ===== 第 $counter 次运行开始 ====="
    
    sleep 15

    claude -p '运行cabal test，每个问题（包括warning）分别运行这个shell命令，每次执行shell命令只输入一个问题claude -p "<问题内容>" --dangerously-skip-permissions --output-format json' --dangerously-skip-permissions --output-format json
    
    exit_code=$?
    echo "[$(date '+%F %T')] ===== 第 $counter 次运行结束，退出码 $exit_code ====="
    echo

    counter=$((counter + 1))
done
