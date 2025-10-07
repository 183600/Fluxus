#!/usr/bin/env bash
# 无限循环执行 qwen 命令，前一次跑完才启动下一次

counter=1
while :; do
    echo "[$(date '+%F %T')] ===== 第 $counter 次运行开始 ====="
    
    sleep 15

    qwen -p '解决stack test和cabal test显示的问题（包括warning），只修复一个bug，debug的时候可以通过加日志和打断点（在命令行当中打断点）来debug，每个问题分别运行这个shell修复claude -p "修复warning的时候可以通过加日志和打断点（在命令行当中打断点）来debug，<命令，例如cabal test>显示<内容>" --dangerously-skip-permissions --output-format json' --yolo
    
    exit_code=$?
    echo "[$(date '+%F %T')] ===== 第 $counter 次运行结束，退出码 $exit_code ====="
    echo

    counter=$((counter + 1))
done
