#!/bin/bash
# 设置正确的locale以避免警告
export LC_ALL=C.utf8
export LANG=C.utf8
export LANGUAGE=C.utf8

# 清理可能存在的无效locale设置
unset LC_CTYPE
unset LC_NUMERIC
unset LC_TIME
unset LC_COLLATE
unset LC_MONETARY
unset LC_MESSAGES
unset LC_PAPER
unset LC_NAME
unset LC_ADDRESS
unset LC_TELEPHONE
unset LC_MEASUREMENT
unset LC_IDENTIFICATION

# 执行传入的命令
exec "$@"