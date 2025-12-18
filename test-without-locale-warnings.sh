#!/bin/sh
# 运行 cabal test 并过滤掉 locale 警告
# 使用方法: ./test-without-locale-warnings.sh [cabal test 参数...]

# 运行测试，使用 C locale 避免 setlocale 警告
export LC_ALL=C
export LANG=C
unset LANGUAGE

# 使用临时文件捕获stderr并过滤
tmpfile=$(mktemp)
trap "rm -f $tmpfile" EXIT

if [ $# -eq 0 ]; then
    cabal test --flags="-fast production" --test-show-details=direct 2>"$tmpfile"
    exitcode=$?
else
    cabal "$@" 2>"$tmpfile"
    exitcode=$?
fi

# 输出过滤后的stderr
grep -v "warning: setlocale" "$tmpfile" >&2

exit $exitcode
