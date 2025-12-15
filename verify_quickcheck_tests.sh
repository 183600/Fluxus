#!/bin/bash
echo "=== QuickCheck 测试用例统计 ==="
echo ""
echo "测试组数量:"
grep -E "^[a-zA-Z].*Properties :: Spec$" test/Test/Fluxus/QuickCheckProperties.hs | wc -l
echo ""
echo "测试组列表:"
grep -E "^[a-zA-Z].*Properties :: Spec$" test/Test/Fluxus/QuickCheckProperties.hs | sed 's/ :: Spec//'
echo ""
echo "属性测试数量:"
grep -c 'prop "' test/Test/Fluxus/QuickCheckProperties.hs
echo ""
echo "属性测试列表:"
grep 'prop "' test/Test/Fluxus/QuickCheckProperties.hs | sed 's/.*prop "/  - /' | sed 's/" .*//'
echo ""
echo "文件总行数:"
wc -l test/Test/Fluxus/QuickCheckProperties.hs | awk '{print $1}'
