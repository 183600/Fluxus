// 示例 3: 包名中的下划线
// Go 允许在包名中使用下划线
// 但通常建议使用简短的小写名称

package string_utils

import "strings"

// ToUpperCase 将字符串转换为大写
func ToUpperCase(s string) string {
	return strings.ToUpper(s)
}

// ToLowerCase 将字符串转换为小写
func ToLowerCase(s string) string {
	return strings.ToLower(s)
}
