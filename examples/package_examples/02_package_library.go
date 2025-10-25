// 示例 2: 自定义包名 - 库包
// 非 main 包用于创建可重用的库
// 包名通常与目录名相同

package mathutils

// Add 返回两个整数的和
func Add(a, b int) int {
	return a + b
}

// Multiply 返回两个整数的积
func Multiply(a, b int) int {
	return a * b
}
