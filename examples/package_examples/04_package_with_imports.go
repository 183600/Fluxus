// 示例 4: 包声明后跟随多个导入
// 包声明必须在文件开头
// 导入语句紧随其后

package main

import (
	"fmt"
	"os"
	"time"
)

func main() {
	fmt.Println("当前时间:", time.Now())
	fmt.Println("操作系统:", os.Getenv("OS"))
}
