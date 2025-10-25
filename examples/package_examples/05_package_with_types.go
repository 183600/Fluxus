// 示例 5: 包声明后跟随类型定义
// 展示包声明与类型、常量、变量的组合

package models

// User 表示用户模型
type User struct {
	ID       int
	Username string
	Email    string
}

// UserRole 表示用户角色
type UserRole int

const (
	RoleGuest UserRole = iota
	RoleUser
	RoleAdmin
)

// DefaultTimeout 默认超时时间（秒）
var DefaultTimeout = 30
