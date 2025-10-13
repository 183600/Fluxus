package main

import (
	"fmt"
	"go/build"
	"path/filepath"
	"runtime"
)

// 测试包导入和初始化
var (
	// 包级别变量
	PackageVersion = "1.0.0"
	initialized    bool
)

func init() {
	// init函数在包导入时自动执行
	fmt.Println("Package initialization: init() called")
	initialized = true
}

// 测试包的可见性规则
type PublicType struct {
	PublicField  int
	privateField string // 不会被外部包访问
}

func (p *PublicType) PublicMethod() string {
	return fmt.Sprintf("Public method: %d, %s", p.PublicField, p.privateField)
}

func (p *PublicType) privateMethod() string {
	return "This is private"
}

// 公开函数
func PublicFunction() {
	fmt.Println("This is a public function")
}

// 私有函数
func privateFunction() {
	fmt.Println("This is a private function")
}

// 测试类型别名和类型定义
type MyInt int
type StringAlias = string

func testTypeAliases() {
	fmt.Println("\n=== Testing type aliases ===")
	
	var x MyInt = 42
	var y int = 100
	
	// MyInt和int是不同的类型
	fmt.Printf("MyInt: %v (type: %T)\n", x, x)
	fmt.Printf("int: %v (type: %T)\n", y, y)
	
	// 需要显式转换
	z := int(x) + y
	fmt.Printf("Sum: %d\n", z)
	
	// 类型别名
	var s StringAlias = "hello"
	fmt.Printf("StringAlias: %v (type: %T)\n", s, s)
}

// 测试方法集
type Counter int

func (c Counter) Value() int {
	return int(c)
}

func (c *Counter) Increment() {
	*c++
}

func testMethodSets() {
	fmt.Println("\n=== Testing method sets ===")
	
	var c Counter = 0
	fmt.Printf("Initial value: %d\n", c.Value())
	
	// 值方法可以通过值或指针调用
	fmt.Printf("Value method on value: %d\n", c.Value())
	
	// 指针方法需要指针
	c.Increment()
	fmt.Printf("After increment: %d\n", c.Value())
}

// 测试嵌入和提升
type Base struct {
	ID   int
	Name string
}

func (b *Base) BaseMethod() string {
	return fmt.Sprintf("Base: %d - %s", b.ID, b.Name)
}

type Derived struct {
	Base // 嵌入
	Extra string
}

func testEmbeddingPromotion() {
	fmt.Println("\n=== Testing embedding and promotion ===")
	
	d := Derived{
		Base:  Base{ID: 1, Name: "test"},
		Extra: "extra data",
	}
	
	// 提升字段
	fmt.Printf("ID (promoted): %d\n", d.ID)
	fmt.Printf("Name (promoted): %s\n", d.Name)
	fmt.Printf("Extra: %s\n", d.Extra)
	
	// 提升方法
	fmt.Println(d.BaseMethod())
}

// 测试接口嵌入
type Reader interface {
	Read() string
}

type Writer interface {
	Write(string)
}

type ReadWriter interface {
	Reader
	Writer
}

type MyReadWriter struct {
	data string
}

func (rw *MyReadWriter) Read() string {
	return rw.data
}

func (rw *MyReadWriter) Write(s string) {
	rw.data = s
}

func testInterfaceEmbedding() {
	fmt.Println("\n=== Testing interface embedding ===")
	
	var rw ReadWriter = &MyReadWriter{}
	rw.Write("hello")
	fmt.Printf("Read: %s\n", rw.Read())
}

// 测试包信息
func testPackageInfo() {
	fmt.Println("\n=== Testing package information ===")
	
	// 获取当前包信息
	pc, file, line, ok := runtime.Caller(0)
	if ok {
		fn := runtime.FuncForPC(pc)
		fmt.Printf("Function: %s\n", fn.Name())
		fmt.Printf("File: %s\n", filepath.Base(file))
		fmt.Printf("Line: %d\n", line)
	}
	
	// 获取Go环境信息
	fmt.Printf("GOROOT: %s\n", runtime.GOROOT())
	fmt.Printf("GOOS: %s\n", runtime.GOOS)
	fmt.Printf("GOARCH: %s\n", runtime.GOARCH)
	
	// 构建上下文
	ctx := build.Default
	fmt.Printf("Build Context GOPATH: %s\n", ctx.GOPATH)
}

// 测试常量和枚举
type Status int

const (
	StatusPending Status = iota
	StatusRunning
	StatusCompleted
	StatusFailed
)

func (s Status) String() string {
	names := []string{"Pending", "Running", "Completed", "Failed"}
	if s < 0 || int(s) >= len(names) {
		return "Unknown"
	}
	return names[s]
}

func testConstantsEnums() {
	fmt.Println("\n=== Testing constants and enums ===")
	
	statuses := []Status{StatusPending, StatusRunning, StatusCompleted, StatusFailed}
	for _, status := range statuses {
		fmt.Printf("Status %d: %s\n", status, status)
	}
}

// 测试空白标识符
func testBlankIdentifier() {
	fmt.Println("\n=== Testing blank identifier ===")
	
	// 忽略返回值
	_, err := fmt.Println("Testing blank identifier")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	}
	
	// 仅为副作用导入包（在实际代码中）
	// import _ "database/sql"
	
	// 强制接口实现检查
	var _ ReadWriter = (*MyReadWriter)(nil)
	fmt.Println("Interface implementation check passed")
}

// 测试包的导出规则
type exportTest struct {
	// 结构体是未导出的，但可以有导出的字段
	ExportedField int
	unexported    int
}

func NewExportTest(exported, unexported int) exportTest {
	return exportTest{
		ExportedField: exported,
		unexported:    unexported,
	}
}

func testExportRules() {
	fmt.Println("\n=== Testing export rules ===")
	
	et := NewExportTest(1, 2)
	fmt.Printf("Exported field: %d\n", et.ExportedField)
	fmt.Printf("Unexported field (internal): %d\n", et.unexported)
}

// 测试包级别初始化顺序
var (
	a = b + 1 // 依赖b
	b = 10
	c = initC() // 函数初始化
)

func initC() int {
	fmt.Println("initC() called during package initialization")
	return 20
}

func testInitializationOrder() {
	fmt.Println("\n=== Testing initialization order ===")
	fmt.Printf("a = %d (should be 11)\n", a)
	fmt.Printf("b = %d (should be 10)\n", b)
	fmt.Printf("c = %d (should be 20)\n", c)
}

func main() {
	fmt.Println("=== Module and Package Tests ===")
	fmt.Printf("Package version: %s\n", PackageVersion)
	fmt.Printf("Initialized: %v\n", initialized)
	
	testTypeAliases()
	testMethodSets()
	testEmbeddingPromotion()
	testInterfaceEmbedding()
	testPackageInfo()
	testConstantsEnums()
	testBlankIdentifier()
	testExportRules()
	testInitializationOrder()
	
	fmt.Println("\n=== Module and package tests completed ===")
}
