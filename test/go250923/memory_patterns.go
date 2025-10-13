package main

import (
	"fmt"
	"runtime"
	"sync"
	"time"
	"unsafe"
)

// 测试值类型 vs 指针类型
type LargeStruct struct {
	Data [1000]int
}

func passByValue(s LargeStruct) {
	// 复制整个结构体
	s.Data[0] = 999
}

func passByPointer(s *LargeStruct) {
	// 只复制指针
	s.Data[0] = 999
}

func testValueVsPointer() {
	fmt.Println("=== Testing value vs pointer ===")
	
	s := LargeStruct{}
	s.Data[0] = 1
	
	fmt.Printf("Before passByValue: %d\n", s.Data[0])
	passByValue(s)
	fmt.Printf("After passByValue: %d (unchanged)\n", s.Data[0])
	
	passByPointer(&s)
	fmt.Printf("After passByPointer: %d (changed)\n", s.Data[0])
	
	// 大小比较
	fmt.Printf("Size of struct: %d bytes\n", unsafe.Sizeof(s))
	fmt.Printf("Size of pointer: %d bytes\n", unsafe.Sizeof(&s))
}

// 测试逃逸分析
func stackAllocation() int {
	// 这个变量应该分配在栈上
	x := 42
	return x
}

func heapAllocation() *int {
	// 这个变量会逃逸到堆上
	x := 42
	return &x
}

func testEscapeAnalysis() {
	fmt.Println("\n=== Testing escape analysis ===")
	
	// 栈分配
	val := stackAllocation()
	fmt.Printf("Stack allocated value: %d\n", val)
	
	// 堆分配
	ptr := heapAllocation()
	fmt.Printf("Heap allocated value: %d\n", *ptr)
	
	// 大对象通常分配在堆上
	large := make([]int, 10000)
	fmt.Printf("Large slice length: %d\n", len(large))
}

// 测试内存池化
type Object struct {
	ID   int
	Data [100]byte
}

var objectPool = sync.Pool{
	New: func() interface{} {
		fmt.Println("Creating new object")
		return &Object{}
	},
}

func testObjectPooling() {
	fmt.Println("\n=== Testing object pooling ===")
	
	// 获取对象
	obj1 := objectPool.Get().(*Object)
	obj1.ID = 1
	fmt.Printf("Got object 1: ID=%d\n", obj1.ID)
	
	obj2 := objectPool.Get().(*Object)
	obj2.ID = 2
	fmt.Printf("Got object 2: ID=%d\n", obj2.ID)
	
	// 归还对象
	objectPool.Put(obj1)
	objectPool.Put(obj2)
	fmt.Println("Returned objects to pool")
	
	// 再次获取（应该复用）
	obj3 := objectPool.Get().(*Object)
	fmt.Printf("Got object 3: ID=%d (reused)\n", obj3.ID)
}

// 测试内存对齐
type UnalignedStruct struct {
	a bool  // 1 byte
	b int64 // 8 bytes
	c bool  // 1 byte
}

type AlignedStruct struct {
	a bool  // 1 byte
	c bool  // 1 byte
	b int64 // 8 bytes
}

func testMemoryAlignment() {
	fmt.Println("\n=== Testing memory alignment ===")
	
	var unaligned UnalignedStruct
	var aligned AlignedStruct
	
	fmt.Printf("Unaligned struct size: %d bytes\n", unsafe.Sizeof(unaligned))
	fmt.Printf("Aligned struct size: %d bytes\n", unsafe.Sizeof(aligned))
	
	// 字段偏移
	fmt.Printf("Unaligned offsets: a=%d, b=%d, c=%d\n",
		unsafe.Offsetof(unaligned.a),
		unsafe.Offsetof(unaligned.b),
		unsafe.Offsetof(unaligned.c))
	
	fmt.Printf("Aligned offsets: a=%d, c=%d, b=%d\n",
		unsafe.Offsetof(aligned.a),
		unsafe.Offsetof(aligned.c),
		unsafe.Offsetof(aligned.b))
}

// 测试切片容量管理
func testSliceCapacity() {
	fmt.Println("\n=== Testing slice capacity management ===")
	
	// 预分配容量
	s1 := make([]int, 0, 10)
	fmt.Printf("Pre-allocated slice: len=%d, cap=%d\n", len(s1), cap(s1))
	
	// 动态增长
	s2 := []int{}
	for i := 0; i < 20; i++ {
		oldCap := cap(s2)
		s2 = append(s2, i)
		if cap(s2) != oldCap {
			fmt.Printf("Capacity grew from %d to %d at length %d\n", oldCap, cap(s2), len(s2))
		}
	}
	
	// 使用完整容量
	s3 := make([]int, 5, 10)
	fmt.Printf("Slice: len=%d, cap=%d\n", len(s3), cap(s3))
	s3 = s3[:cap(s3)] // 扩展到容量
	fmt.Printf("After extending to capacity: len=%d, cap=%d\n", len(s3), cap(s3))
}

// 测试垃圾回收
func testGarbageCollection() {
	fmt.Println("\n=== Testing garbage collection ===")
	
	// 获取GC统计信息
	var m runtime.MemStats
	runtime.ReadMemStats(&m)
	fmt.Printf("Initial heap alloc: %d KB\n", m.Alloc/1024)
	
	// 分配大量内存
	allocated := make([]*[]byte, 100)
	for i := 0; i < 100; i++ {
		data := make([]byte, 1024*1024) // 1MB
		allocated[i] = &data
	}
	
	runtime.ReadMemStats(&m)
	fmt.Printf("After allocation: %d KB\n", m.Alloc/1024)
	
	// 清除引用
	allocated = nil
	
	// 强制GC
	runtime.GC()
	time.Sleep(100 * time.Millisecond)
	
	runtime.ReadMemStats(&m)
	fmt.Printf("After GC: %d KB\n", m.Alloc/1024)
	fmt.Printf("Number of GC runs: %d\n", m.NumGC)
}

// 测试finalizer
type Resource struct {
	ID int
}

func testFinalizer() {
	fmt.Println("\n=== Testing finalizer ===")
	
	r := &Resource{ID: 1}
	
	// 设置finalizer
	runtime.SetFinalizer(r, func(r *Resource) {
		fmt.Printf("Finalizer called for resource %d\n", r.ID)
	})
	
	fmt.Printf("Created resource %d\n", r.ID)
	
	// 清除引用
	r = nil
	
	// 触发GC
	runtime.GC()
	time.Sleep(100 * time.Millisecond)
}

// 测试零值优化
func testZeroValueOptimization() {
	fmt.Println("\n=== Testing zero value optimization ===")
	
	// 零值slice/map不需要初始化
	var s []int
	fmt.Printf("Nil slice: len=%d, cap=%d, is nil=%v\n", len(s), cap(s), s == nil)
	
	var m map[string]int
	fmt.Printf("Nil map: len=%d, is nil=%v\n", len(m), m == nil)
	
	// 可以安全地读取，但不能写入
	fmt.Printf("Read from nil map: %d\n", m["key"])
	
	// 写入需要初始化
	m = make(map[string]int)
	m["key"] = 42
	fmt.Printf("After initialization: %d\n", m["key"])
}

// 测试指针运算限制
func testPointerArithmetic() {
	fmt.Println("\n=== Testing pointer operations ===")
	
	arr := [5]int{1, 2, 3, 4, 5}
	
	// Go不支持指针运算，但可以使用unsafe
	ptr := unsafe.Pointer(&arr[0])
	fmt.Printf("First element: %d\n", *(*int)(ptr))
	
	// 使用unsafe进行"指针运算"
	nextPtr := unsafe.Pointer(uintptr(ptr) + unsafe.Sizeof(arr[0]))
	fmt.Printf("Second element (via unsafe): %d\n", *(*int)(nextPtr))
	
	// 更安全的方式是使用切片
	slice := arr[:]
	fmt.Printf("Safe access: %d, %d\n", slice[0], slice[1])
}

// 测试内存屏障和可见性
func testMemoryVisibility() {
	fmt.Println("\n=== Testing memory visibility ===")
	
	var flag bool
	var data int
	
	// 写入goroutine
	go func() {
		data = 42
		flag = true // 发布数据
	}()
	
	// 读取goroutine
	go func() {
		for !flag {
			// 自旋等待
			runtime.Gosched()
		}
		fmt.Printf("Data read: %d\n", data)
	}()
	
	time.Sleep(100 * time.Millisecond)
}

// 测试结构体填充
type PaddedStruct struct {
	a int8  // 1 byte
	b int64 // 8 bytes (需要对齐到8字节边界)
	c int8  // 1 byte
	d int64 // 8 bytes (需要对齐到8字节边界)
}

func testStructPadding() {
	fmt.Println("\n=== Testing struct padding ===")
	
	var ps PaddedStruct
	fmt.Printf("Struct size: %d bytes\n", unsafe.Sizeof(ps))
	fmt.Printf("Field a offset: %d\n", unsafe.Offsetof(ps.a))
	fmt.Printf("Field b offset: %d\n", unsafe.Offsetof(ps.b))
	fmt.Printf("Field c offset: %d\n", unsafe.Offsetof(ps.c))
	fmt.Printf("Field d offset: %d\n", unsafe.Offsetof(ps.d))
}

func main() {
	fmt.Println("=== Memory Management Patterns ===")
	
	testValueVsPointer()
	testEscapeAnalysis()
	testObjectPooling()
	testMemoryAlignment()
	testSliceCapacity()
	testGarbageCollection()
	testFinalizer()
	testZeroValueOptimization()
	testPointerArithmetic()
	testMemoryVisibility()
	testStructPadding()
	
	fmt.Println("\n=== Memory patterns tests completed ===")
}
