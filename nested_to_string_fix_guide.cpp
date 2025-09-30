// Detailed guide on fixing nested std::to_string calls
#include <iostream>
#include <string>
#include <sstream>

int main() {
    // PROBLEM: Nested std::to_string calls
    // The following would cause a compilation error:
    // std::cout << "Error: " + std::to_string(std::to_string(42)) << std::endl;
    // Error: no matching function for call to 'to_string' with std::string argument
    
    // The error occurs because std::to_string() expects numeric arguments (int, float, double, etc.)
    // but std::to_string(std::to_string(42)) tries to call to_string on a std::string,
    // which is not supported.
    
    std::cout << "=== DEMONSTRATION OF THE PROBLEM ===" << std::endl;
    std::cout << "The following code causes compilation error:" << std::endl;
    std::cout << "// std::to_string(std::to_string(42))  // ERROR!" << std::endl;
    std::cout << std::endl;
    
    std::cout << "=== SOLUTIONS ===" << std::endl;
    
    // SOLUTION 1: Correct use of std::to_string() without nesting
    int value1 = 123;
    double value2 = 45.67;
    
    std::cout << "SOLUTION 1 - Direct conversion:" << std::endl;
    std::cout << "Single conversion: " + std::to_string(value1) << std::endl;
    std::cout << "Another example: " + std::to_string(value2) << std::endl;
    std::cout << std::endl;
    
    // SOLUTION 2: Concatenating multiple converted values
    std::cout << "SOLUTION 2 - Concatenating converted values:" << std::endl;
    std::string result = "Values: " + std::to_string(value1) + " and " + std::to_string(value2);
    std::cout << result << std::endl;
    std::cout << std::endl;
    
    // SOLUTION 3: Using string streams for complex concatenation
    std::cout << "SOLUTION 3 - Using string streams:" << std::endl;
    std::ostringstream oss;
    oss << "Value1: " << value1 << ", Value2: " << value2;
    std::cout << oss.str() << std::endl;
    std::cout << std::endl;
    
    // SOLUTION 4: Using C++14 string concatenation with braces
    std::cout << "SOLUTION 4 - Multiple to_string calls without nesting:" << std::endl;
    std::cout << "Result: " + std::to_string(10) + " + " + std::to_string(20) + " = " + std::to_string(30) << std::endl;
    std::cout << std::endl;
    
    // SOLUTION 5: If working with custom types or complex objects
    std::cout << "SOLUTION 5 - For more complex scenarios:" << std::endl;
    // If you have a custom class, you might need to implement your own conversion
    // For built-in types, just use std::to_string() once per value
    std::cout << "For integer: " + std::to_string(123456) << std::endl;
    std::cout << "For float: " + std::to_string(3.14159f) << std::endl;
    std::cout << "For double: " + std::to_string(2.718281828) << std::endl;
    std::cout << std::endl;
    
    // SOLUTION 6: Modern C++ alternatives (C++20 and later)
    std::cout << "SOLUTION 6 - Modern approaches:" << std::endl;
    // For C++20 and later, you can use std::format (if available)
    // std::cout << std::format("Values: {} and {}", value1, value2) << std::endl;
    
    // For C++11/14/17, stick with stringstream or string concatenation
    std::stringstream ss;
    ss << "Formatted: value1=" << value1 << ", value2=" << value2;
    std::cout << ss.str() << std::endl;
    std::cout << std::endl;
    
    std::cout << "=== KEY POINTS ===" << std::endl;
    std::cout << "1. std::to_string() only accepts numeric types, not std::string" << std::endl;
    std::cout << "2. Never nest std::to_string() calls" << std::endl;
    std::cout << "3. Use direct conversion: std::to_string(number)" << std::endl;
    std::cout << "4. For concatenation: combine multiple std::to_string() calls with + operator" << std::endl;
    std::cout << "5. For complex formatting: consider std::ostringstream" << std::endl;
    
    return 0;
}