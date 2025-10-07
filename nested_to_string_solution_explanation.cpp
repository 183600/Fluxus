/*
 * This file explains the nested std::to_string error and demonstrates the solution
 *
 * PROBLEM:
 * std::to_string() only accepts numeric types (int, float, double, etc.) as arguments.
 * It does NOT accept std::string as an argument.
 * So std::to_string(std::to_string(numeric_value)) causes a compilation error,
 * because the inner std::to_string returns a std::string, which cannot be passed
 * to the outer std::to_string.
 *
 * SOLUTIONS:
 * 1. Use std::to_string only on numeric values
 * 2. For concatenating multiple converted values, use string concatenation with +
 * 3. For complex string building, consider std::ostringstream
 */

#include <iostream>
#include <string>
#include <sstream>  // for std::ostringstream

int main() {
    int num1 = 42;
    double num2 = 3.14159;
    
    // CORRECT: Direct conversion of numeric values
    std::cout << "Direct conversion: " << std::to_string(num1) << std::endl;
    std::cout << "Direct conversion: " << std::to_string(num2) << std::endl;
    
    // CORRECT: String concatenation of multiple conversions
    std::string concatenated = "Number1: " + std::to_string(num1) + ", Number2: " + std::to_string(num2);
    std::cout << concatenated << std::endl;
    
    // ALTERNATIVE: Using ostringstream for complex string building
    std::ostringstream oss;
    oss << "Number1: " << num1 << ", Number2: " << num2 << ", Sum: " << (num1 + static_cast<int>(num2));
    std::cout << oss.str() << std::endl;
    
    // WRONG (this would cause compilation error):
    // std::string error = std::to_string(std::to_string(num1));  // Error: no matching function
    
    return 0;
}