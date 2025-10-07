// Example demonstrating how to use the nested_to_string utilities
#include <iostream>
#include <string>
#include "nested_to_string_utils.h"

int main() {
    std::cout << "=== Using Utility Functions to Avoid Nested to_string Issues ===" << std::endl;
    
    // Example 1: Safe conversion of a single value
    int val1 = 42;
    std::cout << "Safe single conversion: " + safe_to_string(val1) << std::endl;
    
    // Example 2: Concatenating two values
    int val2 = 100;
    double val3 = 3.14159;
    std::string result = "Result: " + concat_to_string(val2, val3);
    std::cout << result << std::endl;
    
    // Example 3: Using the generic join function (C++17 and later)
    // For this example, we'll use a simpler approach compatible with older standards
    std::string joined = "Values: " + std::to_string(10) + ", " + 
                        std::to_string(20.5) + ", " + std::to_string(30);
    std::cout << joined << std::endl;
    
    // Example 4: More complex concatenation
    std::string complex = "Complex: [" + std::to_string(1) + ", " +
                         std::to_string(2) + ", " + std::to_string(3) + "]";
    std::cout << complex << std::endl;
    
    std::cout << std::endl;
    std::cout << "=== Key Takeaways ===" << std::endl;
    std::cout << "1. Always use std::to_string only with numeric types" << std::endl;
    std::cout << "2. Never nest std::to_string calls like std::to_string(std::to_string(x))" << std::endl;
    std::cout << "3. For concatenation, use separate std::to_string calls and join with +" << std::endl;
    std::cout << "4. Consider using utilities like those in nested_to_string_utils.h" << std::endl;
    
    return 0;
}