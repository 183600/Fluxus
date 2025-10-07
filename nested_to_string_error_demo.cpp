// This file demonstrates the nested std::to_string error and how to fix it
#include <iostream>
#include <string>

int main() {
    int value1 = 123;
    double value2 = 45.67;
    
    // CORRECT usage of std::to_string
    std::cout << "Correct: " + std::to_string(value1) << std::endl;
    std::cout << "Correct: " + std::to_string(value2) << std::endl;
    
    // The following would cause a compilation error:
    // std::cout << "Error: " + std::to_string(std::to_string(value1)) << std::endl;
    // Error: no matching function for call to 'to_string' with std::string argument
    
    // If you need to concatenate multiple converted values, do it like this:
    std::string result = "The values are: " + std::to_string(value1) + " and " + std::to_string(value2);
    std::cout << result << std::endl;
    
    // Or using string streams for complex concatenation:
    std::string combined = "Value1: " + std::to_string(value1) + ", Value2: " + std::to_string(value2);
    std::cout << combined << std::endl;
    
    return 0;
}