// This file demonstrates the nested std::to_string error that was fixed
#include <iostream>
#include <string>

int main() {
    int product = 120;
    
    // This is the CORRECT implementation
    std::cout << "Correct: Product is " + std::to_string(product) << std::endl;
    
    // Explanation of the error:
    // The error occurs because std::to_string returns a std::string, but std::to_string 
    // expects a numeric type (int, float, double, etc.), not a std::string.
    
    return 0;
}