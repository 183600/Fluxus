/*
 * This file demonstrates how to fix the "no matching function for call to 'to_string'" error
 * when using nested std::to_string calls like std::to_string(std::to_string(sum_result))
 */

#include <iostream>
#include <string>

int main() {
    int sum_result = 42;
    
    // INCORRECT: This causes the compilation error because std::to_string(sum_result)
    // returns a std::string, and std::to_string doesn't accept std::string as an argument
    // std::string error = std::to_string(std::to_string(sum_result));  // Error: no matching function
    
    // CORRECT: Remove the outer std::to_string call
    std::string correct = std::to_string(sum_result);
    std::cout << "Correct usage: " << correct << std::endl;
    
    // If you need to concatenate with other strings, do it like this:
    std::string concatenated_result = "The result is: " + std::to_string(sum_result);
    std::cout << concatenated_result << std::endl;
    
    // For multiple numeric conversions in a string:
    int num1 = 10, num2 = 20, sum = num1 + num2;
    std::string multiple = "Sum of " + std::to_string(num1) + " and " + std::to_string(num2) + " is " + std::to_string(sum);
    std::cout << multiple << std::endl;
    
    return 0;
}