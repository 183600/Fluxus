/*
 * This file demonstrates the actual compilation error when using nested std::to_string calls
 * This file intentionally contains the error to show what the error looks like.
 * DO NOT try to compile this file as it will result in an error.
 */

#include <iostream>
#include <string>

int main() {
    int sum_result = 42;
    
    // INCORRECT: This causes the compilation error because std::to_string(sum_result)
    // returns a std::string, and std::to_string doesn't accept std::string as an argument
    // This line would cause the exact error mentioned:
    // "no matching function for call to 'to_string'"
    // std::string error = std::to_string(std::to_string(sum_result));
    
    // CORRECT: Just use std::to_string once for numeric values
    std::string correct = std::to_string(sum_result);
    std::cout << "Correct usage: " << correct << std::endl;
    
    return 0;
}