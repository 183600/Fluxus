#include <iostream>
#include <string>

int main() {
    int diff = 42;
    
    // The correct way - no more nested to_string calls:
    std::cout << "Difference: " + std::to_string(diff) << std::endl;
    
    // Another example:
    std::cout << "Value: " + std::to_string(123) << std::endl;
    
    // More examples of correct usage:
    float value = 3.14159f;
    std::cout << "Float value: " + std::to_string(value) << std::endl;
    
    double large_number = 1234567.89;
    std::cout << "Large number: " + std::to_string(large_number) << std::endl;
    
    long long big_int = 123456789012345LL;
    std::cout << "Big integer: " + std::to_string(big_int) << std::endl;
    
    return 0;
}