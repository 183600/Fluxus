#include <iostream>
#include <string>

int main() {
    int diff = 42;
    
    // The correct way to do it:
    std::cout << "Difference: " + std::to_string(diff) << std::endl;
    
    // Additional example of correct usage:
    std::cout << "Value: " + std::to_string(123) << std::endl;
    
    return 0;
}