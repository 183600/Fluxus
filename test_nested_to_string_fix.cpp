// Test file to verify nested std::to_string issue is fixed
#include <iostream>
#include <string>

int main() {
    int product = 120;
    
    // The correct implementation - this should compile and work
    std::cout << "Product: " + std::to_string(product) << std::endl;
    
    // Additional test cases
    int x = 15;
    int y = 25;
    int sum = x + y;
    
    std::cout << "Sum: " + std::to_string(sum) << std::endl;
    
    return 0;
}