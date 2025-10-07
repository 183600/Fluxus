// This file demonstrates what the actual compilation error looks like
// To see the error, uncomment the problematic line below and try to compile

#include <iostream>
#include <string>

int main() {
    int value = 123;
    
    // This works fine:
    std::cout << "Working example: " << std::to_string(value) << std::endl;
    
    // This would generate the error:
    // std::cout << "Error case: " << std::to_string(std::to_string(value)) << std::endl;
    // Error message would be something like:
    // error: no matching function for call to 'to_string'
    // note: candidate expects double, long etc., not std::string
    
    // Instead, use string concatenation:
    std::string correct = "Value: " + std::to_string(value);
    std::cout << correct << std::endl;
    
    return 0;
}