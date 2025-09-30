#include <iostream>
#include <string>

int main() {
    int diff = 42;
    
    // Fixed: Removed the nested to_string call
    std::cout << "Difference: " + std::to_string(diff) << std::endl;
    
    return 0;
}