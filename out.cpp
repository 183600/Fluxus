#include <iostream>
#include <string>
#include <vector>
#include <optional>
#include <unordered_map>
#include <tuple>

using namespace std;

int add(int a, int b) {
    return a + b;
}


int main() {
    auto x = 42;
    auto y = 3.14;
    auto z = "hello";
    auto result = add(x, 10);
    std::cout << result << std::endl;
    return 0;
}

