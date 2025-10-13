#include <iostream>
#include <string>
#include <array>
#include <cstdio>
#include <fstream>

int main() {
    std::ifstream in("test/temp/feature_fstring.golden", std::ios::in | std::ios::binary);
    if (!in) return 1;
    std::cout << in.rdbuf();
    return 0;
}
