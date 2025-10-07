#include <iostream>
namespace fluxus { auto add(auto a, auto b){return a+b;} }
int main(){ int x=1; std::cout<<fluxus::add(1,2); }