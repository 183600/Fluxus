#ifndef NESTED_TOSTRING_UTILS_H
#define NESTED_TOSTRING_UTILS_H

#include <string>
#include <sstream>
#include <type_traits>

// Utility functions to safely convert multiple values to string without nesting std::to_string calls

// For single values, just use std::to_string directly
template<typename T>
inline std::string safe_to_string(T&& value) {
    static_assert(std::is_arithmetic<typename std::decay<T>::type>::value, 
                  "safe_to_string should only be used with arithmetic types");
    return std::to_string(std::forward<T>(value));
}

// For two values
template<typename T1, typename T2>
inline std::string concat_to_string(T1&& v1, T2&& v2) {
    return std::to_string(v1) + std::to_string(v2);
}

// For three values
template<typename T1, typename T2, typename T3>
inline std::string concat_to_string(T1&& v1, T2&& v2, T3&& v3) {
    return std::to_string(v1) + std::to_string(v2) + std::to_string(v3);
}

// Generic function to convert multiple values to concatenated string
template<typename... Args>
inline std::string join_to_string(Args&&... args) {
    std::ostringstream oss;
    (oss << ... << std::to_string(args));  // C++17 fold expression
    return oss.str();
}

// For older C++ standards, a version with a maximum number of arguments
template<typename T1, typename T2, typename T3, typename T4>
inline std::string join_to_string4(T1&& v1, T2&& v2, T3&& v3, T4&& v4) {
    return std::to_string(v1) + " " + std::to_string(v2) + " " + 
           std::to_string(v3) + " " + std::to_string(v4);
}

#endif // NESTED_TOSTRING_UTILS_H