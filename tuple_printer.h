#ifndef TUPLE_PRINTER_H
#define TUPLE_PRINTER_H

#include <tuple>
#include <iostream>
#include <string>

// Helper function to print a tuple
template<typename Tuple, std::size_t... Is>
void print_tuple_impl(std::ostream& os, const Tuple& tuple, std::index_sequence<Is...>) {
    ((os << (Is == 0 ? "" : ", ") << std::get<Is>(tuple)), ...);
}

template<typename... Args>
void print_tuple(std::ostream& os, const std::tuple<Args...>& tuple) {
    os << "(";
    print_tuple_impl(os, tuple, std::make_index_sequence<sizeof...(Args)>{});
    os << ")";
}

// Overload for make_tuple to allow direct printing
template<typename... Args>
std::ostream& operator<<(std::ostream& os, const std::tuple<Args...>& tuple) {
    print_tuple(os, tuple);
    return os;
}

#endif // TUPLE_PRINTER_H