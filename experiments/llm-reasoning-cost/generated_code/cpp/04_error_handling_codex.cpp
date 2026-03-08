#include <cstdint>
#include <string>
#include <variant>

using DivideResult = std::variant<int32_t, std::string>;

DivideResult safe_divide(int32_t numerator, int32_t denominator) {
    if (denominator == 0) {
        return std::string("division by zero");
    }
    return numerator / denominator;
}