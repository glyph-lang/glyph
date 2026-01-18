```cpp
#include <iostream>
#include <optional>
#include <string>
#include <variant>

using Result = std::variant<int, std::string>;

Result safe_divide(int numerator, int denominator) {
    if (denominator == 0) {
        return "division by zero";
    }
    return numerator / denominator;
}

int main() {
    auto result1 = safe_divide(10, 2);
    auto result2 = safe_divide(7, 3);
    auto result3 = safe_divide(5, 0);

    if (std::holds_alternative<int>(result1)) {
        std::cout << "Result 1: " << std::get<int>(result1) << std::endl;
    } else {
        std::cout << "Error 1: " << std::get<std::string>(result1) << std::endl;
    }

    if (std::holds_alternative<int>(result2)) {
        std::cout << "Result 2: " << std::get<int>(result2) << std::endl;
    } else {
        std::cout << "Error 2: " << std::get<std::string>(result2) << std::endl;
    }

    if (std::holds_alternative<int>(result3)) {
        std::cout << "Result 3: " << std::get<int>(result3) << std::endl;
    } else {
        std::cout << "Error 3: " << std::get<std::string>(result3) << std::endl;
    }

    return 0;
}
```