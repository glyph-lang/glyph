#include <iostream>
#include <vector>

struct Point {
    int32_t x;
    int32_t y;

    int32_t squared_mag() const {
        return x * x + y * y;
    }
};

int32_t filter_and_sum(const std::vector<Point>& points, int32_t max_mag) {
    int32_t total = 0;
    for (const auto& point : points) {
        if (point.squared_mag() <= max_mag) {
            total += point.squared_mag();
        }
    }
    return total;
}

int main() {
    std::vector<Point> points = {{1, 2}, {3, 4}, {0, 1}};
    int32_t max_mag = 10;
    int32_t result = filter_and_sum(points, max_mag);
    std::cout << result << std::endl; // Output: 6
    return 0;
}
