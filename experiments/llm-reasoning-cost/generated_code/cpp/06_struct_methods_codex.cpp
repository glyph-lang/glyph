#include <cstdint>
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
    for (const auto& p : points) {
        int32_t mag = p.squared_mag();
        if (mag <= max_mag) {
            total += mag;
        }
    }
    return total;
}