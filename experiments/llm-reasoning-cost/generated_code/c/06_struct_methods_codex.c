#include <stdint.h>
#include <stddef.h>

typedef struct {
    int32_t x;
    int32_t y;
} Point;

int32_t point_squared_mag(Point p) {
    return p.x * p.x + p.y * p.y;
}

int32_t filter_and_sum(const Point *points, size_t count, int32_t max_mag) {
    int32_t total = 0;
    for (size_t i = 0; i < count; ++i) {
        int32_t mag = point_squared_mag(points[i]);
        if (mag <= max_mag) {
            total += mag;
        }
    }
    return total;
}