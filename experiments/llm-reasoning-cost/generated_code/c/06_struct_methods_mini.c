#include <stdio.h>

typedef struct {
    int x;
    int y;
} Point;

int squared_mag(Point p) {
    return p.x * p.x + p.y * p.y;
}

int filter_and_sum(Point points[], int size, int max_mag) {
    int total = 0;
    for (int i = 0; i < size; i++) {
        if (squared_mag(points[i]) <= max_mag) {
            total += squared_mag(points[i]);
        }
    }
    return total;
}

int main() {
    Point points[] = {{1, 2}, {3, 4}, {0, 1}};
    int max_mag = 10;
    int result = filter_and_sum(points, 3, max_mag);
    printf("Total: %d\n", result);
    return 0;
}
