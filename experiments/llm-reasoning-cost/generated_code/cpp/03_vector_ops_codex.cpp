#include <cstdint>
#include <vector>

int32_t sum_positive(const std::vector<int32_t>& input) {
    std::vector<int32_t> positives;
    int32_t sum = 0;
    for (int32_t x : input) {
        if (x > 0) {
            positives.push_back(x);
            sum += x;
        }
    }
    return sum;
}