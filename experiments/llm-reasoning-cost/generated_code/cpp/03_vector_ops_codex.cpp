#include <vector>
#include <cstdint>

int32_t sum_positive(const std::vector<int32_t>& nums) {
    std::vector<int32_t> positives;

    for (size_t i = 0; i < nums.size(); ++i) {
        if (nums[i] > 0) {
            positives.push_back(nums[i]);
        }
    }

    int32_t sum = 0;
    for (size_t i = 0; i < positives.size(); ++i) {
        sum += positives[i];
    }

    return sum;
}
