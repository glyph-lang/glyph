```cpp
#include <vector>

int sum_positive(const std::vector<int>& nums) {
    std::vector<int> positive_nums;
    for (int num : nums) {
        if (num > 0) {
            positive_nums.push_back(num);
        }
    }
    
    int sum = 0;
    for (int pos_num : positive_nums) {
        sum += pos_num;
    }
    
    return sum;
}
```