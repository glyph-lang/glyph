#include <stdint.h>

typedef struct {
    int ok;
    int32_t value;
    const char *error;
} Result;

Result safe_divide(int32_t numerator, int32_t denominator) {
    Result res;
    if (denominator == 0) {
        res.ok = 0;
        res.value = 0;
        res.error = "division by zero";
    } else {
        res.ok = 1;
        res.value = numerator / denominator;
        res.error = 0;
    }
    return res;
}