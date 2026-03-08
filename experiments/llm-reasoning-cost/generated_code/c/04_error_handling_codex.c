#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

typedef struct {
    bool is_ok;
    int32_t value;
    const char *error;
} ResultInt32;

ResultInt32 safe_divide(int32_t numerator, int32_t denominator) {
    if (denominator == 0) {
        return (ResultInt32){ .is_ok = false, .value = 0, .error = "division by zero" };
    }
    return (ResultInt32){ .is_ok = true, .value = numerator / denominator, .error = NULL };
}