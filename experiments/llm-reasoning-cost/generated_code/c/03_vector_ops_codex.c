#include <stdint.h>
#include <stdlib.h>

int32_t sum_positive(const int32_t *input, size_t length) {
    int32_t *positives = (int32_t *)malloc(length * sizeof(int32_t));
    if (positives == NULL && length > 0) {
        return 0;
    }

    size_t pos_count = 0;
    for (size_t i = 0; i < length; ++i) {
        if (input[i] > 0) {
            positives[pos_count++] = input[i]; // push/append
        }
    }

    int32_t sum = 0;
    for (size_t i = 0; i < pos_count; ++i) {
        sum += positives[i];
    }

    free(positives);
    return sum;
}