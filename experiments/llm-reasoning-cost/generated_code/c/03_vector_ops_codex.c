```c
#include <stdint.h>
#include <stdlib.h>

int32_t sum_positive(const int32_t *arr, size_t len) {
    int32_t *vec = NULL;
    size_t size = 0, capacity = 0;

    for (size_t i = 0; i < len; i++) {
        if (arr[i] > 0) {
            if (size == capacity) {
                capacity = capacity ? capacity * 2 : 4;
                int32_t *tmp = (int32_t *)realloc(vec, capacity * sizeof(int32_t));
                if (!tmp) {
                    free(vec);
                    return 0;
                }
                vec = tmp;
            }
            vec[size++] = arr[i];
        }
    }

    int32_t sum = 0;
    for (size_t i = 0; i < size; i++) {
        sum += vec[i];
    }

    free(vec);
    return sum;
}
```