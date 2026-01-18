```c
#include <stdio.h>

int sum_positive(int *vec, int size) {
    int sum = 0;
    int new_vec[size]; // Create a new vector with the same size
    int new_size = 0;  // To keep track of the number of positive numbers

    for (int i = 0; i < size; i++) {
        if (vec[i] > 0) {
            new_vec[new_size] = vec[i]; // Add positive number to new vector
            new_size++;
        }
    }

    // Calculate the sum of the new vector
    for (int i = 0; i < new_size; i++) {
        sum += new_vec[i];
    }

    return sum;
}

int main() {
    int vec[] = {1, -2, 3, -4, 5};
    int size = sizeof(vec) / sizeof(vec[0]);
    int result = sum_positive(vec, size);
    printf("Sum of positive numbers: %d\n", result);
    return 0;
}
```