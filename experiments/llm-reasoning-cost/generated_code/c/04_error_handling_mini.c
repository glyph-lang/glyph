```c
#include <stdio.h>

typedef struct {
    int quotient;
    const char* error;
} Result;

Result safe_divide(int numerator, int denominator) {
    Result result;
    if (denominator == 0) {
        result.error = "division by zero";
        return result;
    }
    result.quotient = numerator / denominator;
    result.error = NULL;
    return result;
}

int main() {
    Result res1 = safe_divide(10, 2);
    if (res1.error) {
        printf("Error: %s\n", res1.error);
    } else {
        printf("Result: %d\n", res1.quotient);
    }

    Result res2 = safe_divide(7, 3);
    if (res2.error) {
        printf("Error: %s\n", res2.error);
    } else {
        printf("Result: %d\n", res2.quotient);
    }

    Result res3 = safe_divide(5, 0);
    if (res3.error) {
        printf("Error: %s\n", res3.error);
    } else {
        printf("Result: %d\n", res3.quotient);
    }

    return 0;
}
```