```c
#include <stdio.h>
#include <stdlib.h>

typedef enum {
    SUCCESS,
    ERROR
} Result;

Result write_greeting(const char *filename) {
    FILE *file = fopen(filename, "w");
    if (file == NULL) {
        return ERROR;
    }

    if (fputs("Hello from the program!\n", file) == EOF) {
        fclose(file);
        return ERROR;
    }

    if (fclose(file) != 0) {
        return ERROR;
    }

    return SUCCESS;
}
```