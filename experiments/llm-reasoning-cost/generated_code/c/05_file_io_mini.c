#include <stdio.h>
#include <stdlib.h>

typedef enum {
    SUCCESS,
    FILE_ERROR
} Result;

Result write_greeting(const char *filename) {
    FILE *file = fopen(filename, "w");
    if (file == NULL) {
        return FILE_ERROR;
    }

    if (fputs("Hello from the program!\n", file) == EOF) {
        fclose(file);
        return FILE_ERROR;
    }

    if (fclose(file) != 0) {
        return FILE_ERROR;
    }

    return SUCCESS;
}
