#include <stdio.h>
#include <errno.h>

int write_greeting(const char *filename) {
    FILE *f = fopen(filename, "w");
    if (!f) return -1;

    int ret = 0;
    if (fputs("Hello from the program!", f) == EOF) {
        ret = -1;
    }

    if (fclose(f) == EOF) {
        ret = -1;
    }

    return ret;
}