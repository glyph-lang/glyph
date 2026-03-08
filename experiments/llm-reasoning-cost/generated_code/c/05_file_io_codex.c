#include <stdio.h>
#include <errno.h>

int write_greeting(const char *filename) {
    FILE *file = fopen(filename, "w");
    if (file == NULL) {
        return errno ? errno : -1;
    }

    const char *text = "Hello from the program!";
    if (fputs(text, file) == EOF) {
        int write_err = ferror(file) ? errno : -1;
        fclose(file);
        return write_err;
    }

    if (fclose(file) != 0) {
        return errno ? errno : -1;
    }

    return 0;
}