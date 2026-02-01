// Glyph process runtime helpers
//
// Provides a minimal external process launching API for Glyph stdlib:
// - glyph_process_run(cmd, args) -> i32

#include <stdint.h>

typedef struct {
    void* data;
    int64_t len;
    int64_t cap;
} GlyphVec;

#if defined(__APPLE__) || defined(__linux__)

#include <errno.h>
#include <spawn.h>
#include <stdlib.h>
#include <sys/wait.h>

extern char** environ;

int32_t glyph_process_run(const char* cmd, const GlyphVec* args) {
    if (cmd == NULL) {
        return -EINVAL;
    }

    // args->data points to an array of `char*`.
    char** arg_ptrs = (char**)args->data;

    // argv layout: [cmd] + args + [NULL]
    int64_t argc_extra = args->len;
    if (argc_extra < 0) {
        return -EINVAL;
    }

    size_t argv_len = (size_t)argc_extra + 2; // cmd + NULL
    char** argv = (char**)malloc(sizeof(char*) * argv_len);
    if (argv == NULL) {
        return -ENOMEM;
    }

    argv[0] = (char*)cmd;
    for (int64_t i = 0; i < args->len; i++) {
        argv[(size_t)i + 1] = arg_ptrs[i];
    }
    argv[argv_len - 1] = NULL;

    pid_t pid;
    int spawn_err = posix_spawnp(&pid, cmd, NULL, NULL, argv, environ);
    free(argv);

    if (spawn_err != 0) {
        // posix_spawnp returns an errno-style error code.
        return -(int32_t)spawn_err;
    }

    int status = 0;
    if (waitpid(pid, &status, 0) < 0) {
        return -(int32_t)errno;
    }

    if (WIFEXITED(status)) {
        return (int32_t)WEXITSTATUS(status);
    }
    if (WIFSIGNALED(status)) {
        return (int32_t)(128 + WTERMSIG(status));
    }

    return -EINVAL;
}

#else

#include <errno.h>

int32_t glyph_process_run(const char* cmd, const GlyphVec* args) {
    (void)cmd;
    (void)args;
    return -ENOSYS;
}

#endif
