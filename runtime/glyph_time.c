#include <stdint.h>
#include <time.h>
#include <stdio.h>

uint64_t glyph_time_now(void) {
    time_t t = time(NULL);
    if (t == (time_t)-1) {
        return 0;
    }
    return (uint64_t)t;
}

const char* glyph_time_to_human_readable(uint64_t ts) {
    static char buffer[20];
    buffer[0] = '\0';

    time_t t = (time_t)ts;
    if ((uint64_t)t != ts) {
        return buffer;
    }

    struct tm tm_buf;
    struct tm* tm_ptr = NULL;

#if defined(_WIN32)
    if (gmtime_s(&tm_buf, &t) != 0) {
        return buffer;
    }
    tm_ptr = &tm_buf;
#else
    tm_ptr = gmtime_r(&t, &tm_buf);
    if (tm_ptr == NULL) {
        return buffer;
    }
#endif

    int len = snprintf(
        buffer,
        sizeof(buffer),
        "%02d/%02d/%04d %02d:%02d:%02d",
        tm_ptr->tm_mday,
        tm_ptr->tm_mon + 1,
        tm_ptr->tm_year + 1900,
        tm_ptr->tm_hour,
        tm_ptr->tm_min,
        tm_ptr->tm_sec
    );

    if (len != 19) {
        buffer[0] = '\0';
        return buffer;
    }

    return buffer;
}
