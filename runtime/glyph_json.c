#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Safe byte access with bounds checking
uint8_t glyph_byte_at(const char* s, size_t index) {
    size_t len = strlen(s);
    if (index >= len) {
        return 0;  // Out of bounds, return null byte
    }
    return (uint8_t)s[index];
}

char* glyph_string_from_byte(uint8_t byte) {
    if (byte == 0) {
        return NULL;
    }
    char* buf = (char*)malloc(2);
    if (!buf) {
        return NULL;
    }
    buf[0] = (char)byte;
    buf[1] = '\0';
    return buf;
}
