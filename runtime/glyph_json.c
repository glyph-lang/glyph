#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

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

int32_t glyph_f64_to_i32(double value) {
    return (int32_t)value;
}

char* glyph_string_from_char(int32_t value) {
    uint8_t byte = (uint8_t)value;
    return glyph_string_from_byte(byte);
}

char* glyph_string_from_i32(int32_t value) {
    char buffer[12];
    int len = snprintf(buffer, sizeof(buffer), "%d", value);
    if (len < 0) {
        return NULL;
    }
    char* out = (char*)malloc((size_t)len + 1);
    if (!out) {
        return NULL;
    }
    memcpy(out, buffer, (size_t)len + 1);
    return out;
}

int32_t glyph_string_char_at(const char* s, size_t index) {
    if (s == NULL) {
        return 0;
    }
    size_t len = strlen(s);
    if (index >= len) {
        return 0;
    }
    return (unsigned char)s[index];
}

int64_t glyph_string_index_of(const char* haystack, const char* needle) {
    if (haystack == NULL || needle == NULL) {
        return -1;
    }
    if (needle[0] == '\0') {
        return 0;
    }
    const char* found = strstr(haystack, needle);
    if (found == NULL) {
        return -1;
    }
    return (int64_t)(found - haystack);
}
