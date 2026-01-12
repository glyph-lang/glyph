// Glyph Formatting Runtime Library
// These functions provide formatting support for Glyph's print/formatting system.
// Link against this library when building executables that use std::print or std::fmt.

#include <unistd.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

// Helper to convert i32 to decimal string and write to fd
// Returns number of bytes written, or -1 on error
int glyph_fmt_write_i32(int fd, int32_t value) {
    char buffer[12];  // Enough for -2147483648 + null terminator
    int len;

    // Handle negative numbers
    if (value < 0) {
        buffer[0] = '-';
        // Handle INT_MIN specially to avoid overflow
        if (value == INT32_MIN) {
            return write(fd, "-2147483648", 11);
        }
        value = -value;
        len = 1;
    } else {
        len = 0;
    }

    // Convert to string in reverse
    char temp[11];
    int temp_len = 0;

    if (value == 0) {
        temp[temp_len++] = '0';
    } else {
        while (value > 0) {
            temp[temp_len++] = '0' + (value % 10);
            value /= 10;
        }
    }

    // Reverse into buffer
    for (int i = 0; i < temp_len; i++) {
        buffer[len++] = temp[temp_len - 1 - i];
    }

    return write(fd, buffer, len);
}

// Helper to convert unsigned 32-bit integer to decimal
// Returns number of bytes written, or -1 on error
int glyph_fmt_write_u32(int fd, uint32_t value) {
    char buffer[11];  // Max 10 digits for u32
    char temp[10];
    int temp_len = 0;

    if (value == 0) {
        buffer[0] = '0';
        return write(fd, buffer, 1);
    }

    while (value > 0) {
        temp[temp_len++] = '0' + (value % 10);
        value /= 10;
    }

    for (int i = 0; i < temp_len; i++) {
        buffer[i] = temp[temp_len - 1 - i];
    }

    return write(fd, buffer, temp_len);
}

// Helper to convert i64 to decimal string and write to fd
// Returns number of bytes written, or -1 on error
int glyph_fmt_write_i64(int fd, int64_t value) {
    char buffer[21];  // Enough for -9223372036854775808
    int len;

    if (value < 0) {
        buffer[0] = '-';
        if (value == INT64_MIN) {
            return write(fd, "-9223372036854775808", 20);
        }
        value = -value;
        len = 1;
    } else {
        len = 0;
    }

    char temp[20];
    int temp_len = 0;

    if (value == 0) {
        temp[temp_len++] = '0';
    } else {
        while (value > 0) {
            temp[temp_len++] = '0' + (value % 10);
            value /= 10;
        }
    }

    for (int i = 0; i < temp_len; i++) {
        buffer[len++] = temp[temp_len - 1 - i];
    }

    return write(fd, buffer, len);
}

// Helper to convert unsigned 64-bit integer to decimal
// Returns number of bytes written, or -1 on error
int glyph_fmt_write_u64(int fd, uint64_t value) {
    char buffer[21];  // Max 20 digits for u64
    char temp[20];
    int temp_len = 0;

    if (value == 0) {
        buffer[0] = '0';
        return write(fd, buffer, 1);
    }

    while (value > 0) {
        temp[temp_len++] = '0' + (value % 10);
        value /= 10;
    }

    for (int i = 0; i < temp_len; i++) {
        buffer[i] = temp[temp_len - 1 - i];
    }

    return write(fd, buffer, temp_len);
}

// Write "true" or "false" to fd based on boolean value
// Returns number of bytes written, or -1 on error
int glyph_fmt_write_bool(int fd, bool value) {
    if (value) {
        return write(fd, "true", 4);
    } else {
        return write(fd, "false", 5);
    }
}

// Write a null-terminated string to fd
// Returns number of bytes written, or -1 on error
int glyph_fmt_write_str(int fd, const char* str) {
    if (!str) {
        return 0;
    }
    size_t len = strlen(str);
    return write(fd, str, len);
}

// Write a single Unicode scalar value encoded as UTF-8
// Returns number of bytes written, or -1 on error
int glyph_fmt_write_char(int fd, uint32_t value) {
    unsigned char buf[4];
    size_t len = 0;

    if (value <= 0x7F) {
        buf[0] = (unsigned char)value;
        len = 1;
    } else if (value <= 0x7FF) {
        buf[0] = 0xC0 | (value >> 6);
        buf[1] = 0x80 | (value & 0x3F);
        len = 2;
    } else if (value <= 0xFFFF) {
        buf[0] = 0xE0 | (value >> 12);
        buf[1] = 0x80 | ((value >> 6) & 0x3F);
        buf[2] = 0x80 | (value & 0x3F);
        len = 3;
    } else if (value <= 0x10FFFF) {
        buf[0] = 0xF0 | (value >> 18);
        buf[1] = 0x80 | ((value >> 12) & 0x3F);
        buf[2] = 0x80 | ((value >> 6) & 0x3F);
        buf[3] = 0x80 | (value & 0x3F);
        len = 4;
    } else {
        // Invalid codepoint; write replacement character '?'
        buf[0] = '?';
        len = 1;
    }

    return write(fd, buf, len);
}
