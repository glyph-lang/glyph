#include <stdint.h>
#include <string.h>

// Safe byte access with bounds checking
uint8_t glyph_byte_at(const char* s, size_t index) {
    size_t len = strlen(s);
    if (index >= len) {
        return 0;  // Out of bounds, return null byte
    }
    return (uint8_t)s[index];
}
