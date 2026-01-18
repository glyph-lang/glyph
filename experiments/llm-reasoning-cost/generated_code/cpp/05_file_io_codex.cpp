#include <fstream>
#include <system_error>
#include <string>
#include <cerrno>

std::error_code write_greeting(const std::string& filename) {
    std::ofstream file(filename, std::ios::out | std::ios::trunc);
    if (!file.is_open()) {
        return std::error_code(errno, std::generic_category());
    }
    file << "Hello from the program!";
    if (!file) {
        return std::error_code(errno, std::generic_category());
    }
    file.close();
    if (!file) {
        return std::error_code(errno, std::generic_category());
    }
    return {};
}