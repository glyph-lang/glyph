#include <fstream>
#include <string>

enum class WriteError {
    None,
    OpenFailed,
    WriteFailed,
    CloseFailed
};

struct WriteResult {
    bool ok;
    WriteError error;
};

WriteResult write_greeting(const std::string& filename) {
    std::ofstream out(filename, std::ios::out | std::ios::trunc);
    if (!out.is_open()) {
        return {false, WriteError::OpenFailed};
    }

    out << "Hello from the program!";
    if (!out) {
        return {false, WriteError::WriteFailed};
    }

    out.close();
    if (!out) {
        return {false, WriteError::CloseFailed};
    }

    return {true, WriteError::None};
}
