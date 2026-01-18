```cpp
#include <iostream>
#include <fstream>
#include <string>
#include <system_error>

enum class Result {
    Success,
    Error
};

Result write_greeting(const std::string& filename) {
    std::ofstream file;
    file.open(filename);
    
    if (!file.is_open()) {
        return Result::Error;
    }

    file << "Hello from the program!";
    
    if (file.fail()) {
        file.close();
        return Result::Error;
    }

    file.close();
    return Result::Success;
}
```