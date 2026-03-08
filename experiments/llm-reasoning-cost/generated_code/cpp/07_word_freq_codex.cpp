#include <algorithm>
#include <cstdint>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

std::unordered_map<std::string, int32_t> count_words(const std::string& text) {
    std::unordered_map<std::string, int32_t> counts;
    std::istringstream iss(text);
    std::string word;

    while (iss >> word) {
        std::transform(word.begin(), word.end(), word.begin(),
                       [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
        ++counts[word];
    }

    return counts;
}

void print_top(const std::unordered_map<std::string, int32_t>& counts, int32_t n) {
    std::vector<std::pair<std::string, int32_t>> entries;
    entries.reserve(counts.size());

    for (const auto& kv : counts) {
        entries.push_back(kv);
    }

    std::sort(entries.begin(), entries.end(),
              [](const auto& a, const auto& b) {
                  if (a.second != b.second) return a.second > b.second;
                  return a.first < b.first;
              });

    int32_t limit = std::min<int32_t>(n, static_cast<int32_t>(entries.size()));
    for (int32_t i = 0; i < limit; ++i) {
        std::cout << "  " << entries[i].second << ' ' << entries[i].first << '\n';
    }
}

int main() {
    std::string text = "the cat sat on the mat the cat sat";
    auto counts = count_words(text);
    print_top(counts, 5);
    return 0;
}
