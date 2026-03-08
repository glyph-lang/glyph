#include <iostream>
#include <sstream>
#include <map>
#include <vector>
#include <algorithm>

std::map<std::string, int> count_words(const std::string& text) {
    std::map<std::string, int> word_count;
    std::istringstream stream(text);
    std::string word;

    while (stream >> word) {
        // Convert to lowercase
        std::transform(word.begin(), word.end(), word.begin(), ::tolower);
        word_count[word]++;
    }

    return word_count;
}

void print_top(const std::map<std::string, int>& word_count, int n) {
    std::vector<std::pair<std::string, int>> sorted_words(word_count.begin(), word_count.end());

    // Sort by count descending, then by word ascending
    std::sort(sorted_words.begin(), sorted_words.end(), [](const auto& a, const auto& b) {
        if (a.second == b.second) {
            return a.first < b.first;
        }
        return a.second > b.second;
    });

    for (int i = 0; i < n && i < sorted_words.size(); ++i) {
        std::cout << "  " << sorted_words[i].second << " " << sorted_words[i].first << std::endl;
    }
}

int main() {
    std::string text = "the cat sat on the mat the cat sat";
    auto word_count = count_words(text);
    print_top(word_count, 5);
    return 0;
}
