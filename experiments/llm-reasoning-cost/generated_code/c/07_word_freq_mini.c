#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_WORD_LENGTH 100
#define MAX_WORDS 1000

typedef struct {
    char word[MAX_WORD_LENGTH];
    int count;
} WordCount;

int compare(const void *a, const void *b) {
    WordCount *wc1 = (WordCount *)a;
    WordCount *wc2 = (WordCount *)b;
    if (wc1->count != wc2->count) {
        return wc2->count - wc1->count; // Descending order by count
    }
    return strcmp(wc1->word, wc2->word); // Ascending order by word
}

void count_words(const char *text, WordCount *wordCounts, int *wordCountSize) {
    char *textCopy = strdup(text);
    char *token = strtok(textCopy, " ");
    int size = 0;

    while (token != NULL) {
        // Convert to lowercase
        for (char *p = token; *p; ++p) {
            *p = tolower(*p);
        }

        // Check if the word is already in the list
        int found = 0;
        for (int i = 0; i < size; i++) {
            if (strcmp(wordCounts[i].word, token) == 0) {
                wordCounts[i].count++;
                found = 1;
                break;
            }
        }

        // If not found, add it to the list
        if (!found) {
            strcpy(wordCounts[size].word, token);
            wordCounts[size].count = 1;
            size++;
        }

        token = strtok(NULL, " ");
    }

    free(textCopy);
    *wordCountSize = size;
}

void print_top(WordCount *wordCounts, int wordCountSize, int n) {
    qsort(wordCounts, wordCountSize, sizeof(WordCount), compare);
    for (int i = 0; i < n && i < wordCountSize; i++) {
        printf("  %d %s\n", wordCounts[i].count, wordCounts[i].word);
    }
}

int main() {
    const char *text = "the cat sat on the mat the cat sat";
    WordCount wordCounts[MAX_WORDS];
    int wordCountSize;

    count_words(text, wordCounts, &wordCountSize);
    print_top(wordCounts, wordCountSize, 5);

    return 0;
}
