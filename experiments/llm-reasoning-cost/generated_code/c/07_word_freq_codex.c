#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct {
    char *word;
    int count;
} Entry;

typedef struct {
    Entry *data;
    int size;
    int capacity;
} WordMap;

static char *str_dup(const char *s) {
    size_t len = strlen(s);
    char *copy = (char *)malloc(len + 1);
    if (!copy) exit(1);
    memcpy(copy, s, len + 1);
    return copy;
}

static void map_init(WordMap *m) {
    m->size = 0;
    m->capacity = 8;
    m->data = (Entry *)malloc((size_t)m->capacity * sizeof(Entry));
    if (!m->data) exit(1);
}

static void map_free(WordMap *m) {
    for (int i = 0; i < m->size; i++) {
        free(m->data[i].word);
    }
    free(m->data);
    m->data = NULL;
    m->size = 0;
    m->capacity = 0;
}

static void map_grow(WordMap *m) {
    m->capacity *= 2;
    Entry *new_data = (Entry *)realloc(m->data, (size_t)m->capacity * sizeof(Entry));
    if (!new_data) exit(1);
    m->data = new_data;
}

static void map_increment(WordMap *m, const char *word) {
    for (int i = 0; i < m->size; i++) {
        if (strcmp(m->data[i].word, word) == 0) {
            m->data[i].count++;
            return;
        }
    }
    if (m->size == m->capacity) {
        map_grow(m);
    }
    m->data[m->size].word = str_dup(word);
    m->data[m->size].count = 1;
    m->size++;
}

WordMap count_words(const char *text) {
    WordMap map;
    map_init(&map);

    char *buf = str_dup(text);
    for (char *p = buf; *p; p++) {
        *p = (char)tolower((unsigned char)*p);
    }

    const char *delim = " \t\r\n";
    char *tok = strtok(buf, delim);
    while (tok) {
        map_increment(&map, tok);
        tok = strtok(NULL, delim);
    }

    free(buf);
    return map;
}

static int cmp_entries(const void *a, const void *b) {
    const Entry *ea = (const Entry *)a;
    const Entry *eb = (const Entry *)b;
    if (ea->count != eb->count) {
        return eb->count - ea->count;
    }
    return strcmp(ea->word, eb->word);
}

void print_top(const WordMap *map, int n) {
    if (n <= 0 || map->size == 0) return;

    Entry *arr = (Entry *)malloc((size_t)map->size * sizeof(Entry));
    if (!arr) exit(1);
    for (int i = 0; i < map->size; i++) {
        arr[i] = map->data[i];
    }

    qsort(arr, (size_t)map->size, sizeof(Entry), cmp_entries);

    int limit = n < map->size ? n : map->size;
    for (int i = 0; i < limit; i++) {
        printf("  %d %s\n", arr[i].count, arr[i].word);
    }

    free(arr);
}

int main(void) {
    const char *text = "the cat sat on the mat the cat sat";
    WordMap counts = count_words(text);
    print_top(&counts, 5);
    map_free(&counts);
    return 0;
}
