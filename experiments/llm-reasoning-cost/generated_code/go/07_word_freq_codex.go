package main

import (
	"fmt"
	"sort"
	"strings"
)

func count_words(text string) map[string]int32 {
	counts := make(map[string]int32)
	words := strings.Fields(strings.ToLower(text))
	for _, w := range words {
		counts[w]++
	}
	return counts
}

func print_top(counts map[string]int32, n int32) {
	type entry struct {
		word  string
		count int32
	}

	entries := make([]entry, 0, len(counts))
	for w, c := range counts {
		entries = append(entries, entry{word: w, count: c})
	}

	sort.Slice(entries, func(i, j int) bool {
		if entries[i].count != entries[j].count {
			return entries[i].count > entries[j].count
		}
		return entries[i].word < entries[j].word
	})

	limit := int(n)
	if limit > len(entries) {
		limit = len(entries)
	}
	for i := 0; i < limit; i++ {
		fmt.Printf("  %d %s\n", entries[i].count, entries[i].word)
	}
}

func main() {
	text := "the cat sat on the mat the cat sat"
	counts := count_words(text)
	print_top(counts, 5)
}
