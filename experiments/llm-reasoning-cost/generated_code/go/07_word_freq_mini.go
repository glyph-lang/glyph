package main

import (
	"fmt"
	"sort"
	"strings"
)

func countWords(text string) map[string]int32 {
	wordCount := make(map[string]int32)
	words := strings.Fields(strings.ToLower(text))
	for _, word := range words {
		wordCount[word]++
	}
	return wordCount
}

func printTop(wordCount map[string]int32, n int32) {
	type wordCountPair struct {
		word  string
		count int32
	}
	var pairs []wordCountPair

	for word, count := range wordCount {
		pairs = append(pairs, wordCountPair{word, count})
	}

	sort.Slice(pairs, func(i, j int) bool {
		if pairs[i].count == pairs[j].count {
			return pairs[i].word < pairs[j].word
		}
		return pairs[i].count > pairs[j].count
	})

	for i := int32(0); i < n && i < int32(len(pairs)); i++ {
		fmt.Printf("  %d %s\n", pairs[i].count, pairs[i].word)
	}
}

func main() {
	text := "the cat sat on the mat the cat sat"
	wordCount := countWords(text)
	printTop(wordCount, 5)
}
