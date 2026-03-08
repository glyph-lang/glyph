# Task: Word Frequency Counter

Write code that counts word frequencies in a string and prints the results sorted by count.

**Requirements:**

1. Define a function `count_words`:
   - Parameter: a string of text
   - Return type: a map/dictionary from string to 32-bit signed integer
   - Split the input on whitespace into words
   - Convert each word to lowercase
   - Count occurrences of each word in a map/dictionary
   - Return the map

2. Define a function `print_top`:
   - Parameters: the word-count map and `n` (32-bit signed integer, max results to print)
   - Collect the map entries, sort them by count descending; break ties alphabetically ascending by word
   - Print the top `n` entries, one per line, in the format: `  <count> <word>` (two leading spaces)

3. In `main`, call `count_words` on this exact string:
   `"the cat sat on the mat the cat sat"`
   Then call `print_top` with `n = 5`.

**Expected output:**
```
  3 the
  2 cat
  2 sat
  1 mat
  1 on
```

**Implementation notes:**
- Use the language's built-in map/dictionary type (HashMap, dict, Map, etc.)
- Use standard library string splitting and sorting
- Case conversion: just lowercase the whole input (no punctuation handling needed)
