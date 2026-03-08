def count_words(text):
    words = text.lower().split()
    word_count = {}
    for word in words:
        word_count[word] = word_count.get(word, 0) + 1
    return word_count

def print_top(word_count, n):
    sorted_words = sorted(word_count.items(), key=lambda item: (-item[1], item[0]))
    for count, word in sorted_words[:n]:
        print(f"  {count} {word}")

def main():
    text = "the cat sat on the mat the cat sat"
    word_count = count_words(text)
    print_top(word_count, 5)

main()
