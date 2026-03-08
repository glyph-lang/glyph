from typing import Dict


def count_words(text: str) -> Dict[str, int]:
    counts: Dict[str, int] = {}
    for word in text.lower().split():
        counts[word] = counts.get(word, 0) + 1
    return counts


def print_top(word_counts: Dict[str, int], n: int) -> None:
    items = sorted(word_counts.items(), key=lambda kv: (-kv[1], kv[0]))
    for word, count in items[:n]:
        print(f"  {count} {word}")


def main() -> None:
    text = "the cat sat on the mat the cat sat"
    counts = count_words(text)
    print_top(counts, 5)


if __name__ == "__main__":
    main()
