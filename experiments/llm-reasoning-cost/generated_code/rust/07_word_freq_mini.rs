use std::collections::HashMap;

fn count_words(text: &str) -> HashMap<String, i32> {
    let mut word_count = HashMap::new();
    for word in text.to_lowercase().split_whitespace() {
        *word_count.entry(word.to_string()).or_insert(0) += 1;
    }
    word_count
}

fn print_top(word_count: &HashMap<String, i32>, n: i32) {
    let mut sorted_counts: Vec<_> = word_count.iter().collect();
    sorted_counts.sort_by(|a, b| {
        let count_cmp = b.1.cmp(a.1);
        if count_cmp == std::cmp::Ordering::Equal {
            a.0.cmp(b.0)
        } else {
            count_cmp
        }
    });

    for (word, count) in sorted_counts.iter().take(n as usize) {
        println!("  {} {}", count, word);
    }
}

fn main() {
    let text = "the cat sat on the mat the cat sat";
    let word_count = count_words(text);
    print_top(&word_count, 5);
}
