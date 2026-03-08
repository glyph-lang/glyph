use std::collections::HashMap;

fn count_words(text: &str) -> HashMap<String, i32> {
    let mut counts: HashMap<String, i32> = HashMap::new();
    for word in text.to_lowercase().split_whitespace() {
        *counts.entry(word.to_string()).or_insert(0) += 1;
    }
    counts
}

fn print_top(counts: &HashMap<String, i32>, n: i32) {
    let mut entries: Vec<(&String, &i32)> = counts.iter().collect();

    entries.sort_by(|a, b| {
        b.1.cmp(a.1)
            .then_with(|| a.0.cmp(b.0))
    });

    for (word, count) in entries.into_iter().take(n.max(0) as usize) {
        println!("  {} {}", count, word);
    }
}

fn main() {
    let text = "the cat sat on the mat the cat sat";
    let counts = count_words(text);
    print_top(&counts, 5);
}