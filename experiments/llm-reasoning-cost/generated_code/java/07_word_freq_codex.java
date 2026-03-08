import java.util.*;

public class Main {
    public static Map<String, Integer> count_words(String text) {
        Map<String, Integer> counts = new HashMap<>();
        String[] words = text.toLowerCase().trim().split("\\s+");

        for (String word : words) {
            if (!word.isEmpty()) {
                counts.put(word, counts.getOrDefault(word, 0) + 1);
            }
        }

        return counts;
    }

    public static void print_top(Map<String, Integer> wordCounts, int n) {
        List<Map.Entry<String, Integer>> entries = new ArrayList<>(wordCounts.entrySet());

        entries.sort((a, b) -> {
            int cmp = Integer.compare(b.getValue(), a.getValue()); // count desc
            if (cmp != 0) return cmp;
            return a.getKey().compareTo(b.getKey()); // word asc
        });

        int limit = Math.min(n, entries.size());
        for (int i = 0; i < limit; i++) {
            Map.Entry<String, Integer> e = entries.get(i);
            System.out.println("  " + e.getValue() + " " + e.getKey());
        }
    }

    public static void main(String[] args) {
        String text = "the cat sat on the mat the cat sat";
        Map<String, Integer> counts = count_words(text);
        print_top(counts, 5);
    }
}