import java.util.*;

public class WordFrequencyCounter {

    public static Map<String, Integer> countWords(String text) {
        Map<String, Integer> wordCount = new HashMap<>();
        String[] words = text.toLowerCase().split("\\s+");
        
        for (String word : words) {
            wordCount.put(word, wordCount.getOrDefault(word, 0) + 1);
        }
        
        return wordCount;
    }

    public static void printTop(Map<String, Integer> wordCount, int n) {
        List<Map.Entry<String, Integer>> sortedEntries = new ArrayList<>(wordCount.entrySet());
        
        sortedEntries.sort((a, b) -> {
            int countComparison = b.getValue().compareTo(a.getValue());
            if (countComparison != 0) {
                return countComparison;
            }
            return a.getKey().compareTo(b.getKey());
        });
        
        for (int i = 0; i < Math.min(n, sortedEntries.size()); i++) {
            Map.Entry<String, Integer> entry = sortedEntries.get(i);
            System.out.printf("  %d %s%n", entry.getValue(), entry.getKey());
        }
    }

    public static void main(String[] args) {
        String text = "the cat sat on the mat the cat sat";
        Map<String, Integer> wordCount = countWords(text);
        printTop(wordCount, 5);
    }
}
