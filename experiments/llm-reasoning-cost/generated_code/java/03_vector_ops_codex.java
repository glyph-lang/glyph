import java.util.ArrayList;
import java.util.List;

public class Main {
    public static int sum_positive(int[] v) {
        List<Integer> positives = new ArrayList<>();
        for (int num : v) {
            if (num > 0) {
                positives.add(num);
            }
        }
        int sum = 0;
        for (int num : positives) {
            sum += num;
        }
        return sum;
    }
}