import java.util.ArrayList;
import java.util.List;

public class Main {
    public static int sum_positive(List<Integer> nums) {
        List<Integer> positives = new ArrayList<>();

        for (int i = 0; i < nums.size(); i++) {
            int value = nums.get(i);
            if (value > 0) {
                positives.add(value);
            }
        }

        int sum = 0;
        for (int i = 0; i < positives.size(); i++) {
            sum += positives.get(i);
        }

        return sum;
    }
}