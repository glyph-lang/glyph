```java
import java.util.ArrayList;
import java.util.List;

public class VectorOperations {
    public static int sum_positive(List<Integer> numbers) {
        List<Integer> positiveNumbers = new ArrayList<>();
        for (int number : numbers) {
            if (number > 0) {
                positiveNumbers.add(number);
            }
        }
        int sum = 0;
        for (int positiveNumber : positiveNumbers) {
            sum += positiveNumber;
        }
        return sum;
    }

    public static void main(String[] args) {
        List<Integer> input = List.of(1, -2, 3, -4, 5);
        int result = sum_positive(input);
        System.out.println(result); // Output: 9
    }
}
```