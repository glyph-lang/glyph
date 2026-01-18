```java
public class Fibonacci {
    public static int fib(int n) {
        if (n < 2) {
            return n;
        }
        return fib(n - 1) + fib(n - 2);
    }

    public static void main(String[] args) {
        System.out.println(fib(0));  // Output: 0
        System.out.println(fib(1));  // Output: 1
        System.out.println(fib(5));  // Output: 5
        System.out.println(fib(10)); // Output: 55
    }
}
```