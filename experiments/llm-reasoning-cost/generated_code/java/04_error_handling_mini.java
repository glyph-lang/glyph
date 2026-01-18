```java
public class SafeDivide {

    public static Result<Integer, String> safeDivide(int numerator, int denominator) {
        if (denominator == 0) {
            return Result.err("division by zero");
        }
        return Result.ok(numerator / denominator);
    }

    public static class Result<T, E> {
        private final T value;
        private final E error;
        private final boolean isError;

        private Result(T value, E error, boolean isError) {
            this.value = value;
            this.error = error;
            this.isError = isError;
        }

        public static <T, E> Result<T, E> ok(T value) {
            return new Result<>(value, null, false);
        }

        public static <T, E> Result<T, E> err(E error) {
            return new Result<>(null, error, true);
        }

        public boolean isError() {
            return isError;
        }

        public T getValue() {
            if (isError) {
                throw new IllegalStateException("Cannot get value from an error result");
            }
            return value;
        }

        public E getError() {
            if (!isError) {
                throw new IllegalStateException("Cannot get error from a success result");
            }
            return error;
        }
    }
}
```