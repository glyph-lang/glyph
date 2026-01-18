public class Main {
    public static class Result<T> {
        private final T value;
        private final String error;
        private Result(T value, String error) {
            this.value = value;
            this.error = error;
        }
        public static <T> Result<T> ok(T value) {
            return new Result<>(value, null);
        }
        public static <T> Result<T> err(String error) {
            return new Result<>(null, error);
        }
        public boolean isOk() { return error == null; }
        public T getValue() { return value; }
        public String getError() { return error; }
    }

    public static Result<Integer> safe_divide(int numerator, int denominator) {
        if (denominator == 0) {
            return Result.err("division by zero");
        }
        return Result.ok(numerator / denominator);
    }
}