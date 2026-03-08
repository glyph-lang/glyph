public class SafeDivide {
    public static Result<Integer, String> safe_divide(int numerator, int denominator) {
        if (denominator == 0) {
            return Result.err("division by zero");
        }
        return Result.ok(numerator / denominator);
    }

    public static final class Result<T, E> {
        private final T ok;
        private final E err;
        private final boolean isOk;

        private Result(T ok, E err, boolean isOk) {
            this.ok = ok;
            this.err = err;
            this.isOk = isOk;
        }

        public static <T, E> Result<T, E> ok(T value) {
            return new Result<>(value, null, true);
        }

        public static <T, E> Result<T, E> err(E error) {
            return new Result<>(null, error, false);
        }

        public boolean isOk() {
            return isOk;
        }

        public boolean isErr() {
            return !isOk;
        }

        public T unwrap() {
            if (!isOk) throw new IllegalStateException("Called unwrap on Err");
            return ok;
        }

        public E unwrapErr() {
            if (isOk) throw new IllegalStateException("Called unwrapErr on Ok");
            return err;
        }

        @Override
        public String toString() {
            return isOk ? "Ok(" + ok + ")" : "Err(" + err + ")";
        }
    }

    public static void main(String[] args) {
        System.out.println(safe_divide(10, 2)); // Ok(5)
        System.out.println(safe_divide(7, 3));  // Ok(2)
        System.out.println(safe_divide(5, 0));  // Err(division by zero)
    }
}