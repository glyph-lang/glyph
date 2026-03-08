import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

public class Main {
    public static Result<Void, IOException> write_greeting(String filename) {
        try (BufferedWriter writer = Files.newBufferedWriter(Path.of(filename), StandardCharsets.UTF_8)) {
            writer.write("Hello from the program!");
            return Result.ok(null);
        } catch (IOException e) {
            return Result.err(e);
        }
    }

    public static final class Result<T, E extends Exception> {
        private final T value;
        private final E error;
        private final boolean ok;

        private Result(T value, E error, boolean ok) {
            this.value = value;
            this.error = error;
            this.ok = ok;
        }

        public static <T, E extends Exception> Result<T, E> ok(T value) {
            return new Result<>(value, null, true);
        }

        public static <T, E extends Exception> Result<T, E> err(E error) {
            return new Result<>(null, error, false);
        }

        public boolean isOk() {
            return ok;
        }

        public boolean isErr() {
            return !ok;
        }

        public T getValue() {
            return value;
        }

        public E getError() {
            return error;
        }
    }
}