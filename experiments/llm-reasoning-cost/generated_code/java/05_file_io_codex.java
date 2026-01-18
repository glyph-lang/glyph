import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;

public class FileUtil {

    public static Result write_greeting(String filename) {
        return write_greeting(Path.of(filename));
    }

    public static Result write_greeting(Path filename) {
        try (FileWriter writer = new FileWriter(filename.toFile())) {
            writer.write("Hello from the program!");
            return Result.success();
        } catch (IOException e) {
            return Result.error(e);
        }
    }

    public static class Result {
        private final boolean success;
        private final Exception error;

        private Result(boolean success, Exception error) {
            this.success = success;
            this.error = error;
        }

        public static Result success() {
            return new Result(true, null);
        }

        public static Result error(Exception e) {
            return new Result(false, e);
        }

        public boolean isSuccess() {
            return success;
        }

        public Exception getError() {
            return error;
        }
    }
}