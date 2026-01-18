```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class FileIO {

    public static Result writeGreeting(String filename) {
        BufferedWriter writer = null;
        try {
            writer = new BufferedWriter(new FileWriter(filename));
            writer.write("Hello from the program!");
            return Result.success();
        } catch (IOException e) {
            return Result.error(e.getMessage());
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (IOException e) {
                    return Result.error(e.getMessage());
                }
            }
        }
    }

    public static class Result {
        private final boolean success;
        private final String errorMessage;

        private Result(boolean success, String errorMessage) {
            this.success = success;
            this.errorMessage = errorMessage;
        }

        public static Result success() {
            return new Result(true, null);
        }

        public static Result error(String errorMessage) {
            return new Result(false, errorMessage);
        }

        public boolean isSuccess() {
            return success;
        }

        public String getErrorMessage() {
            return errorMessage;
        }
    }
}
```