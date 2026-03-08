import java.util.List;

public class Main {
    public static class Point {
        int x;
        int y;

        public Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        public int squared_mag() {
            return x * x + y * y;
        }
    }

    public static int filter_and_sum(List<Point> points, int max_mag) {
        int total = 0;
        for (Point p : points) {
            int mag = p.squared_mag();
            if (mag <= max_mag) {
                total += mag;
            }
        }
        return total;
    }
}