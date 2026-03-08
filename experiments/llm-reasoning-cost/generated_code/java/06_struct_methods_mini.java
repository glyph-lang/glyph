import java.util.List;

class Point {
    private int x;
    private int y;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int squaredMag() {
        return x * x + y * y;
    }
}

public class PointOperations {
    public static int filterAndSum(List<Point> points, int maxMag) {
        int total = 0;
        for (Point point : points) {
            if (point.squaredMag() <= maxMag) {
                total += point.squaredMag();
            }
        }
        return total;
    }
}
