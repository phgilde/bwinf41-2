package touren;

public class Vector2d implements Comparable<Vector2d> {
    final private double x, y;

    public Vector2d(double x, double y) {
        this.x = x;
        this.y = y;
    }
    public Vector2d sub(Vector2d other) {
        return new Vector2d(x - other.x, y - other.y);
    }
    public double dot(Vector2d other) {
        return x * other.x + y * other.y;
    }
    public double length() {
        return Math.sqrt(x * x + y * y);
    }
    @Override
    public int compareTo(Vector2d o) {
        if (x < o.x) {
            return -1;
        } else if (x > o.x) {
            return 1;
        } else if (y < o.y) {
            return -1;
        } else if (y > o.y) {
            return 1;
        } else {
            return 0;
        }
    }	
}
