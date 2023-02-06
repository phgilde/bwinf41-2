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
    public static boolean acute(Vector2d a, Vector2d b, Vector2d c) {
        return a.sub(b).dot(c.sub(b)) > 0;
    }

    public static int countAcutes(Vector2d[] coords, Integer[] individual) {
        int count = 0;
        for (int i = 0; i < individual.length-2; i++) {
            int j = (i + 1);
            int k = (i + 2);
            if (acute(coords[individual[i]], coords[individual[j]], coords[individual[k]])) {
                count++;
            }
        }
        return count;
    }
}
