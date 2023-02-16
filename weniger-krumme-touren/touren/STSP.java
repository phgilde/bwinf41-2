package touren;

public class STSP {
    double[][] distances;

    public STSP(double[][] distances) {
        this.distances = distances;
    }

    public double[][] getDistances() {
        return distances;
    }

    public double getDistance(int i, int j) {
        return distances[i][j];
    }

    public int size() {
        return distances.length;
    }
}
