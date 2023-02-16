package touren;

public class Pair<A, B> {
    A first;

    public A getFirst() {
        return first;
    }

    B second;

    public B getSecond() {
        return second;
    }

    public Pair(A first, B second) {
        this.first = first;
        this.second = second;
    }

    @Override
    public int hashCode() {
        int hash = 17;
        hash = hash * 31 + first.hashCode();
        hash = hash * 31 + second.hashCode();
        return hash;
    }
}
