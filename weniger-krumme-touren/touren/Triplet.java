package touren;

public class Triplet<A, B, C>{
    A first;

    public A getFirst() {
        return first;
    }

    B second;

    public B getSecond() {
        return second;
    }

    C third;

    public C getThird() {
        return third;
    }

    public Triplet(A first, B second, C third) {
        this.first = first;
        this.second = second;
        this.third = third;
    }

    @Override
    public int hashCode() {
        int hash = 17;
        hash = hash * 31 + first.hashCode();
        hash = hash * 31 + second.hashCode();
        hash = hash * 31 + third.hashCode();
        return hash;
    }
}
