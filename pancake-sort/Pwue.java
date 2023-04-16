import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Scanner;
import java.util.Set;

public class Pwue {
    // Einfache Klasse fuer Paare von Integern, die als Schluessel in Maps verwendet werden koennen
    static class IntPair implements Comparable<IntPair> {
        private int num1;
        private int num2;

        public IntPair(int key, int value) {
            this.num1 = key;
            this.num2 = value;
        }

        public int first() {
            return num1;
        }

        public int second() {
            return num2;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o)
                return true;
            if (o == null || getClass() != o.getClass())
                return false;
            IntPair pair = (IntPair) o;
            return (num1 == pair.num1 && num2 == pair.num2);
        }

        @Override
        public int hashCode() {
            // Polynom-hash mit horner schema
            int hash = 17;
            hash = hash * 31 + num1;
            hash = hash * 31 + num2;
            return hash;
        }

        @Override
        public String toString() {
            return "(" + num1 + ", " + num2 + ")";
        }

        @Override
        public int compareTo(Pwue.IntPair o) {
            if (num1 < o.num1) {
                return -1;
            }
            if (num1 > o.num1) {
                return 1;
            }
            if (num2 < o.num2) {
                return -1;
            }
            if (num2 > o.num2) {
                return 1;
            }
            return 0;
        }
    }

    // PWUE-Operation, welche die ersten i Elemente eines Stapels wendet und dann den obersten
    // Pfannkuchen
    // entfernt.
    static Integer[] flipOp(Integer[] a, int i) {
        Integer[] b = new Integer[a.length - 1];
        for (int j = 0; j < i - 1; j++) {
            b[j] = a[i - j - 2];
        }
        for (int j = i; j < a.length; j++) {
            b[j - 1] = a[j];
        }
        return canonical(b);
    }

    // Umgekehrte PWUE-Operation, die erst den Pfannkuchen der Groesse newSize auf den Stapel legt
    // und dann die ersten
    // pos Elemente wendet. Pfannkuchen, die groesser als newSize sind, werden dabei um 1 erhoeht,
    // um zu verhindern, dass
    // eine Groesse doppelt vorkommt.
    static Integer[] revFlipOp(Integer[] a, int pos, int newSize) {
        Integer[] b = new Integer[a.length + 1];
        b[0] = newSize;
        for (int i = 0; i < a.length; i++) {
            if (a[i] >= newSize) {
                b[i + 1] = a[i] + 1;
            } else {
                b[i + 1] = a[i];
            }
        }
        for (int i = 0; i < pos / 2; i++) {
            int tmp = b[i];
            b[i] = b[pos - i - 1];
            b[pos - i - 1] = tmp;
        }
        // System.out.println("revFlipOp(" + Arrays.toString(a) + ", " + pos + ", " + newSize + ") =
        // " + Arrays.toString(b));
        return canonical(b);
    }

    // Bringt die Permutation in die kanonische Form, die allerdings bei 0 statt 1 anfaengt.
    // Folgt der Konvention, dass Indices bei 0 anfangen, nicht bei 1, der kleinste Pfannkuchen
    // ist also der nullte hat also den Index 0.
    static Integer[] canonical(Integer[] a) {
        Integer min = Integer.MAX_VALUE;
        Integer max = Integer.MIN_VALUE;
        for (int i = 0; i < a.length; i++) {
            if (a[i] < min)
                min = a[i];
            if (a[i] > max)
                max = a[i];
        }
        Integer[] values = new Integer[max - min + 1];

        for (int i = 0; i < values.length; i++)
            values[i] = -1;

        for (int i = 0; i < a.length; i++)
            values[a[i] - min] = a[i];

        Integer counter = 0;

        for (int i = 0; i < values.length; i++)
            if (values[i] != -1)
                values[i] = counter++;

        Integer[] result = new Integer[a.length];
        for (int i = 0; i < a.length; i++)
            result[i] = values[a[i] - min];

        return result;

    }

    // Menge aller PWUE-Operationen fuer einen Stapel der Hoehe n, dargestellt als
    // Array der Zahl der zu wendenden Pfannkuchen.
    static Integer[] allFlipOps(int n) {
        Integer[] a = new Integer[n];
        for (int i = 0; i < n; i++) {
            a[i] = i + 1;
        }
        return a;
    }

    // Menge aller umgekehrten PWUE-Operationen fuer einen Stapel der Hoehe n, dargestellt als
    // Array von Paaren (pos, newSize), wobei pos die Anzahl der zu wendenden Pfannkuchen und
    // newSize die Groesse des neuen Pfannkuchens ist.
    static IntPair[] allRevFlipOps(int n) {
        IntPair[] a = new IntPair[(n + 1) * (n + 1)];
        for (int pos = 0; pos <= n; pos++) {
            for (int newSize = 0; newSize <= n; newSize++) {
                a[pos * (n + 1) + newSize] = new IntPair(pos + 1, newSize);
            }
        }
        return a;
    }

    static Integer[] range(int n) {
        assert n >= 0;
        Integer[] a = new Integer[n];
        for (int i = 0; i < n; i++) {
            a[i] = i;
        }
        return a;
    }

    // Hier werden die Zwischenergebnisse der dynamischen Programmierung gespeichert.
    static Map<IntPair, Set<List<Integer>>> memo = new HashMap<>();
    static Map<List<Integer>, IntPair> backref = new HashMap<>();

    // Speichert fuer Stapel die Werte (n, a) aus K(n, a). backref ist eine Inverse von memo.
    static void memoBackref(Set<List<Integer>> s, IntPair p) {
        for (List<Integer> l : s) {
            backref.put(l, p);
        }
    }

    static Set<List<Integer>> k(int n, int a) {
        // Dynamische Programmierung: ggf. schon vorhandenes Ergebnis zurueckgeben
        IntPair key = new IntPair(n, a);
        if (memo.containsKey(key)) {
            return memo.get(key);
        }
        if (a == 0) {
            Set<List<Integer>> result = new HashSet<>();
            result.add(Arrays.asList(range(n)));
            memo.put(key, result);
            memoBackref(result, key);
            return result;
        }
        if (n == 1 && a != 0) {
            Set<List<Integer>> result = new HashSet<>();
            memo.put(key, result);
            memoBackref(result, key);
            return result;
        }
        HashSet<List<Integer>> result = new HashSet<>();
        for (IntPair rFlip : allRevFlipOps(n - 1)) {

            for (List<Integer> seqL : k(n - 1, a - 1)) {
                Integer[] seq = seqL.toArray(new Integer[0]);
                Integer[] rFlipped = revFlipOp(seq, rFlip.first(), rFlip.second());
                if (!(a > 1 || !Arrays.equals(rFlipped, range(n)))) {
                    continue;
                }
                boolean forall = true;
                for (Integer flip : allFlipOps(n)) {
                    if (backref.containsKey(Arrays.asList(flipOp(rFlipped, flip)))) {
                        IntPair p = backref.get(Arrays.asList(flipOp(rFlipped, flip)));
                        if (p.second() < a - 1) {
                            forall = false;
                            break;
                        } else {
                            continue;
                        }
                    }
                    boolean exists = false;
                    for (int b = a - 1; b < 2 * Math.floor(n / 3) + 2; b++) {
                        if (k(n - 1, b).contains(Arrays.asList(flipOp(rFlipped, flip)))) {
                            exists = true;
                            break;
                        }
                    }
                    forall = forall && exists;
                    if (!forall) {
                        break;
                    }
                }
                if (forall) {
                    result.add(Arrays.asList(rFlipped));
                }
            }
        }
        memo.put(key, result);
        memoBackref(result, key);
        return result;
    }

    // Gibt nur ein Element aus k(n, a) zurueck, falls es eins gibt
    static Optional<Integer[]> kHasSolution(int n, int a) {
        if (a == 0) {
            return Optional.of(range(n));
        }
        if (n == 1 && a != 0) {
            return Optional.empty();
        }
        for (IntPair rFlip : allRevFlipOps(n - 1)) {
            for (List<Integer> seqL : k(n - 1, a - 1)) {
                Integer[] seq = seqL.toArray(new Integer[0]);
                Integer[] rFlipped = revFlipOp(seq, rFlip.first(), rFlip.second());
                if (!(a > 1 || !Arrays.equals(rFlipped, range(n))))
                    continue;

                // forall steht fuer den Allquantor ueber alle moeglichen flip-Operationen
                boolean forall = true;
                for (Integer flip : allFlipOps(n)) {
                    // Pruefen, ob der Stapel schon bekannt ist. Verhindert, dass k(n-1,b) fuer a-1
                    // <= b <= 2*floor(n/3)+1
                    // berechnet werden muss.
                    if (backref.containsKey(Arrays.asList(flipOp(rFlipped, flip)))) {
                        IntPair p = backref.get(Arrays.asList(flipOp(rFlipped, flip)));
                        if (p.second() < a - 1) {
                            forall = false;
                            break;
                        } else {
                            continue;
                        }
                    }
                    // exists steht fuer den Existenzquantor ueber N zwischen a-1 und 2*floor(n/3)+1
                    boolean exists = false;
                    for (int b = a - 1; b < 2 * Math.floor(n / 3) + 2; b++) {
                        if (k(n - 1, b).contains(Arrays.asList(flipOp(rFlipped, flip)))) {
                            exists = true;
                            break;
                        }
                    }
                    forall = forall && exists;
                    if (!forall)
                        break;
                }
                if (forall)
                    return Optional.of(rFlipped);
            }
        }
        return Optional.empty();
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        System.out.print("n: ");
        int n = scanner.nextInt();
        scanner.close();
        long startTime = System.currentTimeMillis();
        for (int a = (int) Math.floor(n / 3) * 2 + 1; a > 0; a--) {
            Optional<Integer[]> result = kHasSolution(n, a);
            if (result.isPresent()) {
                System.out.println("max a: " + a);
                for (int i = 0; i < result.get().length; i++) {
                    System.out.print((result.get()[i] + 1) + " ");
                }
                System.out.println();
                System.out.println("time: " + (System.currentTimeMillis() - startTime) + "ms");
                break;
            }
        }

    }
}
