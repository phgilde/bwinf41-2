import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Scanner;
import java.util.Set;

public class Pwue {
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

    static Integer[] revFlipOp(Integer[] a, int i, int n) {
        i--;
        Integer[] b = new Integer[a.length + 1];
        for (int j = 0; j < i; j++) {
            if (a[i - j - 1] >= n) {
                b[j] = a[i - j - 1] + 1;
            } else {
                b[j] = a[i - j - 1];
            }
        }
        b[i] = n;
        for (int j = i; j < a.length; j++) {
            if (a[j] >= n) {
                b[j + 1] = a[j] + 1;
            } else {
                b[j + 1] = a[j];
            }
        }
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

    static Integer[] allFlipOps(int n) {
        Integer[] a = new Integer[n];
        for (int i = 0; i < n; i++) {
            a[i] = i + 1;
        }
        return a;
    }

    static IntPair[] allRevFlipOps(int n) {
        IntPair[] a = new IntPair[n * (n + 1)];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j <= n; j++) {
                a[i * (n + 1) + j] = new IntPair(i + 1, j);
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

    // Hier werden die Zwischenergebnisse der dynamischen Programmierung gespeichert
    static Map<IntPair, Set<List<Integer>>> memo = new HashMap<>();

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
            return result;
        }
        if (n == 1 && a != 0) {
            Set<List<Integer>> result = new HashSet<>();
            memo.put(key, result);
            return result;
        }
        HashSet<List<Integer>> result = new HashSet<>();
        for (IntPair rFlip : allRevFlipOps(n)) {

            for (List<Integer> seqL : k(n - 1, a - 1)) {
                Integer[] seq = seqL.toArray(new Integer[0]);
                Integer[] rFlipped = revFlipOp(seq, rFlip.first(), rFlip.second());
                if (!(a > 1 || !Arrays.equals(rFlipped, range(n)))) {
                    continue;
                }
                boolean r1 = true;
                boolean r2 = false;
                for (Integer flip : allFlipOps(n)) {
                    for (int b = a - 1; b < 2 * Math.floor(n / 3) + 2; b++) {
                        r2 = false;
                        if (k(n - 1, b).contains(Arrays.asList(flipOp(rFlipped, flip)))) {
                            r2 = true;
                            break;
                        }
                    }
                    r1 = r1 && r2;
                    if (!r1) {
                        break;
                    }
                }
                if (r1) {
                    result.add(Arrays.asList(rFlipped));
                }
            }
        }
        memo.put(key, result);
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
        for (IntPair rFlip : allRevFlipOps(n)) {
            for (List<Integer> seqL : k(n - 1, a - 1)) {
                Integer[] seq = seqL.toArray(new Integer[0]);
                Integer[] rFlipped = revFlipOp(seq, rFlip.first(), rFlip.second());
                if (!(a > 1 || !Arrays.equals(rFlipped, range(n))))
                    continue;

                // forall steht fuer den Existenzquantor ueber P_n
                // exists steht fuer den Allquantor ueber N zwischen a-1 und 2*floor(n/3)+1
                boolean forall = true;
                boolean exists = false;
                for (Integer flip : allFlipOps(n)) {
                    exists = false;
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
