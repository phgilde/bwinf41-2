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

        public void setFirst(int num1) {
            this.num1 = num1;
        }

        public void setSecond(int num2) {
            this.num2 = num2;
        }

        public void swap() {
            int temp = num1;
            num1 = num2;
            num2 = temp;
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
        return normalize(b);
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
        return normalize(b);
    }

    static Integer[] normalize(Integer[] a) {
        IntPair[] b = new IntPair[a.length];
        for (int i = 0; i < a.length; i++) {
            b[i] = new IntPair(a[i], i);
        }
        Arrays.sort(b);
        for (int i = 0; i < a.length; i++) {
            b[i].setFirst(i);
            b[i].swap();
        }
        Arrays.sort(b);
        Integer[] c = new Integer[a.length];
        for (int i = 0; i < a.length; i++) {
            c[i] = b[i].second();
        }
        return c;
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
        Integer[] a = new Integer[n];
        for (int i = 0; i < n; i++) {
            a[i] = i;
        }
        return a;
    }

    static Map<IntPair, Set<List<Integer>>> memo = new HashMap<>();


    static Set<List<Integer>> k(int n, int a, int depth) {
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
        System.out.println("| ".repeat(depth) + "| k(" + n + ", " + a + ") "
                + ((int) Math.ceil(n / 1.5) - a + 1) * n * (n + 1)
                        * k(n - 1, a - 1, depth + 1).size()
                + " Its");
        HashSet<List<Integer>> result = new HashSet<>();
        for (IntPair rFlip : allRevFlipOps(n)) {

            for (List<Integer> seqL : k(n - 1, a - 1, depth + 1)) {
                Integer[] seq = seqL.toArray(new Integer[0]);
                Integer[] rFlipped = revFlipOp(seq, rFlip.first(), rFlip.second());
                if (!(a > 1 || !Arrays.equals(rFlipped, range(n)))) {
                    continue;
                }
                boolean r1 = true;
                boolean r2 = false;
                for (Integer flip : allFlipOps(n)) {
                    for (int b = a - 1; b < Math.ceil(n / 1.5); b++) {
                        r2 = false;
                        if (k(n - 1, b, depth + 1)
                                .contains(Arrays.asList(flipOp(rFlipped, flip)))) {
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
        System.out.println("| ".repeat(depth) + "| k(" + n + ", " + a + ") Fertig         ");
        memo.put(key, result);
        return result;
    }

    static Optional<Integer[]> kHasSolution(int n, int a) {
        for (IntPair rFlip : allRevFlipOps(n)) {
            for (List<Integer> seqL : k(n - 1, a - 1, 0)) {
                Integer[] seq = seqL.toArray(new Integer[0]);
                Integer[] rFlipped = revFlipOp(seq, rFlip.first(), rFlip.second());
                if (!(a > 1 || !Arrays.equals(rFlipped, range(n)))) {
                    continue;
                }

                boolean r1 = true;
                boolean r2 = false;
                for (Integer flip : allFlipOps(n)) {
                    for (int b = a - 1; b < Math.ceil(n / 1.5); b++) {
                        r2 = false;
                        if (k(n - 1, b, 0).contains(Arrays.asList(flipOp(rFlipped, flip)))) {
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
                    return Optional.of(rFlipped);
                }
            }
        }
        return Optional.empty();
    }

    public static void main(String[] args) {
        System.out.println(Arrays.deepToString(allRevFlipOps(5)));
        Scanner scanner = new Scanner(System.in);
        System.out.print("n: ");
        int n = scanner.nextInt();
        scanner.close();
        for (int a = (int) Math.ceil(n / 1.5); a > 0; a--) {
            Optional<Integer[]> result = kHasSolution(n, a);
            if (result.isPresent()) {
                System.out.println("max a: " + a);
                System.out.println(Arrays.deepToString(result.get()));
                break;
            }
        }

    }
}
