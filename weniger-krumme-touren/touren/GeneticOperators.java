package touren;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

public class GeneticOperators {
    static Integer[] segmentSwap(Integer[] individual) {
        Integer[] result = individual.clone();
        int i = (int) (Math.random() * (individual.length - 3));
        int j = i + (int) (Math.random() * (individual.length - i - 3));
        int k = j + (int) (Math.random() * (individual.length - j - 1));
        int l = k + (int) (Math.random() * (individual.length - k));
        for (int ix = 0; ix < i; ix++) {
            result[ix] = individual[ix];
        }
        for (int ix = 0; ix < (l - k); ix++) {
            result[i + ix] = individual[k + ix];
        }
        for (int ix = 0; ix < (k - j); ix++) {
            result[i + (l - k) + ix] = individual[j + ix];
        }
        for (int ix = 0; ix < (j - i); ix++) {
            result[i + l - j + ix] = individual[i + ix];
        }
        for (int ix = 0; ix < (individual.length - l); ix++) {
            result[l + ix] = individual[l + ix];
        }

        return result;
    }

    static Integer[] swap(Integer[] individual) {
        Integer[] result = individual.clone();
        int i = (int) (Math.random() * (individual.length - 1));
        int j = (int) (Math.random() * individual.length);
        int tmp = result[i];
        result[i] = result[j];
        result[j] = tmp;
        return result;
    }

    static Integer[] rotate(Integer[] individual) {
        Integer[] result = individual.clone();
        int i = (int) (Math.random() * (individual.length));
        for (int j = 0; j < individual.length; j++) {
            result[j] = individual[(j + i) % individual.length];
        }
        return result;
    }

    static Integer[] reverse(Integer[] individual) {
        Integer[] result = individual.clone();
        int i = (int) (Math.random() * (individual.length - 1));
        int j = (int) (Math.random() * (individual.length - i)) + i;
        for (int k = 0; k < (j - i) / 2; k++) {
            int tmp = result[i + k];
            result[i + k] = result[j - k];
            result[j - k] = tmp;
        }
        return result;
    }

    static Integer[] displace(Integer[] individual) {
        List<Integer> result = new ArrayList<Integer>();
        List<Integer> result2 = new ArrayList<Integer>();
        List<Integer> list = Arrays.asList(individual);
        int i = (int) (Math.random() * (individual.length - 1));
        int j = (int) (Math.random() * (individual.length - i)) + i;
        int k = (int) (Math.random() * (individual.length - j + i));
        result.addAll(list.subList(0, i));
        result.addAll(list.subList(j, individual.length));
        result2.addAll(result.subList(0, k));
        result2.addAll(list.subList(i, j));
        result2.addAll(result.subList(k, result.size()));
        return result2.toArray(new Integer[0]);
    }

    static Integer[] insert(Integer[] individual) {
        Integer[] result = new Integer[individual.length-1];
        Integer[] result2 = new Integer[individual.length];
        int i = (int) (Math.random() * (individual.length - 1));
        int j = (int) (Math.random() * (individual.length - 1));
        for (int ix = 0; ix < i; ix++) {
            result[ix] = individual[ix];
        }
        for (int ix = i; ix < individual.length - 1; ix++) {
            result[ix] = individual[ix + 1];
        }
        for (int ix = 0; ix < j; ix++) {
            result2[ix] = result[ix];
        }
        result2[j] = individual[i];
        for (int ix = j; ix < individual.length - 1; ix++) {
            result2[ix + 1] = result[ix];
        }
        return result2;
    }
    static Integer[] reverseDisplace(Integer[] individual) {
        List<Integer> result = new ArrayList<Integer>();
        List<Integer> result2 = new ArrayList<Integer>();
        List<Integer> list = Arrays.asList(individual);
        int i = (int) (Math.random() * (individual.length - 1));
        int j = (int) (Math.random() * (individual.length - i)) + i;
        int k = (int) (Math.random() * (individual.length - j + i));
        result.addAll(list.subList(0, i));
        result.addAll(list.subList(j, individual.length));
        result2.addAll(result.subList(0, k));
        List<Integer> reverse = list.subList(i, j);
        Collections.reverse(reverse);
        result2.addAll(reverse);
        result2.addAll(result.subList(k, result.size()));
        return result2.toArray(new Integer[0]);
    }

    public static class OperatorNoAcute implements Function<Integer[], Integer[]>{
        private Vector2d[] coords;
        private int maxIterations;
        private int maxNew;
        private Function<Integer[], Integer[]> operator;

        public OperatorNoAcute(Vector2d[] coords, int maxIterations, Function<Integer[], Integer[]> operator, int maxNew) {
            this.coords = coords;
            this.maxIterations = maxIterations;
            this.operator = operator;
            this.maxNew = maxNew;
        }
        public Integer[] apply(Integer[] individual) {
            int count = Vector2d.countAcutes(coords, individual);
            for (int i = 0; i < maxIterations; i++) {
                Integer[] newIndividual = operator.apply(individual);
                int newCount = Vector2d.countAcutes(coords, newIndividual);
                if (newCount + maxNew <= count) {
                    return newIndividual;
                }
            }
            return individual;
        }
    }
}
