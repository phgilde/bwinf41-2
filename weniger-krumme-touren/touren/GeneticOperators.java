package touren;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

/*
 * Operatoren nach Larranaga et al.:
 * "Genetic Algorithms for the Travelling Salesman Problem: A Review of Representations and Operators"
 * (1999)
 */

public class GeneticOperators {
    // displace-operator
    static Integer[] displace(Integer[] individual) {
        List<Integer> result = new ArrayList<Integer>();
        List<Integer> result2 = new ArrayList<Integer>();
        List<Integer> list = Arrays.asList(individual);
        int i = (int) (Math.random() * (individual.length - 1));
        int j = (int) (Math.random() * (individual.length - i)) + i;
        int k = (int) (Math.random() * (individual.length - j + i));

        // result enthaelt permutation ohne segment
        result.addAll(list.subList(0, i));
        result.addAll(list.subList(j, individual.length));

        // segment an neuer stelle einfuegen
        result2.addAll(result.subList(0, k));
        result2.addAll(list.subList(i, j));
        result2.addAll(result.subList(k, result.size()));

        return result2.toArray(new Integer[0]);
    }

    static Integer[] insert(Integer[] individual) {
        Integer[] result = new Integer[individual.length - 1];
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

    static Integer[] threeOpt(Integer[] individual) {
        List<Integer> list = Arrays.asList(individual);
        List<Integer> result = new ArrayList<Integer>();
        int i = (int) (Math.random() * (individual.length - 1));
        int j = (int) (Math.random() * (individual.length - i)) + i;
        int k = (int) (Math.random() * (individual.length - j)) + j;
        Integer[] order = new Integer[] {0, 1, 2, 3};
        for (int l = 0; l < 4; l++) {
            int m = (int) (Math.random() * (4));
            int tmp = order[0];
            order[0] = order[m];
            order[m] = tmp;
        }
        for (int l = 0; l < 4; l++) {
            List<Integer> segment;
            switch (order[l]) {
                case 0:
                    segment = list.subList(0, i);
                    break;
                case 1:
                    segment = (list.subList(i, j));
                    break;
                case 2:
                    segment = (list.subList(j, k));
                    break;
                default:
                    segment = (list.subList(k, individual.length));
                    break;
            }
            if (Math.random() < 0.5) {
                Collections.reverse(segment);
            }
            result.addAll(segment);
        }
        return result.toArray(new Integer[0]);
    }
}
