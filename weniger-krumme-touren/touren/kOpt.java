package touren;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;
import java.util.function.ToDoubleFunction;

public class kOpt {
    public static Integer[] randomOrder(int n) {
        Integer[] tour = new Integer[n];
        for (int i = 0; i < n; i++) {
            tour[i] = i;
        }
        for (int i = 0; i < n; i++) {
            int j = (int) (Math.random() * n);
            int tmp = tour[i];
            tour[i] = tour[j];
            tour[j] = tmp;
        }
        return tour;
    }

    private static Integer[] nearestNeighbor(Vector2d[] coords, double acutePenalty) {
        int first = (int) (Math.random() * coords.length);
        int secord = (int) (Math.random() * coords.length);
        while (first == secord) {
            secord = (int) (Math.random() * coords.length);
        }
        Integer[] result = new Integer[coords.length];
        result[0] = first;
        result[1] = secord;
        boolean[] visited = new boolean[coords.length];
        visited[first] = true;
        visited[secord] = true;
        for (int i = 2; i < coords.length; i++) {
            double minDist = Double.MAX_VALUE;
            int minIndex = -1;
            for (int j = 0; j < coords.length; j++) {
                if (!visited[j]) {
                    double dist = coords[result[i - 1]].sub(coords[j]).length();
                    if (Vector2d.acute(coords[result[i - 2]], coords[result[i - 1]], coords[j])) {
                        dist += acutePenalty;
                    }
                    if (dist < minDist) {
                        minDist = dist;
                        minIndex = j;
                    }
                }
            }
            result[i] = minIndex;
            visited[minIndex] = true;
        }
        return result;
    }

    public static Integer[] threeOptStep(Integer[] tour, ToDoubleFunction<Integer[]> cost) {
        for (Integer a : randomOrder(tour.length)) {
            for (Integer b : randomOrder(tour.length)) {
                for (Integer c : randomOrder(tour.length)) {
                    Integer[] best = tour.clone();
                    if (a < b && b < c) {
                        for (Integer[] perm : permutations(4)) {
                            for (int rev = 0; rev < (1 << 4); rev++) {
                                List<Integer> result = new ArrayList<>();
                                for (int i : perm) {
                                    List<Integer> sublist;
                                    switch (i) {
                                        case 0:
                                            sublist = Arrays.asList(tour).subList(a, b);
                                            break;
                                        case 1:
                                            sublist = Arrays.asList(tour).subList(b, c);
                                            break;
                                        case 2:
                                            sublist = Arrays.asList(tour).subList(c, tour.length);
                                            break;
                                        case 3:
                                            sublist = Arrays.asList(tour).subList(0, a);
                                            break;
                                        default:
                                            throw new RuntimeException("Invalid order");
                                    }
                                    if ((rev & (1 << i)) != 0) {
                                        Collections.reverse(sublist);
                                    }
                                    result.addAll(sublist);
                                }

                                Integer[] candidate = result.toArray(new Integer[0]);
                                if (cost.applyAsDouble(candidate) < cost.applyAsDouble(best)) {
                                    best = candidate;
                                }
                            }
                        }
                        if (cost.applyAsDouble(best) < cost.applyAsDouble(tour)) {
                            return best;
                        }
                    }
                }
            }
        }
        return null;
    }

    // returns a list of all permutations of the first n integers
    public static List<Integer[]> permutations(int n) {
        Integer[] array = new Integer[n];
        for (int i = 0; i < n; i++) {
            array[i] = i;
        }

        List<Integer[]> result = new ArrayList<>();
        int[] c = new int[n];
        for (int i = 0; i < n; i++) {
            c[i] = 0;
        }
        result.add(array.clone());
        int i = 1;
        while (i < n) {
            if (c[i] < i) {
                if (i % 2 == 0) {
                    Integer temp = array[0];
                    array[0] = array[i];
                    array[i] = temp;
                } else {
                    Integer temp = array[c[i]];
                    array[c[i]] = array[i];
                    array[i] = temp;
                }
                result.add(array.clone());
                c[i]++;
                i = 1;
            } else {
                c[i] = 0;
                i++;
            }
        }

        return result;
    }

    static Integer[][] initPopulation(int populationSize, int individualSize) {
        Integer[][] population = new Integer[populationSize][individualSize];
        for (int i = 0; i < populationSize; i++) {
            for (int j = 0; j < individualSize; j++) {
                population[i][j] = j;
            }
            for (int j = 0; j < individualSize; j++) {
                int k = (int) (Math.random() * individualSize);
                int tmp = population[i][j];
                population[i][j] = population[i][k];
                population[i][k] = tmp;
            }
        }
        return population;
    }

    static double lengthUpperBound(Vector2d[] coords) {
        double[] distances = new double[coords.length * coords.length];
        for (int i = 0; i < coords.length; i++) {
            for (int j = 0; j < coords.length; j++) {
                distances[i * coords.length + j] = coords[i].sub(coords[j]).length();
            }
        }
        Arrays.sort(distances);
        double result = 0;
        for (int i = 0; i < coords.length; i++) {
            result += distances[coords.length * coords.length - i - 1];
        }
        return result;
    }

    static double penalizedPathCost(Integer[] solution, Vector2d[] coords, double acutePenalty) {
        Vector2d p1 = coords[solution[0]];
        Vector2d p2 = coords[solution[1]];
        double result = p1.sub(p2).length();
        for (int i = 2; i < solution.length; i++) {
            Vector2d p3 = coords[solution[i]];
            if (Vector2d.acute(p1, p2, p3)) {
                result += acutePenalty;
            }
            result += p2.sub(p3).length();
            p1 = p2;
            p2 = p3;
        }
        return result;
    }

    static Vector2d[] readCoords(String path) {
        List<Vector2d> coords = new ArrayList<>();
        try {
            Files.lines(Paths.get(path)).forEach((line) -> {
                String[] split = line.split(" ");
                coords.add(
                        new Vector2d(Double.parseDouble(split[0]), Double.parseDouble(split[1])));
            });
        } catch (IOException e) {
            e.printStackTrace();
        }
        return coords.toArray(new Vector2d[coords.size()]);
    }

    public static void main(String[] args) {
        System.out.print("Pfad zur Datei: ");
        Scanner scanner = new Scanner(System.in);
        String path = scanner.nextLine();
        scanner.close();
        Vector2d[] coords = readCoords(path);
        System.out.println(coords.length);
        double acutePenalty = lengthUpperBound(coords);
        Integer[] solution = nearestNeighbor(coords, acutePenalty);
        while (true) {
            Integer[] newSolution =
                    threeOptStep(solution, (tour) -> penalizedPathCost(tour, coords, acutePenalty));
            if (newSolution == null) {
                break;
            }
            solution = newSolution;
            System.out.println(penalizedPathCost(solution, coords, acutePenalty));
        }
        System.out.println();
        System.out.println("Cost: " + penalizedPathCost(solution, coords, acutePenalty));
        System.out.println(Arrays.toString(solution));
    }
}
