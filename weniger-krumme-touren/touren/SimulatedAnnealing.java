package touren;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.ToDoubleFunction;

public class SimulatedAnnealing {
    public static class GeneticOperators {
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
    public static class Vector2d implements Comparable<Vector2d> {
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
    }


    // Obere Schranke fuer die Laenge einer Tour, berechnet aus Summe der n teuersten Kanten
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

    // Berechnet die Kosten einer Tour, wobei spitze Winkel bestraft werden
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
        // round to 2 decimal places
        return Math.round(result * 100) / 100.0;
    }

    // Erzeugt eine zufaellige Permutation der Zahlen 0 bis size-1
    static Integer[] randomPermutation(int size) {
        Integer[] result = new Integer[size];
        for (int i = 0; i < size; i++) {
            result[i] = i;
        }
        for (int i = 0; i < size; i++) {
            int k = (int) (Math.random() * size);
            int tmp = result[i];
            result[i] = result[k];
            result[k] = tmp;
        }
        return result;
    }

    // Macht aus Sekunden eine lesbare Zeitangabe
    static String secondsToTime(double seconds) {
        int hours = (int) (seconds / 3600);
        seconds -= hours * 3600;
        int minutes = (int) (seconds / 60);
        seconds -= minutes * 60;
        return String.format("%02d:%02d:%02d", hours, minutes, (int) seconds);
    }


    static Integer[] simulatedAnnealing(Integer[] candidate,
            List<Function<Integer[], Integer[]>> mutationOperators,
            ToDoubleFunction<Integer[]> costFunction, double minTemperature, double temperature,
            double coolingRate, double maxTime, double maxStagnation) {
        int printIteration = 0;
        int iteration = 0;

        candidate = candidate.clone();
        Integer[] candidateBest = candidate.clone();
        double costBest = costFunction.applyAsDouble(candidateBest);
        double costCurrent = costBest;
        double startTime = System.currentTimeMillis();
        int stagnation = 0;
        while (temperature > minTemperature
                && System.currentTimeMillis() - startTime < maxTime * 1000.0
                && stagnation < maxStagnation) {
            Integer[] newCandidate = mutationOperators
                    .get((int) (Math.random() * mutationOperators.size())).apply(candidate.clone());
            double costNew = costFunction.applyAsDouble(newCandidate);

            if (costNew < costBest) {
                candidateBest = newCandidate.clone();
                costBest = costNew;
                stagnation = 0;
            } else {
                stagnation++;
            }
            if (Math.random() < Math.exp((costCurrent - costNew) / temperature)) {
                candidate = newCandidate.clone();
                costCurrent = costNew;
            }

            temperature *= coolingRate;
            iteration++;

            // Ausgabe des Fortschritts
            if (System.currentTimeMillis() - startTime > 1000.0 * printIteration) {
                System.out.print("\rIteration: " + iteration + " Kosten: " + costBest + " Zeit: "
                        + secondsToTime((System.currentTimeMillis() - startTime) / 1000.0)
                        + " Temperatur: " + temperature + "           ");
                printIteration++;
            }
        }
        return candidate;
    }

    // Liest die Koordinaten aus einer Datei
    private static Vector2d[] readCoords(String path) {
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
        String path;
        if (args.length == 1) {
            path = args[0];
        } else {
            Scanner scanner = new Scanner(System.in);
            System.out.println("Pfad zur Datei:");
            path = scanner.nextLine();
            scanner.close();
        }
        Vector2d[] coords = readCoords(path);
        double acutePenalty = lengthUpperBound(coords);
        Integer[] solution = simulatedAnnealing(randomPermutation(coords.length),
                Arrays.asList(GeneticOperators::displace, GeneticOperators::insert,
                        GeneticOperators::reverseDisplace, GeneticOperators::threeOpt),
                (x) -> penalizedPathCost(x, coords, acutePenalty), 0.001, 16 * acutePenalty,
                0.999999, 600, 10000000);
        System.out.println();
        System.out.println("Kosten: " + penalizedPathCost(solution, coords, acutePenalty));
        System.out.println(Arrays.toString(solution));
        // write solution to file
        try {
            Files.write(Paths.get(path + ".solution"), Arrays.toString(solution).getBytes());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

