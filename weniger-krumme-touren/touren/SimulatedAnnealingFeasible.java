package touren;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.ToDoubleFunction;

public class SimulatedAnnealingFeasible {
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


    static Integer[] simulatedAnnealing(Integer[] candidate,
            List<Function<Integer[], Integer[]>> mutationOperators,
            ToDoubleFunction<Integer[]> costFunction, double minTemperature, double temperature,
            double coolingRate, double maxTime, double costMax) {
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
                && costBest > costMax) {
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
                System.out.print("\rIteration: " + iteration + " Cost: " + costBest + " Time: "
                        + secondsToTime((System.currentTimeMillis() - startTime) / 1000.0)
                        + " Temperature: " + temperature + "           ");
                printIteration++;
            }
        }
        return candidate;
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
        System.out.println(acutePenalty);
        Integer[] solution = randomPermutation(coords.length);
        double coolingRate = 0.9;
        for (int i = 0; i < 100; i++) {
            solution = simulatedAnnealing(solution,
                    Arrays.asList(GeneticOperators::displace, GeneticOperators::insert,
                            GeneticOperators::reverseDisplace, GeneticOperators::threeOpt),
                    (x) -> penalizedPathCost(x, coords, acutePenalty), 0.001, acutePenalty,
                    coolingRate, 600, acutePenalty);
            coolingRate = 1 - (1 - coolingRate) * 0.5;
        }
        System.out.println();
        System.out.println("Cost: " + penalizedPathCost(solution, coords, acutePenalty));
        System.out.println(Arrays.toString(solution));
    }
}

