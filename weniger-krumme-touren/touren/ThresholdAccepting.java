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

public class ThresholdAccepting {
    static void sortOn(Integer[][] population, ToDoubleFunction<Integer[]> costFunction) {
        Arrays.sort(population, (a, b) -> {
            double costA = costFunction.applyAsDouble(a);
            double costB = costFunction.applyAsDouble(b);
            if (costA < costB) {
                return -1;
            } else if (costA > costB) {
                return 1;
            } else {
                return 0;
            }
        });
    }

    static int cumWeightChoice(double[] cumWeights) {
        double r = Math.random();
        for (int i = 0; i < cumWeights.length; i++) {
            if (r < cumWeights[i]) {
                return i;
            }
        }
        return cumWeights.length - 1;
    }

    static double[] softmax(double[] x, double temperature) {
        double[] result = new double[x.length];
        double sum = 0;
        for (int i = 0; i < x.length; i++) {
            result[i] = Math.exp(x[i] / temperature);
            sum += result[i];
        }
        for (int i = 0; i < x.length; i++) {
            result[i] /= sum;
        }
        return result;
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

    static Integer[][] nearestNeighborInitPopul(int populationSize, Vector2d[] coords,
            double acutePenalty) {
        Integer[][] population = new Integer[populationSize][coords.length];
        for (int i = 0; i < populationSize; i++) {
            population[i] = nearestNeighbor(coords, acutePenalty);
        }
        return population;
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

    static String secondsToTime(double seconds) {
        int hours = (int) (seconds / 3600);
        seconds -= hours * 3600;
        int minutes = (int) (seconds / 60);
        seconds -= minutes * 60;
        return String.format("%02d:%02d:%02d", hours, minutes, (int) seconds);
    }


    static Integer[] thresholdAcc(Integer[] candidate,
            List<Function<Integer[], Integer[]>> mutationOperators,
            ToDoubleFunction<Integer[]> costFunction, int maxIterations, double threshold,
            double coolingRate, double maxTime) {
        int printIteration = 0;
        int iteration = 0;
        candidate = candidate.clone();
        Integer[] candidateBest = candidate.clone();
        double costBest = costFunction.applyAsDouble(candidateBest);
        double costCurrent = costBest;
        double startTime = System.currentTimeMillis();
        while (iteration < maxIterations
                && System.currentTimeMillis() - startTime < maxTime * 1000.0) {
            Integer[] newCandidate = mutationOperators
                    .get((int) (Math.random() * mutationOperators.size())).apply(candidate.clone());
            double costNew = costFunction.applyAsDouble(newCandidate);
            if (costNew < costBest) {
                candidateBest = newCandidate.clone();
                costBest = costNew;
            }
            if (costNew < costCurrent - threshold) {
                candidate = newCandidate.clone();
                costCurrent = costNew;
            }
            threshold *= coolingRate;
            iteration++;
            if (System.currentTimeMillis() - startTime > 1000.0 * printIteration) {
                System.out.print("\rIteration: " + iteration + " Cost: "
                        + costBest + " Time: "
                        + secondsToTime((System.currentTimeMillis() - startTime) / 1000.0) + " TH: "
                        + threshold + "    ");
                printIteration++;
            }
        }
        return candidate;
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

    public static void main(String[] args) {
        System.out.print("Pfad zur Datei: ");
        Scanner scanner = new Scanner(System.in);
        String path = scanner.nextLine();
        scanner.close();
        Vector2d[] coords = readCoords(path);
        double acutePenalty = lengthUpperBound(coords);
        Integer[] solution = thresholdAcc(initPopulation(1, coords.length)[0],
                Arrays.asList(GeneticOperators::displace, GeneticOperators::insert,
                        GeneticOperators::reverseDisplace, GeneticOperators::fourOpt),
                (x) -> penalizedPathCost(x, coords, acutePenalty), (int) 1e9, 3 * acutePenalty,
                0.999999, 60);
        System.out.println();
        System.out.println("Cost: " + penalizedPathCost(solution, coords, acutePenalty));
        System.out.println(Arrays.toString(solution));
    }
}

