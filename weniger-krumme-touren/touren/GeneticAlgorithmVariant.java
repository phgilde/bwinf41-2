package touren;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.function.Function;
import java.util.function.ToDoubleFunction;

public class GeneticAlgorithmVariant {
    static Integer[][] weightedSample(Integer[][] population, double[] weights, int k) {
        Integer[] order = new Integer[population.length];
        double[] v = new double[population.length];
        for (int i = 0; i < order.length; i++) {
            order[i] = i;
            v[i] = Math.pow(Math.random(), 1 / (weights[i] + 0.00001));
        }

        sortOn(order, (x) -> v[x]);
        Integer[][] result = new Integer[k][population[0].length];
        for (int i = 0; i < k; i++) {
            result[i] = population[order[i]];
        }
        return result;
    }

    static <T> void sortOn(T[] population, ToDoubleFunction<T> costFunction) {
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

    static String secondsToTime(double seconds) {
        int hours = (int) (seconds / 3600);
        seconds -= hours * 3600;
        int minutes = (int) (seconds / 60);
        seconds -= minutes * 60;
        return String.format("%02d:%02d:%02d", hours, minutes, (int) seconds);
    }

    static Integer[] localSearch(Integer[] individual,
            List<Function<Integer[], Integer[]>> mutationOperators,
            ToDoubleFunction<Integer[]> costFunction, double maxIterations, double temperature) {
        Integer[] result = individual.clone();
        for (int i = 0; i < maxIterations; i++) {
            for (Function<Integer[], Integer[]> mutationOperator : mutationOperators) {
                Integer[] mutated = mutationOperator.apply(result.clone());
                if (costFunction.applyAsDouble(mutated) < costFunction.applyAsDouble(result)) {
                    result = mutated;
                }
                if (Math.random() < Math.exp(
                        (costFunction.applyAsDouble(mutated) - costFunction.applyAsDouble(result))
                                / temperature)) {
                    result = mutated;
                }
            }
        }
        return result;
    }

    static Integer[] geneticAlgorithm(Integer[][] initPopulation,
            List<Function<Integer[], Integer[]>> mutationOperators,
            ToDoubleFunction<Integer[]> costFunction, double maxGenerations, double maxStagnation,
            int maxPopulationSize, int eliteSize, double mutationRate, double temperature,
            int maxTime, double localSearchIterations, double localSearchDepth,
            double lsTemperature, int childrenPerGeneration) {
        System.out.println("Genetic algorithm started");
        System.out.println("Parameters:" + "\n\tmaxGenerations: " + maxGenerations
                + "\n\tmaxPopulationSize: " + maxPopulationSize + "\n\teliteSize: " + eliteSize
                + "\n\tmutationRate: " + mutationRate + "\n\ttemperature: " + temperature
                + "\n\tmaxTime: " + maxTime + "\n\tlocalSearchIterations: " + localSearchIterations
                + "\n\tlocalSearchDepth: " + localSearchDepth + "\n\tlsTemperature: "
                + lsTemperature);
        System.out.println(
                "Generation    Lowest Cost      Average Cost      Median Cost   Stagnation   Elapsed time   Remaining time   Generations / s");
        double startTime = System.currentTimeMillis() / 1000.0;
        Integer[][] population = initPopulation;
        int generation = 0;
        int stagnation = 0;
        double lowestCost = Double.POSITIVE_INFINITY;
        Integer[] bestIndividual = null;
        double timeLast = 0;
        int gensLast = 0;
        while (generation < maxGenerations && stagnation < maxStagnation
                && (System.currentTimeMillis() / 1000.0 - startTime) < maxTime) {
            sortOn(population, costFunction);
            if (costFunction.applyAsDouble(population[0]) < lowestCost) {
                lowestCost = costFunction.applyAsDouble(population[0]);
                bestIndividual = population[0];
                stagnation = 0;
            } else {
                stagnation++;
            }
            double[] weights = softmax(Arrays.stream(population)
                    .mapToDouble((x) -> -costFunction.applyAsDouble(x)).toArray(), temperature);
            double[] cumWeights = new double[weights.length];
            cumWeights[0] = weights[0];
            for (int i = 1; i < weights.length; i++) {
                cumWeights[i] = cumWeights[i - 1] + weights[i];
            }
            Integer[][] newPopulation =
                    new Integer[maxPopulationSize + childrenPerGeneration][population[0].length];
            // Reproduction
            for (int i = 0; i < maxPopulationSize; i++) {
                newPopulation[i] = population[i];
            }
            for (int i = maxPopulationSize; i < maxPopulationSize + childrenPerGeneration; i++) {
                Integer[] parent = population[cumWeightChoice(cumWeights)];
                Integer[] child = parent.clone();
                if (Math.random() < mutationRate) {
                    child = mutationOperators.get((int) (Math.random() * mutationOperators.size()))
                            .apply(child);
                }
                newPopulation[i] = child;
            }

            sortOn(newPopulation, costFunction);
            weights = softmax(Arrays.stream(newPopulation)
                    .mapToDouble((x) -> -costFunction.applyAsDouble(x)).skip(eliteSize).toArray(),
                    temperature);

            for (int i = 0; i < eliteSize; i++) {
                population[i] = newPopulation[i];
            }
            Integer[][] selection = weightedSample(
                    Arrays.copyOfRange(newPopulation, eliteSize, newPopulation.length), weights,
                    maxPopulationSize - eliteSize);

            for (int i = 0; i < selection.length; i++) {
                population[i + eliteSize] = selection[i];
            }
            generation++;

            // Local search
            if (generation % localSearchIterations == 0) {
                for (int i = 0; i < eliteSize; i++) {
                    Integer[] candidate = localSearch(population[i], mutationOperators,
                            costFunction, localSearchDepth, lsTemperature);
                    if (costFunction.applyAsDouble(candidate) < costFunction
                            .applyAsDouble(population[i])) {
                        population[i] = candidate;
                    }
                }
            }
            if (System.currentTimeMillis() / 1000.0 - timeLast >= 0.1) {
                double timePerGen = (System.currentTimeMillis() / 1000.0 - startTime) / generation;
                double totalTime = Math.min(maxTime, timePerGen * maxGenerations);
                double timeLeft = totalTime - (System.currentTimeMillis() / 1000.0 - startTime);
                double gensPerSec =
                        (generation - gensLast) / (System.currentTimeMillis() / 1000.0 - timeLast);
                gensLast = generation;
                timeLast = System.currentTimeMillis() / 1000.0;
                System.out.printf("\r%10d %14.2f %17.2f %16.2f %12d %14s %16s %17.2f      ",
                        generation, costFunction.applyAsDouble(population[0]),
                        Arrays.stream(population).mapToDouble(costFunction).average().getAsDouble(),
                        Arrays.stream(population).mapToDouble(costFunction).sorted()
                                .skip(population.length / 2).findFirst().getAsDouble(),
                        stagnation, secondsToTime(System.currentTimeMillis() / 1000.0 - startTime),
                        secondsToTime(timeLeft), gensPerSec);
            }

        }
        return bestIndividual;
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
        Integer[] solution = geneticAlgorithm(initPopulation(200, coords.length),
                Arrays.asList(GeneticOperators::segmentSwap, GeneticOperators::swap,
                        GeneticOperators::rotate, GeneticOperators::reverse,
                        GeneticOperators::displace, GeneticOperators::insert,
                        GeneticOperators::reverseDisplace),
                (x) -> penalizedPathCost(x, coords, acutePenalty), 1e6, Double.POSITIVE_INFINITY,
                200, 50, 1, 5e4, 60, 1e3, 1e3, 1e-3, 200);
        System.out.println();
        System.out.println(Arrays.toString(solution));
    }
}

