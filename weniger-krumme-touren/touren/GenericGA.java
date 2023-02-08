package touren;

import java.util.Arrays;
import java.util.function.BiFunction;
import java.util.function.Function;

public class GenericGA<T> {
    static double[] softmax(double[] x, double temperature) {
        double[] y = new double[x.length];
        double sum = 0;
        for (int i = 0; i < x.length; i++) {
            y[i] = Math.exp(x[i] / temperature);
            sum += y[i];
        }
        for (int i = 0; i < x.length; i++) {
            y[i] /= sum;
        }
        return y;
    }

    static <S> void sortOn(S[] arr, Function<S, Double> cost) {
        Arrays.sort(arr, (a, b) -> Double.compare(cost.apply(a), cost.apply(b)));
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

    private T[] population;
    private Function<T, T> mutation;
    private Function<T, Double> cost;
    private double mutationRate;
    private double crossoverRate;
    private double temperature;
    private int eliteSize;
    private BiFunction<T, T, T> crossover;

    public GenericGA(T[] population, BiFunction<T, T, T> crossover, Function<T, T> mutation, Function<T, Double> cost,
            double mutationRate, double crossoverRate, double temperature, int eliteSize) {
        this.population = population;
        this.mutation = mutation;
        this.crossover = crossover;
        this.cost = cost;
        this.mutationRate = mutationRate;
        this.temperature = temperature;
        this.eliteSize = eliteSize;
    }

    public T[] getPopulation() {
        return population;
    }

    public void step() {
        sortOn(population, cost);
        double[] weights = softmax(
                Arrays.stream(population).mapToDouble(x -> -cost.apply(x)).toArray(), temperature);
        double[] cumWeights = new double[weights.length];
        cumWeights[0] = weights[0];
        for (int i = 1; i < weights.length; i++) {
            cumWeights[i] = cumWeights[i - 1] + weights[i];
        }
        T[] newPopulation = Arrays.copyOf(population, population.length);
        for (int i = 0; i < eliteSize; i++) {
            newPopulation[i] = population[i];
        }
        for (int i = eliteSize; i < population.length; i++) {
            T parent1 = population[cumWeightChoice(cumWeights)];
            T parent2 = population[cumWeightChoice(cumWeights)];
            newPopulation[i] = parent1;
            if (Math.random() < crossoverRate) {
                newPopulation[i] = crossover.apply(parent1, parent2);
            }
            if (Math.random() < mutationRate) {
                newPopulation[i] = mutation.apply(newPopulation[i]);
            }
        }
    }
}
