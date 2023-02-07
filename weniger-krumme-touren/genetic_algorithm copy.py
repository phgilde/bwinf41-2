import cProfile
import pstats
import random
import time

from matplotlib import pyplot as plt
import numpy as np
from datetime import timedelta
from functools import lru_cache
from ga_operators import (
    segment_swap,
    swap,
    rotate,
    reverse,
    displace,
    insert,
    reverse_displace,
    OX1,
    OX2,
)


def softmax(x, temperature=1.0):
    e_x = np.exp((x - np.max(x)) / temperature)
    return e_x / e_x.sum()


def weighted_sample_without_replacement(population, weights, k, rng=random):
    v = [rng.random() ** (1 / (w + 0.000001)) for w in weights]
    order = sorted(range(len(population)), key=lambda i: v[i])
    return [population[i] for i in order[-k:]]


def genetic_algorithm(
    init_population,
    mutation_operators,
    crossover_operators,
    cost_function,
    max_generations,
    max_population_size,
    max_stagnation,
    elite_size,
    mutation_rate,
    crossover_rate,
    verbose,
    max_time,
    temperature,
    children_per_generation,
    cooling_rate,
):
    if verbose:
        print("Genetic algorithm ii.")
        print(
            "Parameters:",
            f"max_generations={max_generations} max_population_size={max_population_size} max_stagnation={max_stagnation} elite_size={elite_size} mutation_rate={mutation_rate} crossover_rate={crossover_rate} temperature={temperature}",
        )
        print(
            "Generation    Lowest Cost      Average Cost      Median Cost   Stagnation   Elapsed time   Remaining time   Generations / s   Temperature"
        )
    start = time.time()
    fitness_history = []
    population = init_population
    generation = 0
    stagnation = 0
    lowest_cost = float("inf")
    best_individual = None
    time_last = 0
    gens_last = 0
    try:
        while (
            generation < max_generations
            and stagnation < max_stagnation
            and time.time() - start < max_time
        ):
            population = sorted(population, key=cost_function)
            if cost_function(population[0]) < lowest_cost:
                lowest_cost = cost_function(population[0])
                best_individual = population[0]
                stagnation = 0
            else:
                stagnation += 1
            fitness_history.append(lowest_cost)
            new_population = set(population)
            population = list(population)
            weights = softmax(
                np.array([-cost_function(x) for x in population]), temperature
            )
            cum_weights = np.cumsum(weights)
            for i in range(children_per_generation):
                parent1, parent2 = random.choices(
                    population, cum_weights=cum_weights, k=2
                )
                parent1, parent2 = list(parent1), list(parent2)
                if crossover_rate > random.random():
                    child = random.choice(crossover_operators)(parent1, parent2)
                else:
                    child = parent1.copy()
                if random.random() < mutation_rate:
                    child = random.choice(mutation_operators)(child)
                new_population.add(tuple(child))

            new_population = sorted(list(new_population), key=cost_function)
            weights = softmax(
                np.array([-cost_function(x) for x in new_population[elite_size:]]),
                temperature,
            )
            cum_weights = np.cumsum(weights)
            population = new_population[
                :elite_size
            ] + weighted_sample_without_replacement(
                new_population[elite_size:],
                weights,
                max_population_size - elite_size,
                rng=random,
            )
            population = list(population)
            generation += 1
            temperature *= cooling_rate
            if verbose and time.time() - time_last >= 0.5:
                time_per_generation = (time.time() - start) / generation
                total_time = min(time_per_generation * max_generations, max_time)
                remaining_time = total_time - (time.time() - start)
                gens_ps = (generation - gens_last) / (time.time() - time_last)
                gens_last = generation
                time_last = time.time()
                print(
                    "\r",
                    f"{generation:>9}",
                    f"{min([cost_function(x) for x in population]):>14.2f}",
                    f"{sum([cost_function(x) for x in population]) / len(population):>17.2f}",
                    f"{sorted([cost_function(x) for x in population])[len(population) // 2]:>16.2f}"
                    f"{stagnation:>13}",
                    f"{str(timedelta(seconds=int(time.time() - start))):>14}"
                    f"{str(timedelta(seconds=int(remaining_time))):>17}",
                    f"{gens_ps:>17.2f}",
                    f"{temperature:>13.2f}     ",
                    end="",
                )
    except KeyboardInterrupt:
        print("\nInterrupted by user")

    return best_individual, fitness_history


@lru_cache(maxsize=100000)
def penalized_path_cost(solution, coords, acute_penalty):
    p1 = coords[solution[0]]
    p2 = coords[solution[1]]
    cost = ((p1[0] - p2[0]) ** 2 + (p1[1] - p2[1]) ** 2) ** 0.5
    for k in solution[2:]:
        p3 = coords[k]
        if acute(p1, p2, p3):
            cost += acute_penalty
        p1 = p2
        p2 = p3
        cost += ((p1[0] - p2[0]) ** 2 + (p1[1] - p2[1]) ** 2) ** 0.5

    return cost


@lru_cache(maxsize=100000)
def acute(p1, p2, p3):
    v1 = (p1[0] - p2[0], p1[1] - p2[1])
    v2 = (p3[0] - p2[0], p3[1] - p2[1])

    return (v1[0] * v2[0] + v1[1] * v2[1]) / (
        (v1[0] ** 2 + v1[1] ** 2) ** 0.5 * (v2[0] ** 2 + v2[1] ** 2) ** 0.5
    ) > 0


def length_upper_bound(coords):
    distances = sorted(
        [
            ((p1[0] - p2[0]) ** 2 + (p1[1] - p2[1]) ** 2) ** 0.5
            for p1 in coords
            for p2 in coords
        ],
        key=lambda x: -x,
    )
    return sum(distances[: len(coords)])


def POS(parent1, parent2):
    pass


def ERX(parent1, parent2):
    pass


def init_population(population_size, individual_size):
    population = []
    for _ in range(population_size):
        individual = tuple(
            sorted(tuple(range(individual_size)), key=lambda x: random.random())
        )
        population.append(individual)
    return population


def main():
    points = []
    with open(input("Pfad zur Datei: ")) as f:
        while line := f.readline():
            points.append(tuple(map(float, line.split())))
    points = tuple(points)
    min_coord = min((min([p[0] for p in points]), min([p[1] for p in points])))
    max_coord = max((max([p[0] for p in points]), max([p[1] for p in points])))
    plt.figure(figsize=(10, 10))
    plt.xlim(min_coord - 50, max_coord + 50)
    plt.ylim(min_coord - 50, max_coord + 50)
    plt.scatter([p[0] for p in points], [p[1] for p in points])
    plt.show()

    acute_penalty = length_upper_bound(points)
    print(acute_penalty)
    cost_func = lambda solution: penalized_path_cost(
        tuple(solution), points, acute_penalty
    )
    solution, cost_hist = genetic_algorithm(
        init_population=init_population(300, len(points)),
        cost_function=cost_func,
        mutation_operators=[
            segment_swap,
            swap,
            rotate,
            reverse,
            displace,
            insert,
            reverse_displace,
        ],
        crossover_operators=[OX1, OX2],
        max_generations=float("inf"),
        max_population_size=100,
        elite_size=5,
        mutation_rate=1.0,
        crossover_rate=0.0,
        max_stagnation=20_000,
        verbose=True,
        max_time=60 * 10,
        temperature=50_000,
        children_per_generation=100,
        cooling_rate=0.9999,
    )
    # logaritmic scale
    plt.yscale("log")
    plt.plot(cost_hist, "b.")
    plt.show()
    plt.figure(figsize=(10, 10))
    plt.yscale("linear")
    plt.xlim(min_coord - 50, max_coord + 50)
    plt.ylim(min_coord - 50, max_coord + 50)
    plt.scatter([p[0] for p in points], [p[1] for p in points])
    plt.plot(
        [points[solution[i]][0] for i in range(len(solution))],
        [points[solution[i]][1] for i in range(len(solution))],
    )
    plt.show()


if __name__ == "__main__":

    cProfile.run("main()", "restats")

    p = pstats.Stats("restats")
    p.strip_dirs().sort_stats("time").print_stats(10)
