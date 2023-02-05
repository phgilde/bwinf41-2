import random
import time

from matplotlib import pyplot as plt
import numpy as np
from datetime import timedelta


def softmax(x):
    e_x = np.exp(x - np.max(x))
    return e_x / e_x.sum()


def genetic_algorithm(
    init_population,
    mutation_operators,
    crossover_operators,
    fitness_function,
    max_generations,
    max_population_size,
    max_stagnation,
    elite_size,
    mutation_rate,
    crossover_rate,
    verbose,
    max_time,
):
    if verbose:
        print(
            "Generation   Best fitness   Average fitness   Median fitness   Stagnation   Elapsed time   Remaining time"
        )
    start = time.time()
    fitness_history = []
    population = init_population
    generation = 0
    stagnation = 0
    best_fitness = -float("inf")
    best_individual = None
    while (
        generation < max_generations
        and stagnation < max_stagnation
        and time.time() - start < max_time
    ):
        population = sorted(population, key=lambda x: fitness_function(x), reverse=True)
        if fitness_function(population[0]) > best_fitness:
            best_fitness = fitness_function(population[0])
            best_individual = population[0]
            stagnation = 0
        else:
            stagnation += 1
        fitness_history.append(best_fitness)
        new_population = []
        new_population = population[:elite_size]
        weights = softmax([fitness_function(x) for x in population])
        while len(new_population) < max_population_size:
            parent1, parent2 = random.choices(population, weights=weights, k=2)
            if crossover_rate > random.random():
                child = random.choice(crossover_operators)(parent1, parent2)
            else:
                child = parent1.copy()
            while random.random() < mutation_rate:
                child = random.choice(mutation_operators)(child)
            new_population.append(child)
        population = new_population
        generation += 1
        if verbose:
            time_per_generation = (time.time() - start) / generation
            total_time = min(time_per_generation * max_generations, max_time)
            remaining_time = total_time - (time.time() - start)

            print(
                "\r",
                f"{generation:>9}",
                f"{max([fitness_function(x) for x in population]):>14.2f}",
                f"{sum([fitness_function(x) for x in population]) / len(population):>17.2f}",
                f"{sorted([fitness_function(x) for x in population])[len(population) // 2]:>16.2f}"
                f"{stagnation:>13}",
                f"{str(timedelta(seconds=int(time.time() - start))):>14}"
                f"{str(timedelta(seconds=int(remaining_time))):>17}             ",
                end="",
            )
    return best_individual, fitness_history


def segment_swap(individual):
    individual = individual.copy()
    i = random.randint(0, len(individual) - 4)
    j = random.randint(i, len(individual) - 3)
    k = random.randint(j + 1, len(individual) - 2)
    l = random.randint(k + 1, len(individual) - 1)
    individual = (
        individual[:i]
        + individual[k:l]
        + individual[j:k]
        + individual[i:j]
        + individual[l:]
    )
    return individual


def swap(individual):
    individual = individual.copy()
    i = random.randint(0, len(individual) - 2)
    j = random.randint(i, len(individual) - 1)
    individual[i], individual[j] = individual[j], individual[i]
    return individual


def rotate(individual):
    individual = individual.copy()
    i = random.randint(0, len(individual))
    individual = individual[i:] + individual[:i]
    return individual


def reverse(individual):
    individual = individual.copy()
    i = random.randint(0, len(individual) - 1)
    j = random.randint(i, len(individual))
    individual = individual[:i] + individual[i:j][::-1] + individual[j:]
    return individual


def displace(individual):
    individual = individual.copy()
    i = random.randint(0, len(individual) - 1)
    j = random.randint(i, len(individual))
    k = random.randint(0, len(individual) - (j - i))
    result = individual[:i] + individual[j:]
    result = result[:k] + individual[i:j] + result[k:]
    return result


def insert(individual):
    individual = individual.copy()
    i = random.randint(0, len(individual) - 1)
    j = random.randint(0, len(individual) - 1)
    selected = individual[i]
    individual = individual[:i] + individual[i + 1 :]
    individual = individual[:j] + [selected] + individual[j:]
    return individual


def reverse_displace(individual):
    individual = individual.copy()
    i = random.randint(0, len(individual) - 1)
    j = random.randint(i, len(individual))
    k = random.randint(0, len(individual) - (j - i))
    result = individual[:i] + individual[j:]
    result = result[:k] + individual[i:j][::-1] + result[k:]
    return result


def cost_func(solution, coords, acute_penalty):
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


def acute(p1, p2, p3):
    v1 = (p1[0] - p2[0], p1[1] - p2[1])
    v2 = (p3[0] - p2[0], p3[1] - p2[1])

    return (v1[0] * v2[0] + v1[1] * v2[1]) / (
        (v1[0] ** 2 + v1[1] ** 2) ** 0.5 * (v2[0] ** 2 + v2[1] ** 2) ** 0.5
    ) > 0


def OX1(parent1, parent2):
    i = random.randint(0, len(parent1) - 2)
    j = random.randint(i, len(parent1) - 1)
    child = [-1 for _ in range(len(parent1))]
    child[i:j] = parent1[i:j]
    ptr = j + 1
    ch_ptr = j + 1
    while -1 in child:
        if ptr >= len(child):
            ptr = 0
        if ch_ptr >= len(child):
            ch_ptr = 0
        if parent2[ptr] not in child:
            child[ch_ptr] = parent2[ptr]
            ch_ptr += 1
        ptr += 1
    assert set(child) == set(parent1) == set(parent2)
    return child


def OX2(parent1, parent2):
    positions = []
    while random.random() < 0.8 and len(positions) < 5:
        position = random.randint(0, len(parent1) - 1)
        if position not in positions:
            positions.append(position)
    positions.sort()
    ixs = sorted([parent2.index(parent1[ix]) for ix in positions])
    child = parent2.copy()
    for ix, pos in zip(ixs, positions):
        child[ix] = parent1[pos]
    assert set(child) == set(parent1) == set(parent2)
    return child


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
        individual = sorted(list(range(individual_size)), key=lambda x: random.random())
        population.append(individual)
    return population


def main():
    points = []
    with open(input("Pfad zur Datei: ")) as f:
        while line := f.readline():
            points.append(tuple(map(float, line.split())))
    min_coord = min((min([p[0] for p in points]), min([p[1] for p in points])))
    max_coord = max((max([p[0] for p in points]), max([p[1] for p in points])))
    plt.figure(figsize=(10, 10))
    plt.xlim(min_coord - 50, max_coord + 50)
    plt.ylim(min_coord - 50, max_coord + 50)
    plt.scatter([p[0] for p in points], [p[1] for p in points])
    plt.show()

    acute_penalty = length_upper_bound(points)
    print(acute_penalty)
    fitness_func = lambda solution: -cost_func(solution, points, acute_penalty)
    solution, cost_hist = genetic_algorithm(
        init_population=init_population(300, len(points)),
        fitness_function=fitness_func,
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
        max_generations=10_000,
        max_population_size=300,
        elite_size=10,
        mutation_rate=0.5,
        crossover_rate=1,
        max_stagnation=1000,
        verbose=True,
        max_time=600,
    )
    plt.plot(cost_hist, "b.")
    plt.show()
    plt.figure(figsize=(10, 10))
    plt.xlim(min_coord - 50, max_coord + 50)
    plt.ylim(min_coord - 50, max_coord + 50)
    plt.scatter([p[0] for p in points], [p[1] for p in points])
    plt.plot(
        [points[solution[i]][0] for i in range(len(solution))],
        [points[solution[i]][1] for i in range(len(solution))],
    )
    plt.show()


if __name__ == "__main__":
    main()
