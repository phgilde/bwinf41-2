import math
import random
from matplotlib import pyplot as plt
import tqdm


def acute(p1, p2, p3):
    v1 = (p1[0] - p2[0], p1[1] - p2[1])
    v2 = (p3[0] - p2[0], p3[1] - p2[1])

    if (v1[0] * v2[0] + v1[1] * v2[1]) / (
        (v1[0] ** 2 + v1[1] ** 2) ** 0.5 * (v2[0] ** 2 + v2[1] ** 2) ** 0.5
    ) > 0:
        return True
    return False


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


def neighbor(solution):
    solution = solution.copy()
    i = random.randint(0, len(solution) - 1)
    j = random.randint(0, len(solution) - 1)
    solution[i], solution[j] = solution[j], solution[i]
    return solution


def simulated_annealing(coords, acute_penalty, temp_func, steps, neighbor_prob=0.2):
    current = sorted(list(range(len(coords))), key=lambda x: random.random())
    cost_hist = [0]
    for i in tqdm.tqdm(range(steps)):
        alternative = neighbor(current)
        while neighbor_prob > random.random():
            alternative = neighbor(alternative)
        delta = cost_func(alternative, coords, acute_penalty) - cost_func(
            current, coords, acute_penalty
        )
        cost_hist.append(cost_func(current, coords, acute_penalty))
        if delta <= 0:
            current = alternative
        else:
            if random.random() < math.exp(-delta / temp_func(i)):
                current = alternative
    return current, cost_hist


# oberere Schranke für die länge einer route
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


def main():
    points = []
    with open(input("Pfad zur Datei: ")) as f:
        while line := f.readline():
            points.append(tuple(map(float, line.split())))

    plt.figure(figsize=(10, 10))
    plt.scatter([p[0] for p in points], [p[1] for p in points])
    plt.show()

    acute_penalty = length_upper_bound(points)
    print(acute_penalty)
    temp_func = lambda x: 50000 * .5 ** (x / 10_000)
    steps = 100_000
    print(math.exp(-acute_penalty / temp_func(90_000)))
    plt.plot([temp_func(i) for i in range(steps)], "b.")
    plt.show()
    solution, cost_hist = simulated_annealing(points, acute_penalty, temp_func, steps, 0.7)
    plt.plot(cost_hist, "b.")
    plt.show()
    plt.figure(figsize=(10, 10))
    plt.scatter([p[0] for p in points], [p[1] for p in points])
    plt.plot(
        [points[solution[i]][0] for i in range(len(solution))],
        [points[solution[i]][1] for i in range(len(solution))],
    )
    plt.show()


if __name__ == "__main__":
    main()
