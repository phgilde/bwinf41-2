from itertools import product
import math
import random

from matplotlib import pyplot as plt
from operators import three_opt, displace, reverse_displace, insert


def simulated_annealing(
    candidate, operator, cost, iterations, temperature, cooling_rate, verbose
):
    best = candidate
    best_cost = cost(best)
    candidate_cost = best_cost
    for i in range(iterations):
        new_candidate = operator(candidate)
        new_candidate_cost = cost(new_candidate)
        if new_candidate_cost < best_cost:
            best = new_candidate
            best_cost = new_candidate_cost
            if verbose:
                print(f"New best: {best_cost} at iteration {i} with temperature {temperature}")
        else:
            if new_candidate_cost - candidate_cost < temperature:
                candidate = new_candidate
                candidate_cost = new_candidate_cost
        temperature *= cooling_rate
    return best


def cost_upper_bound(coords):
    return sum(
        sorted(
            [
                math.sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)
                for (x1, y1), (x2, y2) in product(coords, coords)
            ],
            reverse=True,
        )[: len(coords)]
    )


def acute(p1, p2, p3):
    v1 = (p1[0] - p2[0], p1[1] - p2[1])
    v2 = (p3[0] - p2[0], p3[1] - p2[1])

    if (v1[0] * v2[0] + v1[1] * v2[1]) / (
        (v1[0] ** 2 + v1[1] ** 2) ** 0.5 * (v2[0] ** 2 + v2[1] ** 2) ** 0.5
    ) > 0:
        return True
    return False


def penalized_cost(route, coords, penalty):
    p1, p2 = coords[route[0]], coords[route[1]]
    cost = math.sqrt((p1[0] - p2[0]) ** 2 + (p1[1] - p2[1]) ** 2)
    for i in range(2, len(coords)):
        p3 = coords[route[i]]
        cost += math.sqrt((p2[0] - p3[0]) ** 2 + (p2[1] - p3[1]) ** 2)
        if acute(p1, p2, p3):
            cost += penalty
        p1, p2 = p2, p3
    return cost


def solve(coords, iterations=200000, temperature=3, cooling_rate=0.99993, verbose=False):
    penalty = cost_upper_bound(coords)
    candidate = list(range(len(coords)))
    random.shuffle(candidate)
    cost_function = lambda route: penalized_cost(route, coords, penalty)
    operator = lambda route: random.choice(
        (three_opt, displace, reverse_displace, insert)
    )(route)
    solution = simulated_annealing(
        candidate,
        operator,
        cost_function,
        iterations,
        temperature * penalty,
        cooling_rate,
        verbose
    )
    return solution, cost_function(solution) < penalty, cost_function(solution)


if __name__ == "__main__":
    points = []
    with open(input("Pfad zur Datei: ")) as f:
        while line := f.readline():
            points.append(tuple(map(float, line.split())))

    # plot points
    plt.figure(figsize=(10, 10))
    plt.scatter([p[0] for p in points], [p[1] for p in points])
    plt.show()

    solution = solve(points, verbose=True)
    print(solution)

    # plot solution
    plt.figure(figsize=(10, 10))
    plt.scatter([p[0] for p in points], [p[1] for p in points])
    plt.plot(
        [points[solution[0][i]][0] for i in range(len(solution[0]))],
        [points[solution[0][i]][1] for i in range(len(solution[0]))],
    )
    plt.show()
