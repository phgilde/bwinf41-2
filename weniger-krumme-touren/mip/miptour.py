from typing import List, Tuple
from random import seed, randint
from itertools import product
from math import sqrt
from matplotlib import pyplot as plt
import networkx as nx
from mip import Model, xsum, BINARY, minimize, ConstrsGenerator, CutPool, OptimizationStatus
import sim_ann

# checks if the angle between the three points is acute
def acute(p1, p2, p3):
    v1 = (p1[0] - p2[0], p1[1] - p2[1])
    v2 = (p3[0] - p2[0], p3[1] - p2[1])

    if (v1[0] * v2[0] + v1[1] * v2[1]) / (
        (v1[0] ** 2 + v1[1] ** 2) ** 0.5 * (v2[0] ** 2 + v2[1] ** 2) ** 0.5
    ) > 0:
        return True
    return False


def tsp_instance(n: int, c: List[List[int]]):
    class SubTourCutGenerator(ConstrsGenerator):
        """Class to generate cutting planes for the TSP"""

        def __init__(self, Fl: List[Tuple[int, int]], x_, V_):
            self.F, self.x, self.V = Fl, x_, V_

        def generate_constrs(self, model: Model, depth: int = 0, npass: int = 0):
            xf, V_, cp, G = model.translate(self.x), self.V, CutPool(), nx.DiGraph()
            for (u, v) in [(k, l) for (k, l) in product(V_, V_) if k != l and xf[k][l]]:
                G.add_edge(u, v, capacity=xf[u][v].x)
            for (u, v) in F:
                val, (S, NS) = nx.minimum_cut(G, u, v)
                if val <= 0.99:
                    aInS = [
                        (xf[i][j], xf[i][j].x)
                        for (i, j) in product(V_, V_)
                        if i != j and xf[i][j] and i in S and j in S
                    ]
                    if sum(f for v, f in aInS) >= (len(S) - 1) + 1e-4:
                        cut = xsum(1.0 * v for v, fm in aInS) <= len(S) - 1
                        cp.add(cut)
                        if len(cp.cuts) > 256:
                            for cut in cp.cuts:
                                model += cut
                            return
            for cut in cp.cuts:
                model += cut

    V = set(range(n))
    Arcs = [(i, j) for (i, j) in product(V, V) if i != j]

    model = Model()

    # binary variables indicating if arc (i,j) is used on the route or not
    x = [[model.add_var(var_type=BINARY) for j in V] for i in V]

    # objective function: minimize the distance
    model.objective = minimize(xsum(c[i][j] * x[i][j] for (i, j) in Arcs))

    # constraint : leave each city only once
    for i in V:
        model += xsum(x[i][j] for j in V - {i}) == 1

    # constraint : enter each city only once
    for i in V:
        model += xsum(x[j][i] for j in V - {i}) == 1

    # no subtours of size 2
    for (i, j) in Arcs:
        model += x[i][j] + x[j][i] <= 1

    # computing farthest point for each point, these will be checked first for
    # isolated subtours
    F, G = [], nx.DiGraph()
    for (i, j) in Arcs:
        G.add_edge(i, j, weight=c[i][j])
    for i in V:
        P, D = nx.dijkstra_predecessor_and_distance(G, source=i)
        DS = list(D.items())
        DS.sort(key=lambda x: x[1])
        F.append((i, DS[-1][0]))

    model.cuts_generator = SubTourCutGenerator(F, x, V)
    model.lazy_constrs_generator = SubTourCutGenerator(F, x, V)
    return model, x


points = []
with open(input("Pfad zur Datei: ")) as f:
    while line := f.readline():
        points.append(tuple(map(float, line.split())))

# plot points
plt.figure(figsize=(10, 10))
plt.scatter([p[0] for p in points], [p[1] for p in points])
plt.show()

print("Modell wird erstellt...")

# create weight matrix
weight_matrix = []
for i in range(len(points)):
    weight_matrix.append([])
    for j in range(len(points)):
        weight_matrix[i].append(
            ((points[i][0] - points[j][0]) ** 2 + (points[i][1] - points[j][1]) ** 2)
            ** 0.5
        )
    weight_matrix[i].append(0)
weight_matrix.append([0] * (len(points) + 1))

# create problem
model, x = tsp_instance(len(points) + 1, weight_matrix)

# acute angle constraint
for i in range(len(points)):
    for j in range(len(points)):
        if i != j:
            for k in range(len(points)):
                if i != k and j != k:
                    if acute(points[i], points[j], points[k]):
                        model += x[i][j] + x[j][k] <= 1

print("Suche Startlösung...")

init_solution, succ, cost = sim_ann.solve(points, verbose=True)
if succ:
    print(f"Startlösung gefunden mit Kosten {cost}!")
    # plot initial solution
    plt.figure(figsize=(10, 10))
    plt.scatter([p[0] for p in points], [p[1] for p in points])
    for i in range(len(init_solution) - 1):
        plt.plot(
            [points[init_solution[i]][0], points[init_solution[i + 1]][0]],
            [points[init_solution[i]][1], points[init_solution[i + 1]][1]],
            color="red",
        )
    plt.show()


    p1 = init_solution[0]
    start = []
    for p2 in init_solution[1:]:
        start.append((x[p1][p2], 1.0))
        p1 = p2
    start.append((x[-1][init_solution[0]], 1.0))
    start.append((x[init_solution[-1]][-1], 1.0))
    model.start = start

print("Suche optimale Lösung...")
model.optimize(max_seconds=60*float(input("Zeit in Minuten: ")))
print(model.status)

if model.status in (OptimizationStatus.OPTIMAL, OptimizationStatus.FEASIBLE):
    plt.figure(figsize=(10, 10))
    plt.scatter([p[0] for p in points], [p[1] for p in points])
    for i in range(len(points)):
        for j in range(len(points)):
            if x[i][j].x == 1:
                plt.plot(
                    [points[i][0], points[j][0]], [points[i][1], points[j][1]], "r"
                )
    plt.show()
if model.status == OptimizationStatus.NO_SOLUTION_FOUND:
    print("Keine weitere Lösung gefunden!")