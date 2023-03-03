from time import sleep
from typing import List, Tuple
from random import seed, randint
from itertools import product
from math import sqrt
from matplotlib import pyplot as plt
import networkx as nx
from mip import (
    Model,
    xsum,
    BINARY,
    minimize,
    ConstrsGenerator,
    CutPool,
    OptimizationStatus,
    CBC,
)
import sim_ann
import java_interface


def edge(a, b):
    return (a, b) if a < b else (b, a)


# checks if the angle between the three points is acute
def acute(p1, p2, p3):
    v1 = (p1[0] - p2[0], p1[1] - p2[1])
    v2 = (p3[0] - p2[0], p3[1] - p2[1])

    if (v1[0] * v2[0] + v1[1] * v2[1]) / (
        (v1[0] ** 2 + v1[1] ** 2) ** 0.5 * (v2[0] ** 2 + v2[1] ** 2) ** 0.5
    ) > 0:
        return True
    return False


def tsp_instance(n: int, c: List[List[int]], points: List[Tuple[int, int]]):
    class SubTourCutGenerator(ConstrsGenerator):
        """Class to generate cutting planes for the TSP"""

        def __init__(self, x_, V_, c_, name, use_johnson):
            self.x, self.V, self.c = x_, V_, c_
            self.name = name
            self.use_johnson = use_johnson

        def generate_constrs(self, model: Model, depth: int = 0, npass: int = 0):
            xf, V_, G = model.translate(self.x), self.V, nx.Graph()
            for (u, v) in [
                (k, l) for (k, l) in product(V_, V_) if k < l and xf[(k, l)].x > 0.01
            ]:
                G.add_edge(u, v, capacity=xf[(u, v)].x)
            # show the graph
            # nx.draw(G, with_labels=True, pos=points)
            # plt.show()
            try:
                cycle = nx.algorithms.cycles.find_cycle(G, orientation="ignore")
                cut = (
                    xsum(xf[edge(u, v)] for u,v,_ in cycle)
                    <= len(cycle) - 1
                )
                model += cut
            except nx.NetworkXNoCycle:
                pass
            
            if self.use_johnson:
                # let node1 and node2 be the nodes with highest cost in G between them
                D = nx.algorithms.shortest_paths.weighted.johnson(G, weight="capacity")
                node1, node2 = max(
                    product(V_, V_),
                    key=lambda x: nx.path_weight(G, D[x[0]].get(x[1], [0]), "capacity") if x[0] != x[1] else 0,
                )
            else:
                node1, node2 = max(product(V_, V_), key=lambda x: c[x[0]][x[1]] if x[0] != x[1] else 0)
            

            # find minimum cut between node1 and node2
            cut_value, (T, TS) = nx.algorithms.flow.minimum_cut(
                G, node1, node2, capacity="capacity"
            )
            if cut_value < 1 - 1e-6:
                cut = xsum(xf[edge(u, v)] for u, v in G.subgraph(TS).edges) <= len(G.subgraph(TS).edges) - 1
                model += cut


    V = set(range(n))
    Arcs = [(i, j) for (i, j) in product(V, V) if i < j]

    model = Model()
    model.threads = 4

    # binary variables indicating if arc (i,j) is used on the route or not
    x = {arc: model.add_var(name=f"Arc {arc}", var_type=BINARY) for arc in Arcs}
    ends = [model.add_var(name=f"End {i}", var_type=BINARY) for i in V]
    # objective function: minimize the distance
    model.objective = minimize(xsum(c[i][j] * x[(i, j)] for (i, j) in Arcs))

    # constraint : leave each city only once
    for i in V:
        model += xsum(x[(j, k)] for j, k in Arcs if i in (j, k)) + ends[i] == 2

    model += xsum(ends) == 2

    # computing farthest point for each point, these will be checked first for
    # isolated subtours
    F = []
    for i in V:
        f = max(V, key=lambda j: c[i][j])
        F.append((i, f))

    model.cuts_generator = SubTourCutGenerator(x, V, "cuts_generator", c, False)
    model.lazy_constrs_generator = SubTourCutGenerator(x, V, "lazy_constrs_generator", c, False)
    return model, x, ends


points = []
with open(path := input("Pfad zur Datei: ")) as f:
    while line := f.readline():
        points.append(tuple(map(float, line.split())))

main_time = float(input("Zeit für MIP-Löser in Minuten: "))
# plot points
plt.figure(figsize=(10, 10))
min_coord = min((min([p[0] for p in points]), min([p[1] for p in points])))
max_coord = max((max([p[0] for p in points]), max([p[1] for p in points])))
plt.xlim(min_coord - 50, max_coord + 50)
plt.ylim(min_coord - 50, max_coord + 50)
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

# create problem
model, x, ends = tsp_instance(len(points), weight_matrix, points)

# acute angle constraint
for i in range(len(points)):
    for j in range(0, len(points)):
            if i != j:
                for k in range(0, len(points)):
                    if i != k and j != k:
                        if acute(points[i], points[j], points[k]):
                            model += x[edge(i, j)] + x[edge(j, k)] <= 1
print("Suche Startlösung...")

init_solution = java_interface.solveTA(path)
if False:
    # plot initial solution
    plt.figure(figsize=(10, 10))
    plt.xlim(min_coord - 50, max_coord + 50)
    plt.ylim(min_coord - 50, max_coord + 50)
    plt.scatter([p[0] for p in points], [p[1] for p in points])
    for i in range(len(init_solution) - 1):
        plt.plot(
            [points[init_solution[i]][0], points[init_solution[i + 1]][0]],
            [points[init_solution[i]][1], points[init_solution[i + 1]][1]],
            color="red",
        )
    plt.show()
if True:
    p1 = init_solution[0]
    start = []
    for p2 in init_solution[1:]:
        start.append((x[edge(p1, p2)], 1.0))
        p1 = p2
    start.append((ends[init_solution[0]], 1.0))
    start.append((ends[init_solution[-1]], 1.0))
    model.start = start
print(model.validate_mip_start())

print("Suche optimale Lösung...")
model.emphasis = 2
model.optimize(max_seconds=main_time * 60)
print(model.status)
import winsound
winsound.MessageBeep()

if model.status in (OptimizationStatus.OPTIMAL, OptimizationStatus.FEASIBLE):
    print("Lösung gefunden!")
    
    plt.figure(figsize=(10, 10))
    min_coord = min((min([p[0] for p in points]), min([p[1] for p in points])))
    max_coord = max((max([p[0] for p in points]), max([p[1] for p in points])))
    plt.xlim(min_coord - 50, max_coord + 50)
    plt.ylim(min_coord - 50, max_coord + 50)
    plt.scatter([p[0] for p in points], [p[1] for p in points])
    for i in range(len(points)):
        for j in range(i + 1, len(points)):
            if x[(i, j)].x == 1:
                plt.plot(
                    [points[i][0], points[j][0]], [points[i][1], points[j][1]], "r"
                )
    plt.show()
if model.status == OptimizationStatus.NO_SOLUTION_FOUND:
    print("Keine weitere Lösung gefunden!")
if model.status == OptimizationStatus.INFEASIBLE:
    print("Startlösung ist optimal!")
