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
import sys
import java_interface
import time


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
            self.count = 0
            self.time_cycle = 0
            self.time_components = 0
            self.time_cut = 0

        def generate_constrs(self, model: Model, depth: int = 0, npass: int = 0):
            xf, V_, G = model.translate(self.x), self.V, nx.Graph()
            for (u, v) in [
                (k, l) for (k, l) in product(V_, V_) if k < l and xf[(k, l)] is not None
            ]:
                if xf[(u, v)].x > 0.01:
                    G.add_edge(u, v, capacity=xf[(u, v)].x)
            if (
                len(
                    list(
                        filter(
                            lambda e: e[0] in G.nodes and e[1] in G.nodes,
                            product(V_, V_),
                        )
                    )
                )
                == 0
            ):
                return
            # show the graph
            # nx.draw(G, with_labels=True, pos=points)
            # plt.show()
            try:
                start = time.time()
                cycle = nx.algorithms.cycles.find_cycle(G, orientation="ignore")
                self.time_cycle += time.time() - start
                S = {u for u, _, _ in cycle}
                if sum(xf[edge(u, v)].x for u in S for v in S if u < v) > len(S) - 1:
                    cut = xsum(xf[edge(u, v)] for u in S for v in S if u < v) <= len(S) - 1
                    model += cut
            except nx.NetworkXNoCycle:
                pass
            start = time.time()
            components = list(nx.algorithms.components.connected_components(G))
            self.time_components += time.time() - start
            if len(components) == 1:
                start = time.time()
                cut_value, (
                    S,
                    ST,
                ) = nx.algorithms.connectivity.stoerwagner.stoer_wagner(G)
                self.time_cut += time.time() - start
                if len(S) == 1 or len(ST) == 1:
                    return
                if cut_value < 1 - 1e-6:
                    if (
                        sum(xf[edge(u, v)].x for u in S for v in S if u < v)
                        > len(S) - 1
                    ):
                        cut = (
                            xsum(xf[edge(u, v)] for u in S for v in S if u < v)
                            <= len(S) - 1
                        )
                        model += cut
                    if (
                        sum(xf[edge(u, v)].x for u in ST for v in ST if u < v)
                        > len(ST) - 1
                    ):
                        cut = (
                            xsum(xf[edge(u, v)] for u in ST for v in ST if u < v)
                            <= len(ST) - 1
                        )
                        model += cut
            else:
                for component in components:
                    if len(component) == 1:
                        continue
                    if (
                        sum(xf[edge(u, v)].x for u in component for v in component if u < v)
                        > len(component) - 1
                    ):
                        cut = (
                            xsum(
                                xf[edge(u, v)]
                                for u in component
                                for v in component
                                if u < v
                            )
                            <= len(component) - 1
                        )
                        model += cut

    V = set(range(n))
    Arcs = [(i, j) for (i, j) in product(V, V) if i < j]

    model = Model()

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

    model.cuts_generator = SubTourCutGenerator(x, V, c, "cuts_generator", False)
    model.lazy_constrs_generator = SubTourCutGenerator(
        x, V, "lazy_constrs_generator", c, True
    )
    return model, x, ends


points = []
with open(path := sys.argv[1]) as f:
    while line := f.readline():
        points.append(tuple(map(float, line.split())))


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
model.optimize(max_seconds=float("inf"))
print(model.status)
import winsound

winsound.MessageBeep()
print(path)
if model.status in (OptimizationStatus.OPTIMAL, OptimizationStatus.FEASIBLE):
    print("Lösung gefunden!")

if model.status == OptimizationStatus.NO_SOLUTION_FOUND:
    print("Keine weitere Lösung gefunden!")
if model.status == OptimizationStatus.INFEASIBLE:
    print("Startlösung ist optimal!")

print("Cuts: ", model.cuts_generator.time_cut)
print("Components: ", model.cuts_generator.time_components)
print("Cycle: ", model.cuts_generator.time_cycle)