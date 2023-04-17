from typing import List, Tuple
from itertools import product
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

import os

java_path = "weniger-krumme-touren/build/SimulatedAnnealing.jar"


def solveTA(path):
    if not os.path.exists(path + ".solution"):
        os.system(
            f"java -cp weniger-krumme-touren/build touren.SimulatedAnnealing {path}"
        )
    with open(path + ".solution") as f:
        return tuple(map(int, f.readline()[1:-1].split(", ")))


# sortiert das paar (a, b) so, dass a < b
def edge(a, b):
    return (a, b) if a < b else (b, a)


# Prueft, ob die Punkte einen spitzen Winkel bilden
def acute(p1, p2, p3):
    v1 = (p1[0] - p2[0], p1[1] - p2[1])
    v2 = (p3[0] - p2[0], p3[1] - p2[1])

    if (v1[0] * v2[0] + v1[1] * v2[1]) / (
        (v1[0] ** 2 + v1[1] ** 2) ** 0.5 * (v2[0] ** 2 + v2[1] ** 2) ** 0.5
    ) > 0:
        return True
    return False


class SubTourCutGenerator(ConstrsGenerator):
    def __init__(self, x_, V_):
        self.x, self.V = x_, V_

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
        # Subtour-Ungleichung fuer Zykel
        try:
            cycle = nx.algorithms.cycles.find_cycle(G, orientation="ignore")
            S = {u for u, _, _ in cycle}
            if sum(xf[edge(u, v)].x for u in S for v in S if u < v) > len(S) - 1:
                cut = xsum(xf[edge(u, v)] for u in S for v in S if u < v) <= len(S) - 1
                model += cut
        except nx.NetworkXNoCycle:
            pass
        # Komponenten Suchen
        components = list(nx.algorithms.components.connected_components(G))
        if len(components) == 1:
            # Falls nur eine Komponente, Min-Cut-Ungleichung
            cut_value, (
                S,
                ST,
            ) = nx.algorithms.connectivity.stoerwagner.stoer_wagner(G)
            # Beide Komponenten muessen mindestens 2 Knoten haben
            if len(S) == 1 or len(ST) == 1:
                return
            # falls die ungleichungen verletzt werden, werden sie hinzugefuegt
            if sum(xf[edge(u, v)].x for u in S for v in S if u < v) > len(S) - 1:
                cut = xsum(xf[edge(u, v)] for u in S for v in S if u < v) <= len(S) - 1
                model += cut
            if sum(xf[edge(u, v)].x for u in ST for v in ST if u < v) > len(ST) - 1:
                cut = (
                    xsum(xf[edge(u, v)] for u in ST for v in ST if u < v) <= len(ST) - 1
                )
                model += cut
        else:
            # Falls mehrere Komponenten, Ungleichung fuer jede Komponente
            for component in components:
                # Komponenten muessen mindestens 2 Knoten haben
                if len(component) == 1:
                    continue
                # Falls die Ungleichungen verletzt werden, werden sie hinzugefuegt
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


# Erstellt ein mip-Modell der TSP-Instanz
def tsp_instance(n: int, c: List[List[int]], points: List[Tuple[int, int]]):

    V = set(range(n))
    Arcs = [(i, j) for (i, j) in product(V, V) if i < j]

    model = Model()

    # Binaere Variable fuer die Kanten
    x = {arc: model.add_var(name=f"Arc {arc}", var_type=BINARY) for arc in Arcs}
    ends = [model.add_var(name=f"End {i}", var_type=BINARY) for i in V]
    # objective function: minimize the distance
    model.objective = minimize(xsum(c[i][j] * x[(i, j)] for (i, j) in Arcs))

    # Jeder Knoten hat einen Grad von 2, ausser Anfang und Ende
    for i in V:
        model += xsum(x[(j, k)] for j, k in Arcs if i in (j, k)) + ends[i] == 2

    # Es gibt genau einen Anfang und ein Ende
    model += xsum(ends) == 2

    model.cuts_generator = SubTourCutGenerator(x, V)
    model.lazy_constrs_generator = SubTourCutGenerator(x, V)
    return model, x, ends


points = []
with open(path := input("Pfad zur Datei: ")) as f:
    while line := f.readline():
        points.append(tuple(map(float, line.split())))
max_gap = float(input("Maximale Luecke zur unteren Schranke in Prozent: ")) / 100


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
print("Suche Startloesung...")

init_solution = solveTA(path)
p1 = init_solution[0]
start = []
for p2 in init_solution[1:]:
    start.append((x[edge(p1, p2)], 1.0))
    p1 = p2
start.append((ends[init_solution[0]], 1.0))
start.append((ends[init_solution[-1]], 1.0))
model.start = start

print("Suche optimale Loesung...")
# Vorbeugung von Rundungsfehlern
model.max_mip_gap = max_gap + 0.0001
model.optimize(max_seconds=float("inf"))
import winsound

print(model.status)
winsound.MessageBeep()
if model.status in (OptimizationStatus.OPTIMAL, OptimizationStatus.FEASIBLE):
    print("Loesung gefunden!")
    print("Kosten:", model.objective_value * 100 // 1 / 100)
    solution = []
    for i in range(len(points)):
        if ends[i].x == 1:
            solution.append(i)
            break
    solution.append(
        next((j for j in range(len(points)) if i != j and x[edge(i, j)].x == 1), None)
    )
    while ends[solution[-1]].x == 0:
        solution.append(
            next(
                (
                    j
                    for j in range(len(points))
                    if j != solution[-1]
                    and x[edge(solution[-1], j)].x == 1
                    and j not in solution
                ),
                None,
            )
        )
    print(solution)
if model.status == OptimizationStatus.NO_SOLUTION_FOUND:
    print("Keine weitere Loesung gefunden!")

    print("Kosten:", model.objective_value)
    print(init_solution)

if model.status == OptimizationStatus.INFEASIBLE:
    if all(
        not acute(init_solution[i], init_solution[i + 1], init_solution[i + 2])
        for i in range(len(init_solution) - 2)
    ):
        print("Startloesung ist optimal!")
        print(
            "Kosten:",
            sum(
                weight_matrix[i][j]
                for i, j in zip(init_solution[:-1], init_solution[1:])
            )
            * 100
            // 1
            / 100,
        )
        print(init_solution)
    else:
        print("Es konnte keine gueltige Loesung gefunden werden!")
