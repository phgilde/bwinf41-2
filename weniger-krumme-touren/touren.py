import pulp
import matplotlib.pyplot as plt

# checks if the angle between the three points is acute
def acute(p1, p2, p3):
    v1 = (p1[0] - p2[0], p1[1] - p2[1])
    v2 = (p3[0] - p2[0], p3[1] - p2[1])

    if (v1[0] * v2[0] + v1[1] * v2[1]) / (
        (v1[0] ** 2 + v1[1] ** 2) ** 0.5 * (v2[0] ** 2 + v2[1] ** 2) ** 0.5
    ) > 0:
        return True
    return False

# creates an instance of the tsp problem
def tsp_instance(n_points, weight_matrix):
    problem = pulp.LpProblem("Tourenplanung", pulp.LpMinimize)

    # create variables
    x = pulp.LpVariable.dicts(
        "x",
        [(i, j) for i in range(n_points) for j in range(n_points)],
        lowBound=0,
        upBound=1,
        cat="Integer",
    )
    u = pulp.LpVariable.dicts("u", range(n_points), lowBound=0, upBound=n_points - 1)

    # create objective function
    problem += pulp.lpSum(
        [weight_matrix[i][j] * x[(i, j)] for i in range(n_points) for j in range(n_points)]
    )

    # create constraints
    for i in range(n_points):
        problem += pulp.lpSum([x[(i, j)] for j in range(n_points)]) == 1
        problem += pulp.lpSum([x[(j, i)] for j in range(n_points)]) == 1
        problem += x[(i, i)] == 0
    
    for i in range(1, n_points):
        for j in range(1, n_points):
            if i != j:
                problem += u[i] - u[j] + n_points * x[(i, j)] <= n_points - 1
    

    return problem, x

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
            (points[i][0] - points[j][0]) ** 2 + (points[i][1] - points[j][1]) ** 2
        )
    weight_matrix[i].append(0)
weight_matrix.append([0] * (len(points) + 1))

# create problem
problem, x = tsp_instance(len(points) + 1, weight_matrix)

# acute angle constraint
for i in range(len(points)):
    for j in range(len(points)):
        if i != j:
            for k in range(len(points)):
                if i != k and j != k:
                    if acute(points[i], points[j], points[k]):
                        problem += x[(i, j)] + x[(j, k)] <= 1

print("Starte Berechnung...")

# solve problem while printing the status
problem.solve(pulp.PULP_CBC_CMD(msg=True, timeLimit=60*float(input("Zeitlimit in Minuten: "))))

print("Fertig!")
# plot solution if it exists

if problem.status == 1:
    plt.figure(figsize=(10, 10))
    plt.scatter([p[0] for p in points], [p[1] for p in points])
    for i in range(len(points)):
        for j in range(len(points)):
            if x[(i, j)].value() == 1:
                plt.plot([points[i][0], points[j][0]], [points[i][1], points[j][1]], "r")
    plt.show()
    print("Länge der Strecke: ", pulp.value(problem.objective))