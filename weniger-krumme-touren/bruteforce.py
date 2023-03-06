from matplotlib import pyplot as plt
from scipy.spatial import ConvexHull
from numpy import array
from functools import lru_cache

@lru_cache(maxsize=100000)
def acute(p1, p2, p3):
    v1 = (p1[0] - p2[0], p1[1] - p2[1])
    v2 = (p3[0] - p2[0], p3[1] - p2[1])

    if (v1[0] * v2[0] + v1[1] * v2[1]) / (
        (v1[0] ** 2 + v1[1] ** 2) ** 0.5 * (v2[0] ** 2 + v2[1] ** 2) ** 0.5
    ) > 0:
        return True
    return False


# returns true if the convex hull contains 3 acute angles
def count_acutes(hull):
    count = 0
    for i in range(len(hull.vertices)):
        if acute(
            tuple(hull.points[hull.vertices[i - 2]]),
            tuple(hull.points[hull.vertices[i - 1]]),
            tuple(hull.points[hull.vertices[i]]),
        ):
            count += 1
    return count


def get_acute(hull):
    result = []
    for i in range(len(hull.vertices)):
        if acute(
            tuple(hull.points[hull.vertices[i - 2]]),
            tuple(hull.points[hull.vertices[i - 1]]),
            tuple(hull.points[hull.vertices[i]]),
        ):
            result.append(tuple(hull.points[hull.vertices[i - 1]]))
    return result

def count_not_acute(prev1, prev2, points):
    result = 0
    for point in points:
        if not acute(prev1, prev2, point):
            result += 1
    return result

def solve(prev1, prev2, points, depth):
    if len(points) == 0:
        return []
    # convert points to array
    points_a = array(list(points))
    # find convex hull of points
    points_check = points
    hull_points = None
    if len(points) > 2:
        hull = ConvexHull(points_a, qhull_options="QJ")
        hull_points = [tuple(points_a[i]) for i in hull.vertices]
        acutes = count_acutes(hull)
        if acutes == 3:
            return None
        if acutes == 2:
            points_check = get_acute(hull)

    for point in (
        sorted(
            points_check,
            key=lambda p: (
                -count_not_acute(prev2, p, points - {p}),
                (p[0] - prev2[0]) ** 2 + (p[1] - prev2[1]) ** 2,
            ),
        )
        if prev2 != None
        else hull_points
    ):
        if prev1 == None or prev2 == None or not acute(prev1, prev2, point):
            if depth < 10:
                print("Trying", point)
            nextroute = solve(prev2, tuple(point), points - {tuple(point)}, depth + 1)
            if nextroute != None:
                return [point] + nextroute


    return None


def bruteforce(points):
    return solve(None, None, set(points), 0)


points = []
with open(path := input("Pfad zur Datei: ")) as f:
    while line := f.readline():
        points.append(tuple(map(float, line.split())))

# plot points
plt.figure(figsize=(10, 10))
min_coord = min((min([p[0] for p in points]), min([p[1] for p in points])))
max_coord = max((max([p[0] for p in points]), max([p[1] for p in points])))
plt.xlim(min_coord - 50, max_coord + 50)
plt.ylim(min_coord - 50, max_coord + 50)
plt.scatter([p[0] for p in points], [p[1] for p in points])
plt.show()

tour = bruteforce(points)

if tour != None:
    indices = [points.index(tuple(p)) for p in tour]
    tour = indices
    print(f"Tour: {tour}")

    min_coord = min((min([p[0] for p in points]), min([p[1] for p in points])))
    max_coord = max((max([p[0] for p in points]), max([p[1] for p in points])))
    plt.figure(figsize=(10, 10))
    plt.xlim(min_coord - 50, max_coord + 50)
    plt.ylim(min_coord - 50, max_coord + 50)
    plt.scatter([p[0] for p in points], [p[1] for p in points])
    plt.plot(
        [points[tour[i]][0] for i in range(len(tour))],
        [points[tour[i]][1] for i in range(len(tour))],
    )
    plt.show()
