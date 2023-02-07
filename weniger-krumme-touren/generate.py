import datetime
import random
import matplotlib.pyplot as plt


# vector that is rectangular to the given vector
def acute(p1, p2, p3):
    v1 = (p1[0] - p2[0], p1[1] - p2[1])
    v2 = (p3[0] - p2[0], p3[1] - p2[1])

    return (v1[0] * v2[0] + v1[1] * v2[1]) > 0


def orthogonal(v):
    return (-v[1], v[0])

def scale(v, s):
    return (v[0] * s, v[1] * s)


def next_point(p1, p2, maxX, maxY):
    c = 0
    normalized_dist = scale(
        (p2[0] - p1[0], p2[1] - p1[1]),
        1 / ((p2[0] - p1[0]) ** 2 + (p2[1] - p1[1]) ** 2) ** 0.5,
    )
    ortho = orthogonal(normalized_dist)
    while True:
        # assert c < 10000, "too many tries"
        scale1 = random.random() * 5
        scale2 = random.random() * 100 - 50
        p3 = (
            p2[0] + scale2 * ortho[0] + scale1 * normalized_dist[0],
            p2[1] + scale2 * ortho[1] + scale1 * normalized_dist[1],
        )
        # plot p1, p2, p3

        if (
            p3 != p1
            and p2 != p3
            and p3 != (0, 0)
            and p3 != (maxX, maxY)
            and not acute(p1, p2, p3)
        ):
            return p3


length = int(input("Anzahl der Punkte: "))
maxX = 300
maxY = 300

points = [
    (random.randint(0, maxX), random.randint(0, maxY)),
    (random.randint(0, maxX), random.randint(0, maxY)),
]
for i in range(length - 2):
    points.append(next_point(points[-2], points[-1], maxX, maxY))

# round to 2 decimal places
points = [(round(x, 2), round(y, 2)) for x, y in points]

# write to a file named with current time and date
with open(
    f"points_{datetime.datetime.now().strftime('%Y-%m-%d_%H-%M-%S')}.txt", "w"
) as f:
    for point in points:
        f.write(f"{point[0]} {point[1]}\n")

cost = 0
for i in range(length - 1):
    cost += (
        (points[i][0] - points[i + 1][0]) ** 2 + (points[i][1] - points[i + 1][1]) ** 2
    ) ** 0.5

print(f"Kosten: {cost:.2f}")
