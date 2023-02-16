import matplotlib.pyplot as plt

points = []
with open(input("Pfad zur Datei: ")) as f:
    while line := f.readline():
        points.append(tuple(map(float, line.split())))

#tour_raw = input("Tour: ")
#tour = tuple(map(int, tour_raw[1:-1].split(", ")))
tour = [16, 55, 49, 1, 11, 27, 43, 21, 9, 14, 18, 29, 51, 10, 42, 45, 48, 0, 20, 5, 41, 4, 2, 36, 47, 37, 22, 50, 33, 53, 15, 28, 58, 38, 23, 24, 19, 3, 13, 6, 12, 40, 59, 44, 35, 30, 56, 46, 25, 8, 34, 17, 31, 52, 
7, 26, 32, 39, 54, 57]
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
