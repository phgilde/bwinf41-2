import matplotlib.pyplot as plt

points = []
with open(input("Pfad zur Datei: ")) as f:
    while line := f.readline():
        points.append(tuple(map(float, line.split())))

#tour_raw = input("Tour: ")
#tour = tuple(map(int, tour_raw[1:-1].split(", ")))
tour = [21, 20, 5, 56, 31, 67, 62, 15, 2, 73, 60, 4, 77, 47, 55, 52, 24, 6, 76, 44, 72, 27, 25, 41, 66, 11, 71, 26, 19, 78, 10, 1, 74, 40, 28, 54, 23, 58, 45, 57, 46, 65, 59, 43, 79, 17, 22, 12, 37, 13, 34, 42, 32, 
35, 48, 50, 61, 7, 39, 64, 30, 38, 36, 51, 18, 0, 3, 16, 14, 9, 33, 70, 49, 29, 75, 63, 8, 53, 69, 68]

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
