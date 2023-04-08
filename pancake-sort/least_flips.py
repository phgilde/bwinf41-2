import math
from a_star import a_star

# Pfannkuchenstapel umdrehen und Pfannkuchen essen.
def flip(arr, k):
    return arr[: k - 1][::-1] + arr[k:]


# Gibt alle moeglichen naechsten Reihenfolgen zurueck.
def next_arrs(arr):
    for i in range(1, len(arr) + 1):
        yield normalize(flip(arr, i))


# Zaehlt, wie viele aufeinanderfolgende Pfannkuchen nebeneinander liegen.
def count_adj(arr):
    adj = 0
    for i in range(1, len(arr)):
        if arr[i] - arr[i - 1] in (1, -1):
            adj += 1
    if arr[-1] == max(arr):
        adj += 1
    return adj


# Veraendert die Zahlen in der Liste so, dass sie in [0, ..., n-1] liegen, 
# wobei die Reihenfolge erhalten bleibt.
# Algorithmus mit O(n), was auch die kleinstmoegliche Zeitkomplexitaet ist,
# da ja schon die ausgabe des ergebnisses Zeit O(n) braucht
def normalize(arr):
    a_min = min(arr)
    a_max = max(arr)
    values = [-1 for _ in range(a_max - a_min + 1)]
    for item in arr:
        values[item - a_min] = item
    counter = 0
    for i in range(len(values)):
        if values[i] != -1:
            values[i] = counter
            counter += 1
    return tuple(values[x - a_min] for x in arr)


# Naehert die minimale Anzahl von flips()s mit count_adj() an.
def heuristic(arr):
    return math.ceil((len(arr) - count_adj(normalize(arr))) / 3)


# prueft, ob die Liste in der richtigen Reihenfolge ist.
def is_sorted(arr):
    return all(arr[i] <= arr[i + 1] for i in range(len(arr) - 1))


# Gibt die Optimale Reihenfolge von flip()s zurueck, um die Liste zu sortieren.
def least_flips(arr, count_steps=False):
    return a_star(normalize(arr), is_sorted, next_arrs, lambda a, b: 1, heuristic, count_steps)


def find_flip(pre, post):
    for i in range(1, len(pre) + 1):
        if normalize(flip(pre, i)) == post:
            return i


def main():
    path = input("Pfad: ")
    with open(path) as f:
        n_pancakes = int(f.readline())
        pancakes = tuple(int(x) for x in f.readlines())
    pancakes = normalize(pancakes)
    steps = least_flips(pancakes)
    print(steps)
    print(len(steps) - 1, "Schritte")
    print(len(pancakes), "Pfannkuchen")
    print(heuristic(pancakes), "heuristik")
    print(math.ceil(len(pancakes) / 1.5), "upper bound")
    print(len(pancakes) / 2, "lower bound")

    print("-- schritte --")
    pre = None
    not_normalized = steps[0]
    print(not_normalized)
    for step in steps:
        if pre is not None:
            ix = find_flip(pre, step)
            print("Wende erste", ix)
            not_normalized = flip(not_normalized, ix)
            print(not_normalized)
        pre = step


if __name__ == "__main__":
    main()
