import math
from a_star import a_star

# Pfannkuchenstapel umdrehen und Pfannkuchen essen.
# Zeitkomplexität: O(k), weil k elemente verändert werden.
def flip(arr, k):
    return arr[: k - 1][::-1] + arr[k:]


# Gibt alle möglichen nächsten Reihenfolgen zurück.
# Zeitkomplexität: O(n^2), weil flip() O(n) ist und wir n-1 mal aufrufen.
def next_arrs(arr):
    for i in range(1, len(arr) + 1):
        yield normalize(flip(arr, i))


# Zählt, wie viele aufeinanderfolgende Pfannkuchen nebeneinander liegen.
# Zeitkomplexität: O(n), weil wir nur einmal durch die Liste laufen.
def count_adj(arr):
    adj = 0
    for i in range(1, len(arr)):
        if arr[i] - arr[i - 1] in (1, -1):
            adj += 1
    if arr[-1] == max(arr):
        adj += 1
    return adj


# Verändert die Zahlen in der Liste so, dass sie in [0, ..., n-1] liegen, wobei die Reihenfolge erhalten bleibt.
# Zeitkomplexität: O(n^2), weil order.index() O(n) ist und wir n mal aufrufen.
def normalize(arr):
    order = sorted(arr)
    return tuple(order.index(x) for x in arr)


# Nähert die minimale Anzahl von flips()s mit count_adj() an.
# Zeitkomplexität: O(n^2), weil normalize() O(n^2) ist.
def heuristic(arr):
    return math.floor((len(arr) - count_adj(normalize(arr))) / 3)


# prüft, ob die Liste in der richtigen Reihenfolge ist.
# Zeitkomplexität: O(n), weil wir nur einmal durch die Liste laufen.
def is_sorted(arr):
    return all(arr[i] <= arr[i + 1] for i in range(len(arr) - 1))


# Gibt die Optimale Reihenfolge von flip()s zurück, um die Liste zu sortieren.
# Zeitkomplexität: ??
def least_flips(arr):
    return a_star(normalize(arr), is_sorted, next_arrs, lambda a, b: 1, heuristic)


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
