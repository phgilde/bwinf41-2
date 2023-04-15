import math
from queue import PriorityQueue

# findet den kuerzesten pfad von start_node zu einem knoten, der target_pred erfuellt.
def a_star(start_node, target_pred, adj_func, cost_func, heur_func, count_steps=False):
    if count_steps:
        steps = 0
    i = 0
    queue = PriorityQueue()
    queue.put((0, heur_func(start_node), i, start_node))
    prev = {start_node: None}
    cost = {start_node: 0 + heur_func(start_node)}
    while not queue.empty():
        if count_steps:
            steps += 1
        _, _, _, node = queue.get()
        if target_pred(node):
            if count_steps:
                return reconstruct_path(node, prev), steps
            return reconstruct_path(node, prev)
        for adj_node in adj_func(node):
            new_cost = cost[node] - heur_func(node) + cost_func(node, adj_node) + heur_func(adj_node)
            if adj_node not in cost or new_cost < cost[adj_node]:
                i -= 1
                cost[adj_node] = new_cost
                queue.put((new_cost, heur_func(node), i, adj_node))
                prev[adj_node] = node


# gibt den pfad von node zum anfang zurueck.
def reconstruct_path(node, prev):
    path = [node]
    while prev[node] is not None:
        node = prev[node]
        path.append(node)
    return list(reversed(path))


# Pfannkuchenstapel umdrehen und Pfannkuchen essen.
def flip(arr, k):
    return arr[: k - 1][::-1] + arr[k:]


# Gibt alle moeglichen naechsten Stapel zurueck.
def next_arrs(arr):
    for i in range(1, len(arr) + 1):
        yield canonize(flip(arr, i))


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
def canonize(arr):
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

# Zweite implementierung von normalize, die langsamer ist, aber besser mit
# groesseren zahlen funktioniert.
def canonize2(arr):
    return tuple(sorted(arr).index(x) for x in arr)

# Naehert die minimale Anzahl von flips()s mit count_adj() an.
def heuristic(arr):
    return math.ceil((len(arr) - count_adj(canonize(arr))) / 3)


# prueft, ob die Liste in der richtigen Reihenfolge ist.
def is_sorted(arr):
    return all(arr[i] <= arr[i + 1] for i in range(len(arr) - 1))


# Gibt die Optimale Reihenfolge von flip()s zurueck, um die Liste zu sortieren.
def least_flips(arr, count_steps=False):
    return a_star(canonize(arr), is_sorted, next_arrs, lambda a, b: 1, heuristic, count_steps)


# Findet die PWUE-Operation von pre zu post.
def find_flip(pre, post):
    for i in range(1, len(pre) + 1):
        if canonize(flip(pre, i)) == canonize(post):
            return i


def main():
    # stapel einlesen
    path = input("Pfad: ")
    with open(path) as f:
        n_pancakes = int(f.readline())
        pancakes = tuple(int(x) for x in f.readlines())

    # Da nach Erweiterung beliebig grosse Zahlen verwendet werden koennen,
    # muss die langsamere Version verwendet werden.
    pancakes = canonize2(pancakes)
    steps = least_flips(pancakes)

    print("-- schritte --")
    pre = None
    not_normalized = list(map(lambda x: x+1, steps[0]))
    print(" ".join(map(str, not_normalized)))
    for step in steps:
        if pre is not None:
            ix = find_flip(pre, step)
            print("Wende erste", ix)
            not_normalized = flip(not_normalized, ix)
            print(" ".join(map(str, not_normalized)))
        pre = step


if __name__ == "__main__":
    main()
