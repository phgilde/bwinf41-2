from random import randint, sample

# Operatoren nach Larranaga et al.: "Genetic Algorithms for the Travelling
# Salesman Problem: A Review of Representations and Operators" (1999)


def three_opt(route):
    p1 = randint(0, len(route) - 3)
    p2 = randint(p1, len(route) - 2)
    p3 = randint(p2, len(route) - 1)

    split = [route[:p1], route[p1:p2], route[p2:p3], route[p3:]]
    split = [x[::-1] if randint(0, 1) else x for x in split]
    return [a for x in sample(split, 4) for a in x]


def displace(route):
    start = randint(0, len(route) - 3)
    stop = randint(start, len(route) - 2)
    pos = randint(0, stop - start)
    removed = route[:start] + route[stop:]
    return removed[:pos] + route[start:stop] + removed[pos:]


def reverse_displace(route):
    start = randint(0, len(route) - 3)
    stop = randint(start, len(route) - 2)
    pos = randint(0, stop - start)
    removed = route[:start] + route[stop:]
    return removed[:pos] + route[start:stop][::-1] + removed[pos:]


def insert(route):
    p1 = randint(0, len(route) - 2)
    p2 = randint(0, len(route) - 2)
    route_removed = route[:p1] + route[p1 + 1 :]
    return route_removed[:p2] + [route[p1]] + route_removed[p2:]
