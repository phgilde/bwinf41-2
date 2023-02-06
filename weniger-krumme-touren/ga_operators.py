import random


def segment_swap(individual):
    individual = individual.copy()
    i = random.randint(0, len(individual) - 4)
    j = random.randint(i, len(individual) - 3)
    k = random.randint(j + 1, len(individual) - 2)
    l = random.randint(k + 1, len(individual) - 1)
    individual = (
        individual[:i]
        + individual[k:l]
        + individual[j:k]
        + individual[i:j]
        + individual[l:]
    )
    return individual


def swap(individual):
    individual = individual.copy()
    i = random.randint(0, len(individual) - 2)
    j = random.randint(i, len(individual) - 1)
    individual[i], individual[j] = individual[j], individual[i]
    return individual


def rotate(individual):
    individual = individual.copy()
    i = random.randint(0, len(individual))
    individual = individual[i:] + individual[:i]
    return individual


def reverse(individual):
    individual = individual.copy()
    i = random.randint(0, len(individual) - 1)
    j = random.randint(i, len(individual))
    individual = individual[:i] + individual[i:j][::-1] + individual[j:]
    return individual


def displace(individual):
    individual = individual.copy()
    i = random.randint(0, len(individual) - 1)
    j = random.randint(i, len(individual))
    k = random.randint(0, len(individual))
    result = individual[:i] + individual[j:]
    result = result[:k] + individual[i:j] + result[k:]
    return result


def insert(individual):
    individual = individual.copy()
    i = random.randint(0, len(individual) - 1)
    j = random.randint(0, len(individual) - 1)
    selected = individual[i]
    individual = individual[:i] + individual[i + 1 :]
    individual = individual[:j] + [selected] + individual[j:]
    return individual


def reverse_displace(individual):
    individual = individual.copy()
    i = random.randint(0, len(individual) - 1)
    j = random.randint(i, len(individual))
    k = random.randint(0, len(individual) - (j - i))
    result = individual[:i] + individual[j:]
    result = result[:k] + individual[i:j][::-1] + result[k:]
    return result


def OX1(parent1, parent2):
    i = random.randint(0, len(parent1) - 2)
    j = random.randint(i, len(parent1) - 1)
    child = [-1 for _ in range(len(parent1))]
    child[i:j] = parent1[i:j]
    ptr = j + 1
    ch_ptr = j + 1
    while -1 in child:
        if ptr >= len(child):
            ptr = 0
        if ch_ptr >= len(child):
            ch_ptr = 0
        if parent2[ptr] not in child:
            child[ch_ptr] = parent2[ptr]
            ch_ptr += 1
        ptr += 1
    assert set(child) == set(parent1) == set(parent2)
    return child


def OX2(parent1, parent2):
    positions = []
    while random.random() < 0.8 and len(positions) < 5:
        position = random.randint(0, len(parent1) - 1)
        if position not in positions:
            positions.append(position)
    positions.sort()
    ixs = sorted([parent2.index(parent1[ix]) for ix in positions])
    child = parent2.copy()
    for ix, pos in zip(ixs, positions):
        child[ix] = parent1[pos]
    assert set(child) == set(parent1) == set(parent2)
    return child
