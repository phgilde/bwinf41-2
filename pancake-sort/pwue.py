from itertools import permutations
from least_flips import least_flips


def reverse_flip(arr, i, new):
    arr_new = (new,) + arr
    return arr_new[:i][::-1] + arr_new[i:]


def flip(arr, i):
    return arr[: i - 1][::-1] + arr[i:]


def next_arrs(arr):
    for i in range(2, len(arr) + 1):
        yield flip(arr, i)


def prev_arrs(arr, max_n):
    for i in range(1, len(arr) + 1):
        for new in range(max_n):
            if not new in arr:
                yield reverse_flip(arr, i, new)


def all_permutations(n):
    return permutations(range(n))

def hard_permutations(start, end):
    initial = tuple(range(start))
    current_hardest = 
    for i in range(end-start):

def main():
    size = int(input("Size: "))
    max_steps = 0
    for arr in all_permutations(size):
        steps = len(least_flips(arr))
        if steps > max_steps:
            max_steps = steps
    print("PWUE:", max_steps - 1)

if __name__ == "__main__":
    main()