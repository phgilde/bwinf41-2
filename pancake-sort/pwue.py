from functools import lru_cache
import math
import cProfile
import pstats


class FlipOp:
    def __init__(self, pos):
        self.pos = pos

    def __repr__(self):
        return f"FlipOp({self.pos})"

    def __call__(self, seq):
        return normalize(seq[: self.pos - 1][::-1] + seq[self.pos :])


class RevFlipOp:
    def __init__(self, pos, new):
        self.pos = pos
        self.new = new

    def __repr__(self):
        return f"RevFlipOp({self.pos}, {self.new})"

    def __call__(self, seq):
        pre = (self.new,) + tuple(a + 1 if a >= self.new else a for a in seq)
        return normalize(pre[: self.pos][::-1] + pre[self.pos :])


def allFlipOps(n):
    return [FlipOp(i) for i in range(1, n + 1)]


def allRevFlipOps(n):
    return [RevFlipOp(i, j) for i in range(1, n + 1) for j in range(n + 1)]


@lru_cache(maxsize=2**20)
def normalize(seq):
    return tuple(
        map(
            lambda x: x[0],
            sorted(
                zip(
                    range(len(seq)),
                    map(lambda x: x[1], sorted(zip(seq, range(len(seq))))),
                ),
                key=lambda x: x[1],
            ),
        )
    )


@lru_cache(maxsize=None)
def k(n, a):
    if a == 0:
        return {tuple(range(n))}
    res = set()
    for rflip in allRevFlipOps(n):
        for seq in k(n - 1, a - 1):
            if not (a > 1 or rflip(seq) != tuple(range(n))):
                continue
            r1 = True
            r2 = False
            for flip in allFlipOps(n):
                for b in range(a - 1, math.ceil(n / 1.5)):
                    r2 = False
                    if flip(rflip(seq)) in k(n - 1, b):
                        r2 = True
                        break
                r1 = r1 and r2
                if not r1:
                    break
            if r1:
                res.add(rflip(seq))
    print(n, a, len(res))
    return res
    return {
        rflip(seq)
        for rflip in allRevFlipOps(n)
        for seq in k(n - 1, a - 1)
        if (a > 1 or rflip(seq) != tuple(range(n)))
        and all(
            any(
                flip(rflip(seq)) in k(n - 1, b)
                for b in range(a - 1, math.ceil(n / 1.5))
            )
            for flip in allFlipOps(n)
        )
    }


def main():
    n = int(input("n: "))
    for a in range(1, math.ceil(n / 1.5))[::-1]:
        print(a, len(k(n, a)))
        if len(k(n, a)) > 0:
            break


if __name__ == "__main__":
    cProfile.run("main()", "restats")

    p = pstats.Stats("restats")
    p.strip_dirs().sort_stats("time").print_stats(10)