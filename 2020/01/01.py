#!/usr/bin/env python3

# Part 1
with open("input.txt") as f:
    cache = dict()
    for line in f:
        n = int(line)
        cache[n] = True
        try:
            cache[2020 - n]
            print(n * (2020 - n))
            break
        except KeyError:
            continue

# Part 2
# TODO still O(n^3) :-(
# TODO should be doable in O(n^2) (according to Doom Discord)
with open("input.txt") as f:
    numbers = [int(line) for line in f]
    for (i, ni) in enumerate(numbers):
        for (j, nj) in enumerate(numbers[i:], i):
            for (k, nk) in enumerate(numbers[j:], j):
                if ni + nj + nk == 2020:
                    print(ni * nj * nk)
                    break
