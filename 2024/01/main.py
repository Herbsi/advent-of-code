#!/usr/bin/env python3

from collections import Counter

import numpy as np
import operator as op
import re


def parse_input(filename):
    with open(filename, "r") as f:
        return np.array(
            [
                [int(group) for group in re.match(r"(\d+)\s+(\d+)", line).group(1, 2)]
                for line in f
            ]
        )


inp = parse_input("input.txt")

## Part 1 ######################################################################

print(np.sum(np.abs(np.apply_along_axis(np.diff, 1, np.sort(inp, 0)))))


## Part 2 ######################################################################

keys = set(inp[:, 0])
counter = Counter(inp[:, 1])
print(np.sum(np.fromiter((k * counter[k] for k in keys), dtype=np.int32)))
