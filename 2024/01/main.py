#!/usr/bin/env python3

from collections import Counter

import numpy as np
import operator as op
import re


def parse_line(line):
    return np.array(
        [int(group) for group in re.match(r"(\d+)\s+(\d+)", line).group(1, 2)]
    )


## Part 1 ######################################################################

with open("input.txt", "r") as f:
    print(
        np.sum(
            np.abs(
                np.apply_along_axis(
                    np.diff, 1, np.sort(np.array([parse_line(line) for line in f]), 0)
                )
            )
        )
    )


## Part 2 ######################################################################

with open("input.txt", "r") as f:
    [left, right] = np.transpose(np.array([parse_line(line) for line in f]))
    keys = set(left)
    counter = Counter(right)
    print(np.sum(np.fromiter((k * counter[k] for k in keys), dtype=np.int32)))
