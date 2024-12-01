#!/usr/bin/env python3

import numpy as np
import re


def parse_line(line):
    return np.array(
        [int(group) for group in re.match(r"(\d+)\s+(\d+)", line).group(1, 2)]
    )


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
