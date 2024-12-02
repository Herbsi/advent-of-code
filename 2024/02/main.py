#!/usr/bin/env python3

import numpy as np
import re


def parse_input(filename):
    pattern = re.compile(r"(\d+)")
    with open(filename, "r") as f:
        return [np.array([int(group) for group in pattern.findall(line)]) for line in f]


reports = parse_input("input.txt")


## Part 1 ######################################################################


def report_save_p(report):
    diff = np.diff(report)
    return (
        (np.all(diff < 0) or np.all(diff > 0))
        and np.all(1 <= np.abs(diff))
        and np.all(np.abs(diff) <= 3)
    )


print(np.sum(list(map(report_save_p, reports))))
