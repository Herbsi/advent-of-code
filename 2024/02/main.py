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


## Part 2 ######################################################################


def report_almost_save_p(report):
    ## Quadratic, but oh well.
    return report_save_p(report) or (
        np.any(
            np.fromiter(
                (
                    report_save_p(np.concat((report[0:i], report[(i + 1) :])))
                    for i in range(len(report))
                ),
                dtype=int,
            )
        )
    )


print(np.sum(list(map(report_almost_save_p, reports))))
