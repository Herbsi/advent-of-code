#!/usr/bin/env python3

import functools
import itertools
import re
from collections import deque

filename = "input.txt"


def parse_line(line):
    equation_re = re.compile(r"(\d+):((?:\s\d+)+)")

    test_value, values = equation_re.match(line).groups()

    return (int(test_value), [int(value) for value in values.split(" ")[1:]])


with open(filename, "r") as f:
    equations = [parse_line(line) for line in f.read().split("\n")[:-1]]


operators = ["+", "*", "||"]


def process_equation(test_value, values, operators=["+", "*"]):
    def folder(acc, cur):
        match cur[1]:
            case "+":
                return acc + cur[0]
            case "*":
                return acc * cur[0]
            case "||":
                return int(f"{acc}{cur[0]}")

    for ops in itertools.product(operators, repeat=len(values) - 1):
        if functools.reduce(folder, zip(values[1:], ops), values[0]) == test_value:
            return test_value
    else:
        return None


print(
    sum(
        (
            v
            for (test_value, values) in equations
            if (v := process_equation(test_value, values))
        )
    )
)

## Part 2 ######################################################################

print(
    sum(
        (
            v
            for (test_value, values) in equations
            if (v := process_equation(test_value, values, ["+", "*", "||"]))
        )
    )
)
