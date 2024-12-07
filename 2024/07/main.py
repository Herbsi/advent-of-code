#!/usr/bin/env python3

import itertools
import re
from collections import deque

filename = "input.txt"

operators = ["+", "*", "||"]


def parse_line(line):
    equation_re = re.compile(r"(\d+):((?:\s\d+)+)")

    test_value, values = equation_re.match(line).groups()

    return (int(test_value), values.split(" ")[1:])


with open(filename, "r") as f:
    equations = [parse_line(line) for line in f.read().split("\n")[:-1]]


def build_expression(values, operators):
    values = deque(values)
    operators = deque(operators)
    expr = ["".join(["("] * (len(values) - 1)), values.popleft()]

    while len(values) != 0:
        expr.append(operators.popleft())
        expr.append(values.popleft())
        expr.append(")")

    return "".join(expr)


total_calibration_result = 0
for test_value, values in equations:
    for ops in itertools.product(operators[0:2], repeat=len(values) - 1):
        value = eval("".join(build_expression(values, ops)))
        if test_value == value:
            total_calibration_result += test_value
            break

print(total_calibration_result)

## Part 2 ######################################################################

total_calibration_result = 0
for test_value, values in equations:
    for ops in itertools.product(operators, repeat=len(values) - 1):
        value = int(values[0])
        current_value = None
        for i in range(0, len(values) - 1):
            op = ops[i]
            current_value = values[i + 1]
            match op:
                case "+":
                    value += int(current_value)
                case "*":
                    value *= int(current_value)
                case "||":
                    value = int("".join([str(value), current_value]))

        if value == test_value:
            total_calibration_result += test_value
            break


print(total_calibration_result)
