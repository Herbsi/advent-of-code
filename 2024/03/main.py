#!/usr/bin/env python3

from collections import deque

import numpy as np
import re

mul_instr_re = re.compile(r"mul\((\d{1,3}),(\d{1,3})\)")

with open("input.txt", "r") as f:
    memory = f.read()

## Part 1 ######################################################################

print(np.sum([int(a) * int(b) for (a, b) in mul_instr_re.findall(memory)]))


## Part 2 ######################################################################

instr_re = re.compile(r"(?:mul\(\d{1,3},\d{1,3}\))|(?:do\(\))|(?:don't\(\))")
instr_stack = deque((match.group(0) for match in instr_re.finditer(memory)))

active = True
result = 0
while instr_stack:
    match instr_stack.popleft():
        case "do()":
            active = True
        case "don't()":
            active = False
        case instr:
            if active == True:
                (a, b) = mul_instr_re.match(instr).groups((1, 2))
                result += int(a) * int(b)

print(result)
