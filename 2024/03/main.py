#!/usr/bin/env python3

import numpy as np
import re

mul_instr_re = re.compile(r"mul\((\d{1,3}),(\d{1,3})\)")

with open("input.txt", "r") as f:
    memory = f.read()

## Part 1 ######################################################################

print(np.sum([int(a) * int(b) for (a, b) in mul_instr_re.findall(memory)]))
