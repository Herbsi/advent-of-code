#!usr/bin/env python3

import re

test = """\
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet\
"""

def calibration_value(line):
    first_digit = re.compile(r".*?(\d).*")
    last_digit = re.compile(r".*(\d).*?")

    return int(first_digit.match(line).group(1) + last_digit.match(line).group(1))

def calibration_value_2(line):
    d = {
        "1": 1,
        "2": 2,
        "3": 3,
        "4": 4,
        "5": 5,
        "6": 6,
        "7": 7,
        "8": 8,
        "9": 9,
        "one": 1,
        "two": 2,
        "three": 3,
        "four": 4,
        "five": 5,
        "six": 6,
        "seven": 7,
        "eight": 8,
        "nine": 9
    }

    digits = list(re.escape(k) for k in d.keys())
    first_digit = re.compile(r".*?(" + '|'.join(digits) + r").*")
    last_digit = re.compile(r".*(" + '|'.join(digits) + r").*?")
    first_digit = first_digit.match(line).group(1)
    last_digit = last_digit.match(line).group(1)
    return 10*d[first_digit] + d[last_digit]


print("Part 1")
with open("input.txt", "r") as f:
    print(sum(
        calibration_value(line) for line in f
    ))

print("Test 1")
test_2 = """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"""
for line in str.split(test_2, "\n"):
    print(f"{line}\t{calibration_value_2(line)}")

print("Part 2")
with open("input.txt", "r") as f:
    total = 0
    for line in f:
        # print(f"{line}\t{calibration_value_2(line)}")
        total += calibration_value_2(line)

    print(total)
