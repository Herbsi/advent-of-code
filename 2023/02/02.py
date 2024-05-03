#!/usr/bin/env python3

import re

id = re.compile(r"Game (\d+)")
red = re.compile(r"(\d+) red")
blue = re.compile(r"(\d+) blue")
green = re.compile(r"(\d+) green")

def parse_line_1(line):
    game_id = int(id.match(line).group(1))
    max_red = max(int(m.group(1)) for m in red.finditer(line))
    max_blue = max(int(m.group(1)) for m in blue.finditer(line))
    max_green = max(int(m.group(1)) for m in green.finditer(line))

    if max_red <= 12 and max_blue <= 13 and max_green <= 14:
        return game_id
    else:
        return 0


print("Test")
test="""Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""
for line in str.split(test, "\n"):
    print(f"{line}\t{parse_line_1(line)}")



print("Part 1")
with open("input", "r") as f:
    print(sum(parse_line_1(line) for line in f))
