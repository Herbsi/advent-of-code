#!/usr/bin/env python3

import copy
import enum
from collections import Counter
from itertools import chain


class Direction(enum.Enum):
    NORTH = 0
    EAST = 1
    SOUTH = 2
    WEST = 3

    def turn(self):
        return Direction((self.value + 1) % 4)


def parse_input(filename):
    with open(filename, "r") as f:
        area = [
            list(line) for line in f.read().split("\n")[:-1]
        ]  # Delete empty newline

    guard = (None, None)
    for i in range(len(area)):
        for j in range(len(area[0])):
            if area[i][j] == "^":
                guard = (i, j)
                area[i][j] = "X"

    return (area, guard, Direction(0))


def move(area, guard, direction):
    area[guard[0]][guard[1]] = "X"
    next_location = guard
    match direction:
        case Direction.NORTH:
            next_location = (guard[0] - 1, guard[1])
        case Direction.EAST:
            next_location = (guard[0], guard[1] + 1)
        case Direction.SOUTH:
            next_location = (guard[0] + 1, guard[1])
        case Direction.WEST:
            next_location = (guard[0], guard[1] - 1)

    if not (
        (0 <= next_location[0] < len(area)) and (0 <= next_location[1] < len(area[0]))
    ):
        return (area, "Gone", direction)

    match area[next_location[0]][next_location[1]]:
        case "#":
            return (area, guard, direction.turn())
        case "." | "X":
            guard = next_location
            return (area, guard, direction)


(area, guard, direction) = parse_input("input.txt")

## Part 1 ######################################################################

while guard != "Gone":
    (area, guard, direction) = move(area, guard, direction)

print(Counter(chain(*area))["X"])


## Part 2 ######################################################################


(area_default, guard_start, direction_start) = parse_input("input.txt")

## Walk all once.
area_walked = copy.deepcopy(area_default)
guard = guard_start
direction = direction_start

while guard != "Gone":
    (area_walked, guard, direction) = move(area_walked, guard, direction)

## For every potential new obstruction, try to find a loop.
cnt = 0
for i in range(len(area_walked)):
    for j in range(len(area_walked[0])):
        if area_walked[i][j] == "X":
            area = copy.deepcopy(area_default)
            guard = guard_start
            direction = direction_start

            area[i][j] = "#"
            stack = set()
            while guard != "Gone":
                if (guard, direction) in stack:
                    cnt += 1
                    break

                stack.add((guard, direction))
                (area, guard, direction) = move(area, guard, direction)

print(cnt)
