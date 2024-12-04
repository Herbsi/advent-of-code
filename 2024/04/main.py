#!/usr/bin/env python3

import functools
import itertools

test = """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""

test_array = list(map(list, str.split(test, "\n")))
with open("input.txt", "r") as f:
    input_array = list(map(list, str.split(f.read(), "\n")))[
        :-1
    ]  # Remove empty newline


## Part 1 ######################################################################


def xmas(i, j):
    return filter(
        lambda coord: all((i >= 0 and j >= 0 for (i, j) in coord)),
        map(  # I eagerly evaluate here, but I do not know why it is necessary.
            list,
            itertools.starmap(
                itertools.zip_longest,
                itertools.product(
                    [[i] * 4, range(i, i + 4), range(i, i - 4, -1)],
                    [[j] * 4, range(j, j + 4), range(j, j - 4, -1)],
                ),
            ),
        ),
    )


def count_xmas(array):
    def str_from_coord(ij, coord):
        try:
            return "".join(map(lambda ij: array[ij[0]][ij[1]], coord))
        except IndexError:
            return None

    return sum(
        map(
            lambda ij: sum(
                ("XMAS" == str_from_coord(ij, coord) for coord in xmas(ij[0], ij[1]))
            ),
            filter(
                lambda ij: array[ij[0]][ij[1]] == "X",
                itertools.product(range(len(array)), range(len(array[0]))),
            ),
        ),
    )


print(count_xmas(input_array))


## Part 2 ######################################################################


def count_x_mas(array):
    def x_mas_border(ij):
        return "".join(
            array[ij[0] + k][ij[1] + l]
            for (k, l) in [(-1, -1), (-1, +1), (+1, +1), (+1, -1)]
        )

    return sum(
        map(
            lambda ij: x_mas_border(ij) in ["MMSS", "SMMS", "SSMM", "MSSM"],
            filter(
                lambda ij: array[ij[0]][ij[1]] == "A",
                itertools.product(
                    range(1, len(array) - 1), range(1, len(array[0]) - 1)
                ),
            ),
        )
    )


print(count_x_mas(input_array))
