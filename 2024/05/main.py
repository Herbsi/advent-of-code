#!/usr/bin/env python3

import re
from collections import deque

filename = "input.txt"


def parse_input(filename):
    with open(filename, "r") as f:
        inp = f.read()

    [page_ordering_rules, updates] = list(
        map(lambda s: s.split("\n"), inp.split("\n\n"))
    )
    updates = updates[:-1]  # Remove empty newline

    page_ordering_re = re.compile(r"(\d+)\|(\d+)")
    page_ordering_dict = dict()

    for rule in page_ordering_rules:
        (lhs, rhs) = tuple(map(int, page_ordering_re.match(rule).group(1, 2)))
        try:
            page_ordering_dict[lhs].add(rhs)
        except KeyError:
            page_ordering_dict[lhs] = {rhs}

    updates = [
        [int(page_number) for page_number in update.split(",")] for update in updates
    ]

    return (page_ordering_dict, updates)


(page_ordering_dict, updates) = parse_input(filename)


def valid_update_p(update):
    pages_so_far = set()
    for page in update:
        pages_so_far |= {page}
        try:
            if not page_ordering_dict[page].isdisjoint(pages_so_far):
                return False
        except KeyError:
            continue

    return True


def middle_page(update):
    n = len(update)
    return update[(n - 1) // 2]


## Part 1 ######################################################################

print(sum(map(middle_page, filter(valid_update_p, updates))))


## Part 2 ######################################################################
