#!/usr/bin/env python3

import copy
import re

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
    valid_predecessors = dict()

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

### Kahn's algorithm


def order_update(update):
    graph = dict(
        (k, v & set(update))
        for (k, v) in copy.deepcopy(page_ordering_dict).items()
        if k in update
    )

    successors = set.union(
        *[graph[page_number] for page_number in update if page_number in graph.keys()]
    )

    update_ordered = list()
    start_nodes = set(graph.keys()) - successors

    while bool(start_nodes):
        n = start_nodes.pop()
        update_ordered.append(n)
        try:
            for m in graph[n].copy():
                graph[n] -= set([m])
                if len([nodes for nodes in graph.values() if m in nodes]) == 0:
                    start_nodes.add(m)
        except KeyError:
            continue

    return update_ordered


print(
    sum(
        map(
            middle_page,
            map(
                order_update, filter(lambda update: not valid_update_p(update), updates)
            ),
        )
    )
)
