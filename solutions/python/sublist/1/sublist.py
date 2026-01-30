"""
This exercise stub and the test suite contain several enumerated constants.

Enumerated constants can be done with a NAME assigned to an arbitrary,
but unique value. An integer is traditionally used because it’s memory
efficient.
It is a common practice to export both constants and functions that work with
those constants (ex. the constants in the os, subprocess and re modules).

You can learn more here: https://en.wikipedia.org/wiki/Enumerated_type
"""

# Possible sublist categories.
# Change the values as you see fit.
SUBLIST = 1
SUPERLIST = 2
EQUAL = 3
UNEQUAL = 0


def sublist(list_one, list_two):
    if equal(list_one, list_two):
        return EQUAL
    if include(list_one, list_two):
        return SUPERLIST
    if include(list_two, list_one):
        return SUBLIST
    return UNEQUAL

def equal(a, b):
    l = len(a)
    if l != len(b):
        return False
    for i in range(l):
        if a[i] != b[i]:
            return False
    return True

def include(a, b):
    la, lb = len(a), len(b)
    if la <= lb:
        return False
    for i in range(la):
        if la - i < lb:
            return False
        if equal(a[i:(i + lb)], b):
            return True
    return False
