def equilateral(sides):
    return sides[0] != 0 and sides[0] == sides[1] == sides[2]


def isosceles(sides):
    sides = sorted(sides)
    if sides[0] + sides[1] < sides[2]:
        return False
    return sides[0] == sides[1] or sides[1] == sides[2]


def scalene(sides):
    sides = sorted(sides)
    if sides[0] + sides[1] < sides[2]:
        return False
    return sides[0] != sides[1] and sides[1] != sides[2]