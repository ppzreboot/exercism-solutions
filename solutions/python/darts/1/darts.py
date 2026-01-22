def score(x, y):
    n = x * x + y * y
    if n <= 1:
        return 10
    if n <= 25:
        return 5
    if n <= 100:
        return 1
    return 0
