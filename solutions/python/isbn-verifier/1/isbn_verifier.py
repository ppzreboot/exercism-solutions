def is_valid(isbn):
    isbn = isbn.replace('-', '')
    if (
        len(isbn) != 10
        or not isbn[:9].isdecimal()
        or not (isbn[9].isdecimal() or isbn[9] == 'X')
    ):
        return False

    sum = 0
    for i in range(10):
        sum += 10 if isbn[i] == 'X' else int(isbn[i]) * (10 - i)
    return sum % 11 == 0