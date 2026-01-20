def is_armstrong_number(number):
    raw_number = number
    digits = []
    while True:
        if number == 0:
            break
        digits.append(number % 10)
        number //= 10
    return sum([n ** len(digits) for n in digits]) == raw_number
