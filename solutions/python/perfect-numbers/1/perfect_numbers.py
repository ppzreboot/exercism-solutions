def classify(number):
    """ A perfect number equals the sum of its positive divisors.

    :param number: int a positive integer
    :return: str the classification of the input integer
    """
    if number < 1:
        raise ValueError("Classification is only possible for positive integers.")
    if number == 1:
        return 'deficient'
    s = sum(get_n(number))
    if s == number:
        return 'perfect'
    if s > number:
        return 'abundant'
    return 'deficient'
        
def get_n(number):
    list = [1]
    for i in range(2, number):
        if number % i == 0:
            list.append(i)
    return list
