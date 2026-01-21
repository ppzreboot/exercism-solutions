def response(hey_bob):
    hey_bob = hey_bob.strip()
    is_question = hey_bob.endswith('?')
    is_yelling = hey_bob.isupper()
    if is_question and is_yelling:
        return "Calm down, I know what I'm doing!"
    if is_question:
        return 'Sure.'
    if is_yelling:
        return 'Whoa, chill out!'
    if len(hey_bob) == 0:
        return 'Fine. Be that way!'
    return 'Whatever.'