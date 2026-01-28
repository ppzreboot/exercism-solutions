def is_pangram(sentence):
    all_letters = {
        chr(i)
        for i in range(ord('a'), ord('z') + 1)
    }
    for c in sentence.lower():
        if len(all_letters) == 0:
            return True
        all_letters.discard(c)
    return len(all_letters) == 0
        