def is_isogram(string):
    c_list = [c for c in string.lower() if c != ' ' and c != '-']
    return len(c_list) == len(set(c_list)) 
    