def round_scores(student_scores):
    return [round(s) for s in student_scores]

def count_failed_students(student_scores):
    return len([
        s
        for s in student_scores
        if s <= 40
    ])

def above_threshold(student_scores, threshold):
    return [
        s
        for s in student_scores
        if s >= threshold
    ]

def letter_grades(highest):
    step = (highest - 41) / 4
    D = 41
    C = round(D + step)
    B = round(C + step)
    A = round(B + step)
    return [D, C, B, A]

def student_ranking(student_scores, student_names):
    result = []
    for i in range(len(student_scores)):
        result.append(str(i + 1) + '. ' + student_names[i] + ': ' + str(student_scores[i]))
    return result

def perfect_score(student_info):
    for info in student_info:
        if info[1] == 100:
            return info
    return []
