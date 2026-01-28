def tick(matrix):
    if len(matrix) == 0:
        return []
    row_num = len(matrix)
    row_length = len(matrix[0])
    result = []
    for row_index in range(row_num):
        new_row = []
        result.append(new_row)
        for index in range(row_length):
            anc = count(matrix, row_num, row_length, row_index, index) # anc: alive neighbor count
            alive = matrix[row_index][index]
            new_row.append(
                alive and (anc == 2 or anc == 3)
                or not alive and anc == 3
            )
    return result

def count(matrix, row_num, row_length, row_index, index):
    neighbors = [
        (index, row_index + 1), # top
        (index + 1, row_index + 1), # top right
        (index + 1, row_index), # right
        (index + 1, row_index - 1), # right bottom
        (index, row_index - 1), # bottom
        (index - 1, row_index - 1), # bottom left
        (index - 1, row_index), # left
        (index - 1, row_index + 1) # left top
    ]
    return len([n
        for n in neighbors
        if (
            0 <= n[0] < row_length # out of range
            and 0 <= n[1] < row_num # out of range
            and matrix[n[1]][n[0]]
        )
    ])