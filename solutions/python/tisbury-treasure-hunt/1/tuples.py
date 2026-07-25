def get_coordinate(record):
    return record[1]


def convert_coordinate(coordinate):
    return (coordinate[0], coordinate[1])


def compare_records(azara_record, rui_record):
    return convert_coordinate(azara_record[1]) == rui_record[1]


def create_record(azara_record, rui_record):
    if compare_records(azara_record, rui_record):
        return azara_record + rui_record
    return 'not a match'

def stringify(group):
    return f"('{group[0]}', '{group[2]}', ('{group[1][0]}', '{group[1][1]}'), '{group[4]}')\n"
    
def clean_up(combined_record_group):
    result = ''
    for g in combined_record_group:
        result += stringify(g)
    return result
