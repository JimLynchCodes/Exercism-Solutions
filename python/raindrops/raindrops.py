def convert(n):

    result = ''
    dropMap = {
        3: 'Pling',
        5: 'Plang',
        7: 'Plong'
    }

    for dropNum in dropMap:
        if (n % dropNum == 0):
            result = result + dropMap[dropNum]

    if (result == ''):
        result = str(n)

    return result
