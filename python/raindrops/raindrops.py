def convert(n):

    result = ''
    
    dropTypes = [
        (3, 'Pling'),
        (5, 'Plang'),
        (7, 'Plong'),
    ]

    for dropType in dropTypes:
        if (n % dropType[0] == 0):
            result = result + dropType[1]

    return result or str(n)
