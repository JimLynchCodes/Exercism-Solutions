
DROP_TYPES = [
    (3, 'Pling'),
    (5, 'Plang'),
    (7, 'Plong'),
]


def convert(n):
    result = [sound for factor, sound in DROP_TYPES if n % factor == 0]
    return ''.join(result) or str(n)
