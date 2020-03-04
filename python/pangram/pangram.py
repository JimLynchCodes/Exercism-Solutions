import string
import re

def is_pangram(sentence):
    
    lowered = sentence.lower()
    letters = re.findall('[a-z]', lowered)
    uniques = set(letters)

    return len(uniques) == 26
    
