#!/usr/bin/env python3

DAY = '01'
FILENAME = 'input'

def solver(data, digit_selector):
    acc = 0
    l = len(data)
    for i, digit in enumerate(data):
        other = data[i - digit_selector(l)] # Utilize python's negative indexing
        if digit == other:
            acc += digit
    return acc


SOLVE_1 = lambda length: 1
SOLVE_2 = lambda length: length // 2

if __name__ == '__main__':
    data = []
    with open(FILENAME) as f:
        data = list(map(int, f.read()[:]))

    for i, digit_selector in enumerate([SOLVE_1, SOLVE_2]):
        print(f'{DAY}.{i+1} answer:', solver(data, digit_selector))

# $
# 01.1 answer: 1141
# 01.2 answer: 950
