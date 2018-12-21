#!/usr/bin/python3

import fileinput
import random
import sys

def resample_lines(lines, resample_size):
    # Print CSV header line
    print(lines[0], end='')
    data_lines = lines[1:]
    if not data_lines:
        return
    for i in range(resample_size):
        print(random.choice(data_lines), end='')

if __name__ == '__main__':
    if len(sys.argv) <= 1:
        print("Please specify a resample size")
        exit()

    lines = list(fileinput.input(files=['-']))
    if not lines:
        exit()
    resample_lines(lines, int(sys.argv[1]))
