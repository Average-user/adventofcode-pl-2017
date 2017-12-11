#!/usr/bin/env python3

f = open("Inputs/day10.txt").read()

lengths1 = [int(x) for x in f.strip().split(",")]
lengths2 = [ord(x) for x in f.strip()] + [17, 31, 73, 47, 23]
print(lengths2)

def run(lengths, times):
    position = 0
    skip = 0

    sequence = list(range(256))

    for _ in range(times):
        for l in lengths:
            for i in range(l // 2):
                now = (position + i) % len(sequence)
                later = (position + l - 1 - i) % len(sequence)
                sequence[now], sequence[later] = sequence[later], sequence[now]

            position += l + skip
            skip += 1

    return sequence

sequence1 = run(lengths1, 1)
sequence2 = run(lengths2, 1)
print(sequence2)


hashstr = ""
for i in range(len(sequence2) // 16):
    num = 0
    for j in range(16):
        num ^= sequence2[i * 16 + j]
    hashstr += hex(num)[2:].zfill(2)

print l
print(sequence1[0] * sequence1[1])
print(hashstr)
