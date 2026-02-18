#!/bin/python3

with open("long.tex", "r") as fh:
    search = ["land", "lor"]

    string = fh.read()
    pos = 0
    count = 0
    chars = 0
    print("& ", end="")
    while pos >= 0:
        prev = pos
        pos1 = string.find("\\land", pos + 1)
        pos2 = string.find("\\lor", pos + 1)
        pos3 = string.find("(", pos + 1)
        pos = min(pos1, pos2)
        chars += pos - prev - 4
        chars += 5 * string[prev:pos].count("(")
        chars += 2 * string[prev:pos].count(")")
        print("{}".format(string[prev:pos]), end="")
        count += 1
        if (count > 8) or (chars > 128):
            print("\\\\\n& ", end="")
            chars = 0
            count = 0


print()

