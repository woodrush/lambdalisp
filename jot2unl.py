import sys


if len(sys.argv) == 1:
    print("Error: filename not provided")
    exit()

with open(sys.argv[1], "rt") as f:
    code = f.read()


ret = "{}"
for c in reversed(code):
    if c == "0":
        ret = ret.format("``{}sk")
    elif c == "1":
        ret = ret.format("`s`k{}")

ret = ret.format("i")

print(ret, end="")
