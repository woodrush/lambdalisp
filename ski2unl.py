import sys
import re

if len(sys.argv) == 1:
    print("Error: filename not provided")
    exit()

with open(sys.argv[1], "rt") as f:
    code = f.read()

code = f"({code})"

stack = [[]]
for c in code:
    if c == "(":
        stack.append([])
    elif c == ")":
        expr = ("`" * (len(stack[-1]) - 1)) + "".join(stack[-1])
        stack[-2].append(expr)
        del stack[-1]
    elif c in ["S", "K", "I"]:
        stack[-1].append(c.lower())

print(stack[0][0], end="")
