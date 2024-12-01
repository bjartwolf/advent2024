import builtins
from day_1_Core.program import (distance, product_of_input)
from fable_modules.fable_python.stdlib.builtins_ import print

file1: str = builtins.open("..\\input1.txt", mode = "r").read()

file2: str = builtins.open("..\\input2.txt", mode = "r").read()

print(distance(file1))

print(distance(file2))

print(product_of_input(file1))

print(product_of_input(file2))

