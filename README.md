# Sudoku

This is a solver for the popular puzzle game *sudoku*.

In its current state, the program takes a sudoku from a text file and returns its solution\
(if any) to stdout.


The input format consists of a string of exactly 81 integers separated by (any combination\
of) whitespace.

An example for a valid input file is *testsudoku1.txt*, which contains:

---

5 3 0 0 7 0 0 0 0\
6 0 0 1 9 5 0 0 0\
0 9 8 0 0 0 0 6 0

8 0 0 0 6 0 0 0 3\
4 0 0 8 0 3 0 0 1\
7 0 0 0 2 0 0 0 6

0 6 0 0 0 0 2 8 0\
0 0 0 4 1 9 0 0 5\
0 0 0 0 8 0 0 7 9

---




The call "sudoku testsudoku1.txt" returns:

[5,3,4,6,7,8,9,1,2]\
[6,7,2,1,9,5,3,4,8]\
[1,9,8,3,4,2,5,6,7]\
[8,5,9,7,6,1,4,2,3]\
[4,2,6,8,5,3,7,9,1]\
[7,1,3,9,2,4,8,5,6]\
[9,6,1,5,3,7,2,8,4]\
[2,8,7,4,1,9,6,3,5]\
[3,4,5,2,8,6,1,7,9]



---
(I'll probably change the output format later.)



# What it does



 - TODO


