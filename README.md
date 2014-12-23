haskell-sudoku
====================

### sudoku solver written in Haskell


Usage
--------------------

Usage of the program is straight forward.  Running the program without
arguments causes the puzzle to be read from standard input:

```bash
$ ./sudoku < tests/hard
```

It can also be run with a single argument specifying a file containing a
puzzle:

```bash
$ ./sudoku tests/hard
```


Puzzle Format
--------------------

The puzzle format is quite simple as well.  It is a plain text file that can be
best explained with an example:

```
-------------
| 6 |  2| 84|
|23 |4  | 6 |
| 84|   |3  |
-------------
|   |64 |  5|
|8 1|7  |6  |
|   | 3 | 2 |
-------------
|   |26 |   |
|   |   | 13|
|  2|8  |   |
-------------
```

The program output uses the same format.  Here is the output corresponding to
the previous example:

```
-------------
|167|352|984|
|235|489|167|
|984|176|352|
-------------
|329|641|875|
|841|725|639|
|756|938|421|
-------------
|413|267|598|
|678|594|213|
|592|813|746|
-------------
```
