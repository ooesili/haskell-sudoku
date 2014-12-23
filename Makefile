all: sudoku

sudoku: sudoku.hs
	ghc -Wall $^ -o $@

clean:
	rm -f sudoku{.hi,.o,}
