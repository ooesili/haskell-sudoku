all: sudoku

sudoku: sudoku.hs
	ghc -Wall $^ -o $@

clean:
	rm sudoku{.hi,.o,}
