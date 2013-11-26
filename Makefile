all: sudoku

sudoku: sudoku.hs
	ghc -Wall $^ -o $@
