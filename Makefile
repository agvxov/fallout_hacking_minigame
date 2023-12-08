.PHONY: run

OUT := main

main: main.hs
	ghc -o ${OUT} main.hs

run: main
	./${OUT}

clean:
	-rm ${OUT}
