all:
	happy -gca ParexpGram.y
	alex -g LexexpGram.x
	ghc --make TestexpGram.hs -o TestexpGram

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocexpGram.ps

distclean: clean
	-rm -f DocexpGram.* LexexpGram.* ParexpGram.* LayoutexpGram.* SkelexpGram.* PrintexpGram.* TestexpGram.* AbsexpGram.* TestexpGram ErrM.* SharedString.* ComposOp.* expGram.dtd XMLexpGram.* Makefile*
	

