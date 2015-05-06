all:
	happy -gca Parpastiny.y
	alex -g Lexpastiny.x
	ghc --make Testpastiny.hs -o Testpastiny
	ghc --make interpreter.hs -o interpreter
    
interpreter: 
	Abspastiny.hs pastiny.hs Lexpastiny.hs Parpastiny.hs Skelpastiny.hs Printpastiny.hs ErrM.hs Interpret.hs pastiny.hs
	ghc --make interpreter.hs -o interpreter
    
interpreter: 
	Abspastiny.hs pastiny.hs Lexpastiny.hs Parpastiny.hs Skelpastiny.hs Printpastiny.hs ErrM.hs Interpret.hs pastiny.hs
	ghc --make interpreter.hs -o interpreter
	
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Docpastiny.ps

distclean: clean
	-rm -f Docpastiny.* Lexpastiny.* Parpastiny.* Layoutpastiny.* Skelpastiny.* Printpastiny.* Testpastiny.* Abspastiny.* Testpastiny ErrM.* SharedString.* ComposOp.* pastiny.dtd XMLpastiny.* Makefile*
	

