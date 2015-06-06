
male(a).
male(b).
female(c).
female(d).

brother(X,Y) :- female(X), !, fail.
brother(X,Y) :- female(Y), !, fail.
brother(X,Y) :- male(X), male(Y).

sister(X,Y) :- male(X),!,fail.

sister(X,Y,Z) :- 
	( female(X) 
	-> Z = write([ prod('E', [[nt('E'), +, nt('T')],  [nt('T')]]), prod('T', [[id],  ['(', nt('E'), ')']])   ])
	; Z = Y ).