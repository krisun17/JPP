male(a).
male(b).
female(c).
female(d).

brother(X,Y) :- female(X), !, fail.
brother(X,Y) :- female(Y), !, fail.
brother(X,Y) :- male(X), male(Y).