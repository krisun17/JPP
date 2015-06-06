lista([]).
lista([X|L]) :- lista(L).

pierwszy(X,[X|L]) :- lista(L).

element(X, [X|_]).                                
element(X, [_|Tail]) :- element(X, Tail). 

scal([],X,X) :- lista(X).
scal(X,[],X) :- lista(X).
scal([X|Xs],Y,[X|Zs]) :- scal(Xs,Y,Zs).

reverse([],[]).
reverse([X|Xs],YsX) :- reverse(Xs,Ys), scal(Ys,[X],YsX).

ostatni(X,L) :- reverse(L,R), pierwszy(X,R).

intersect([X|Xs],[X|Ys]).
intersect([X|Xs],Ys) :- intersect(Xs,Ys).
intersect(Xs,[X|Ys]) :- intersect(Xs,Ys).

podziel([],[],[]).
podziel(X,[],X).
podziel([X,Y|L], [X|Xs], [Y|Ys]) :- podziel(L,Xs,Ys).
