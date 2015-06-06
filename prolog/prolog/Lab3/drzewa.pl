drzewo(d).
drzewo(p1).
drzewo(p2).
drzewo(l1).
drzewo(l2).
prawySyn(p1,d).
lewySyn(l1,d).
prawySyn(p2,p1).
lewySyn(l2,l1).

%drzewo(NULL). 
%drzewo(D) :- lewySyn(L,D), prawySyn(R,D), drzewo(L), drzewo(R).

drzewo(nil).
drzewo(tree(L,_,P)) :- drzewo(L), drzewo(P).

insert(nil, Elem, tree(nil, Elem, nil)).
%wnie wstawia wielokrotnie insert(tree(L,E,P),E,tree(L,E,P)) :- E =< W, !, insert(L,E,NL).
insert(tree(L,W,P),E,tree(NL,W,P)) :- E =< W, !, insert(L,E,NL).
insert(tree(L,W,P),E,tree(L,W,NP)) :- E > W, insert(L,E,NP).

%notmember(X,[]).
notmember(X,[Y|L]) :- X =\= Y, notmember(Y,L).
notmember(X,[]).

not(Goal) :- call(Goal),!,fail.
not(Goal). 

edge(a,b).
edge(a,d).
edge(a,c).
edge(b,c).

pathC(A,B,P) :- path(A,B,[A],P).

path(A,B,Vis,[A|P]) :- edge(A,C), not(member(C,Vis)), path(C,B,[C | Vis], [A|P]).


