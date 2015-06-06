:- use_module(library(lists)).
                                                  % nie LL(1), LeftRec
grammar(ex1, [prod('E', [[nt('E'), '+', nt('T')],  [nt('T')]]),
              prod('T', [[id],  ['(', nt('E'), ')']])   ]).


grammar(ex2, [prod('A', [[nt('A'), x], [x]])]).    % nie LL(1), LeftRec

grammar(ex3, [prod('A', [[x, nt('A')], [x]])]).    % nie LL(1)

                                                   % LL(2)
grammar(ex4, [prod('A', [[a, nt('B')], [a, nt('C')]]),
              prod('B', [[b]]),
              prod('C', [[c]])   ]).

                                                   % LL(1)
grammar(ex5, [prod('A', [[a, nt('R')]]),
              prod('R', [[nt('B')], [nt('C')]]),
              prod('B', [[b]]),
              prod('C', [[c]])   ]).

                                                   % LL(1), nie jest SLR(1)
grammar(ex6, [prod('S', [[nt('A'), a, nt('A'), b],
                         [nt('B'), b, nt('B'), a]]),
              prod('A', [[]]),
              prod('B', [[]])    ]).

grammar(ex7, [prod('A', [[a], [nt('B'), x]]),
              prod('B', [[b], [nt('A'), y]]) ]).

grammar(ex8, [prod('A', [[nt('A'), a]]) ]).        % nie można usunąć LeftRec

grammar(ex9, [prod('A', [[nt('R')]]),           % jest cykl
              prod('R', [[nt('B')], [nt('C')]]),
              prod('B', [[b], [nt('A')]]),
              prod('C', [[c]])   ]).

grammar(ex10, [prod('F', [['(', nt('L'), ')']]),           % jfirst
              prod('L', [[nt('L'),nt('E')], []]),
              prod('E', [[nt('F')], ['a']]) ]).

%sprawdzenie czy jest LL1
teSamePocz( [X | Xs], [X | Ys]).
teSamePoczLista( [X,Y | Xs] ) :- teSamePocz(X,Y).
teSamePoczLista( [X,Y | Xs] ) :- teSamePoczLista( [X | Xs] ).
teSamePoczLista( [X | Xs] ) :- teSamePoczLista( Xs ).

jestLL1( [ prod(NT, TM) | TProd] ) :- teSamePoczLista( [[nt(NT)] | TM] ), !, fail.
jestLL1( [ prod(NT, TM) | TProd] ) :- teSamePoczLista( TM ), !, fail.
jestLL1( [ prod(NT, TM) | TProd] ) :- teSamePoczLista( TProd ).
jestLL1( Gram ).

%sprawdzenie cyklu

jestNt([ prod(A, L) | Ls ], nt(A)).
jestNt([ prod(A, L) | Ls ], C) :- jestNt(Ls, C).

%wezProd( [], A, prod(A, []) ).
wezProd([ prod(A, L) | Ls ], A, prod(A, L)).
wezProd([ prod(X, L) | Ls ], A, W) :- wezProd( Ls, A, W).

listaNt( [], [] ).
listaNt( [ prod(A, L) | Ls ], [A | Wyn] ) :- listaNt( Ls, Wyn ).

jestPuste(A, prod(A, [[] | L]) ).
jestPuste(A, prod(A, [X | L]) ) :- jestPuste(A, prod(A, L) ).

jestKrawedzProd(nt(A), nt(B), prod(A, [ [nt(F) | [] ] | Xs])) :- nt(F)==nt(B).
jestKrawedzProd(nt(A), nt(B), prod(A, [X | Xs])) :- jestKrawedzProd(nt(A), nt(B), prod(A, Xs)).

jestKrawedz(nt(A), nt(B), [X | Xs] ) :- jestKrawedzProd(nt(A), nt(B), X).
jestKrawedz(nt(A), nt(B), [X | Xs]) :- jestKrawedz(nt(A), nt(B), Xs).

path(nt(A),nt(B),G) :- travel(nt(A),nt(B),[nt(A)],G).
travel(nt(A),nt(B),Vis,G) :- jestNt(G,C), jestKrawedz(nt(A),nt(C),G), nt(B) \== nt(C), \+member(nt(C),Vis), travel(nt(C),nt(B),[nt(C)|Vis],G).
travel(nt(A),nt(B),Vis,G) :- jestKrawedz(nt(A),nt(B),G).

jestCykl(G) :- jestNt(G,A), jestNt(G,B), jestKrawedz(nt(A),nt(B),G), path(nt(B), nt(A), G).

% sprawdzenie lewostronnej rekurencji

%sortProdAcc(prod(A, [ [nt(A) | Sym] | T ]), prod(A, Pos), R):-  sortProdAcc( prod(A, T), prod(A, [ [nt(A) | Sym] | Pos ]), R). 
%sortProdAcc(prod(A, [ X | T ]), prod(A, Pos), R) :- append(Pos, [X], Ls), sortProdAcc( prod(A, T), prod(A, Ls), R).
%sortProdAcc(prod(A,[]),X,X).

%sortProd(L,R):-  sortProdAcc(L,prod(A,[]),R).

addLastElem([], S, []).
addLastElem( [X | Xs], S, [NR | NL] ) :- append(X,[S],NR), addLastElem(Xs, S, NL).

getNTerm( prod(A, []), prod(A, []) ).
getNTerm( prod(A, [ [nt(A) | Sym] | T ]), prod(A,  [ Sym | Wyn ]) ) :- getNTerm( prod(A, T), prod(A, Wyn) ).
getNTerm( prod(A, [ X | T ]), prod(A, Wyn) ) :- getNTerm( prod(A, T), prod(A, Wyn) ).

getOTerms( prod(A, []), prod(A, []) ).
getOTerms( prod(A, [ [nt(A) | Sym] | T ]), prod(A, Wyn) ) :- getOTerms( prod(A, T), prod(A, Wyn) ).
getOTerms( prod(A, [ X | T ]), prod(A, [ X | Wyn ]) ) :- getOTerms( prod(A, T), prod(A, Wyn) ).

notRec( prod(A, []) ).
notRec( prod(A, [ [X | Sym] | T ]) ) :- X \== nt(A), notRec( prod(A, T) ).

remLeftOneProd( prod(A,L), [prod(A, L)] ) :- notRec( prod(A, L) ).
remLeftOneProd( prod(A,L), [prod(A, MPr), prod(R, NPo)] ) :- getNTerm( prod(A,L), prod(A,NTerms)), getOTerms( prod(A,L), prod(A,OTerms) ), 
atom_concat(A,'1',R), addLastElem(OTerms, nt(R), MPr), addLastElem(NTerms, nt(R), NPr), append(NPr, [['eps']], NPo).


remDirectLeftRec([], []).
remDirectLeftRec( [Prod | Tprod], W ) :- append(NPr, N, W), remLeftOneProd( Prod, NPr ), remDirectLeftRec( Tprod, N ).

remLeftRec(G, Wyn) :- jestCykl(G), !, fail.
remLeftRec(G, Wyn) :- remDirectLeftRec(G, Wyn).

%zbiory first i follow

first( prod(A, []), [], G).
first( prod(A, [ [nt(X) | Sym] | L]), Wyn, G ) :- A \== X, wezProd(G, X, P), first( P, WynX, G ), first( prod(A, L), WynA, G ), append(WynX, WynA, Wyn).
first( prod(A, [ [nt(X) | Sym] | L]), Wyn, G ) :- A == X, first( prod(A, [Sym | L] ), Wyn, G ).
first( prod(A, [ [X | Sym] | L]), [ X | WynA], G ) :- \+jestNt(G, X), first( prod(A, L), WynA, G ).
first( prod(A, [ [] | L]), WynA, G ) :- first( prod(A, L), WynA, G ).

firstL( A, G, Wyn) :- wezProd(G, A, P), first( P, Wyn, G).

followOne( A, prod(X, []), [], G).
followOne( A, prod(X, [ [nt(A) | [nt(B) | Sym]] | L]), Wyn, G) :- A \== B, firstL(B, G, WynB), followOne( A, prod(X, [ [nt(B) | Sym] | L]), WynA, G), append(WynA, WynB, Wyn).
followOne( A, prod(X, [ [nt(A)] | L]), Wyn, G) :- followAll( X, G, WynX, G), followOne( A, prod(X, L), WynA, G), append(WynA, WynX, Wyn).
followOne( A, prod(X, [ [nt(A) | [B | Sym]] | L]), [ B | WynA ], G) :- \+jestNt(G,B), followOne( A, prod(X, [ [B | Sym] | L]), WynA, G).
followOne( A, prod(X, [ [B | Sym] | L]), Wyn, G) :- nt(A) \== B, followOne( A, prod(X, [ Sym | L]), Wyn, G).
followOne( A, prod(X, [ [] | L]), Wyn, G) :- followOne( A, prod(X, L), Wyn, G).

followAll(A, [], [], G).
followAll(A, [ P | XP ], Wyn, G) :- followOne(A, P, WynP, G), followAll(A, XP, WynXP, G), append(WynP, WynXP, Wyn).

followList(G, [], []).
followList(G, [ A | L ], [ follow(A,Wyn) | WynP ] ) :- followAll(A, G, Wyn1, G), removeDups(Wyn1, Wyn), followList(G, L, WynP).

follow(G, Wyn) :- listaNt(G,L), followList(G,L,Wyn).

%zbiory select

selectOne(A, [], [], G).
selectOne(A, [], Wyn, G) :- followAll(A, G, Wyn, G).
selectOne(A, [ X | L ], [X], G) :- \+jestNt(G,X).
selectOne(A, [ nt(X) | L ], Wyn, G) :- wezProd(G,X,P), jestPuste(X,P), firstL(X,G,WynX), selectOne(A, L, WynL, G), append(WynX, WynL, Wyn).
selectOne(A, [ nt(X) | L ], Wyn, G) :- wezProd(G,X,P), \+jestPuste(X,P), firstL(X, G, Wyn).

removeDups( [], [] ).
removeDups( [ X | L], W ) :- member(X,L), !, removeDups( L, W ).
removeDups( [ X | L], [X | W] ) :- removeDups( L, W ).

selectRem(A, L, W, G) :- selectOne(A, L, W1, G), removeDups(W1,W).

selectProd( prod(A, []), [], G).
selectProd( prod(A, [ X | L ] ), [WA | W], G) :- selectRem(A, X, WA, G), selectProd( prod(A, L), W, G).

selectAll( [], [], G ).
selectAll([ P | XP ], Wyn, G ) :- selectProd( P, WP, G), selectAll( XP, W, G), append(WP, W, Wyn).

select(G,W) :- selectAll(G,W,G).

% sprawdzamy czy jest LL1

inter([], L2).
inter([H1 | T1], L2) :- \+member(H1, L2), inter(T1, L2).

pustePrz( [ X1, X2 | L ] ) :- inter(X1, X2), pustePrz( [ X1 | L ] ), pustePrz( [ X2 | L ] ).
pustePrz( [X] ).

prodLL1( P, G ) :- selectProd( P, W, G), pustePrz(W).

czyLL1( [], G).
czyLL1( [P | X], G) :- prodLL1(P, G), czyLL1(X,G).

jestLL21(G) :- czyLL1(G,G).

