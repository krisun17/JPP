%nat(x) wtw, gdy x jest liczbą naturalną
%plus(x,y,z) wtw, gdy x + y = z
%minus(x,y,z) wtw, gdy x - y = z
%fib(k,n)

nat(z).
nat(s(N)) :- nat(N).

plus(X,z,X).
plus(X,s(Y),s(Z)) :- plus(X,Y,Z).

minus(X,Y,Z) :- plus(Y,Z,X).

fib(z,z).
fib(s(z),s(z)).
%fib(s(s(z)),s(z)).
fib(s(s(K)),N) :- fib(K,M), fib(s(K),L), plus(M,L,N).  

%fib,ktory sie nie zapetli (trzeba wiedziec jak program bedzie uzywany!
fib(s(s(K)),s(s(N))) :- fib(K,s(M)), fib(s(K),s(L)), plus(M,L,N).  
