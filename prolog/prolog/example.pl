% uruchamianie: sicstus
% kompilacja: [plik].
% przejscie w tryb definicji: [user].

% dziecko(Dziecko, Matka, Ojciec)
dziecko(jasio, ewa, jan).
dziecko(stasio, ewa, jan).
dziecko(basia, anna, piotr).
dziecko(jan, ela, jakub).
dziecko(ela, maria, jacek).

%ojciec(Dziecko, Ojciec)
%matka(Dziecko, Matka)
%rodzic(Dziecko, Rodzic)
%babcia(Dziecko, Babcia)
%wnuk(Wnuk, Babcia)
%przodek(Przodek, Potomek)

ojciec(D,O) :- dziecko(D,_,O).
matka(D,M) :- dziecko(D,M,_).
rodzic(D,R) :- ojciec(D,R).
rodzic(D,R) :- matka(D,R).
babcia(D,B) :- rodzic(D,R),matka(R,B).
przodek(Prz,Pot) :- rodzic(Pot,Prz).
przodek(Prz,Pot) :- rodzic(Pot,R),przodek(Prz,R).
