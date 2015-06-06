%dziecko(Dziecko, Matka, Ojciec, płeć)
dziecko(jasio, ewa, jan, ch).
dziecko(stasio, ewa, jan, ch).
dziecko(basia, anna, piotr, dz).
dziecko(jan, ela, jakub, ch).

syn(Dziecko, Matka, Ojciec) :- dziecko(Dziecko, Matka, Ojciec, ch).
dziecko(Dziecko, Matka, Ojciec) :- dziecko(Dziecko, Matka, Ojciec, _).
