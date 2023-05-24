:- dynamic joga/2.

joga(pele, futebol).
joga(romario, futebol).
joga(guga,tenis).
esporte(E) :- joga(_,E).

:- dynamic estou/1.
estou(nazare).

ando(Destino) :- retract(estou(Origem)),
    asserta(estou(Destino)),
    format('Andei de ~w para ~w', [Origem, Destino]).
