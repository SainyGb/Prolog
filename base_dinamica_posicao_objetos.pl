:- dynamic pos/2.

pos(tv, sala).
pos(bola,quarto).
pos(carteira, quarto).
pos(chave, garagem).
pos(robo, garabem).

ande(Destino) :- pos(_, Destino),
                 retract(pos(robo, Origem)),
                 asserta(pos(robo, Destino)),
                 format('Robo andou de ~w para ~w', [Origem, Destino]).

estou :- listing(pos(robo,_)).

objetos :- findall(Local, pos(robo, Local), Result),
           format("Objetos: ~w", [Result]).
