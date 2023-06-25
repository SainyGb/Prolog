:- dynamic pos/2.
:- dynamic mochila_robo/1.

pos(tv, sala).
pos(bola,quarto).
pos(carteira, quarto).
pos(chave, garagem).
pos(robo, garagem).

mochila:-   findall(X, mochila_robo(X), Result),
            format("~w", [Result]).       

pegue(Objeto):- Objeto \= robo,
                pos(Objeto, Local),
    			pos(robo, Local),
                asserta(mochila_robo(Objeto)),
    			retract(pos(Objeto, Local)),
                format("Robo pegou ~w e colocou na mochila.", [Objeto]).

solte(Objeto):- pos(robo, Local),
                mochila_robo(Objeto),
                retract(mochila_robo(Objeto)),
                asserta(pos(Objeto, Local)),
                format("Robo soltou ~w na ~w.", [Objeto, Local]).

ande(Destino) :- pos(_, Destino),
                 retract(pos(robo, Origem)),
                 asserta(pos(robo, Destino)),
                 format('Robo andou de ~w para ~w', [Origem, Destino]).

estou :- pos(robo,X), format("Local atual do robo: ~w", [X]), !.

objetos :-  pos(robo,Local),
            findall(Objetos, (pos(Objetos, Local), Objetos \= robo), Result),
            format("Objetos: ~w", [Result]), !.
