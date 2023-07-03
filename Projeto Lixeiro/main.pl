/*DATA SET*/
/*
aresta(1, 2, 10).
aresta(2, 3, 20).
aresta(2, 4, 15).
aresta(3, 5, 30).
aresta(4, 5, 25).
aresta(4, 6, 40).
aresta(5, 7, 35).
aresta(6, 7, 45).
aresta(6, 8, 50).
aresta(7, 9, 55).
aresta(8, 9, 60).
aresta(8, 10, 12).
aresta(9, 10, 18).
aresta(10,10,22).
*/


aresta(1,2,21).
aresta(2,3,1).
aresta(3,1,2).
aresta(1,3,22).

/*DIJKSTRA*/

connected(X, Y, Peso) :- aresta(X, Y, Peso).
/*connected(X, Y, Peso) :- aresta(Y, X, Peso).*/

adjacente(X, Y) :- connected(X, Y, _).

caminho(X, Y, Path, Peso) :-
    dfs(X, Y, [X], Q, Peso),
    reverse(Q, Path).

dfs(X, Y, Path, [Y|Path], W) :-
    adjacente(X, Y),
    connected(X, Y, W).

dfs(X, Y, Visited, Q, Peso) :-
    adjacente(X, Z),
    Z \== Y,
    \+ member(Z, Visited),
    connected(X, Z, W),
    dfs(Z, Y, [Z|Visited], Q, W1),
    Peso is W + W1.

/*CAIXEIRO VIAJANTE*/
/* Finds the length of a list, while there is something in the list it increments N
	when there is nothing left it returns.*/

len([], 0).
len([_|T], N):- len(T, X), N is X+1 .

all_edges(Ls,Len):-
	findall(X,aresta(X,_,_),L),sort(L,Ls),length(Ls,Len).

/*Best path, is called by shortest_path.  It sends it the paths found in a
 path, distance format*/

best_path(Start,Visited, Total):- path(Start, Start, Visited, Total).


/*Path is expanded to take in distance so far and the nodes visited */

path(Start, Fin, Visited, Total) :- path(Start, Fin, [Start], Visited, 0, Total).

/*This adds the stopping location to the visited list, adds the distance and then calls recursive
	to the next stopping location along the path */

path(Start, Fin, CurrentLoc, Visited, Costn, Total) :-
    aresta(Start, StopLoc, Distance), NewCostn is Costn + Distance, \+ member(StopLoc, CurrentLoc),
    path(StopLoc, Fin, [StopLoc|CurrentLoc], Visited, NewCostn, Total).

/*When we find a path back to the starting point, make that the total distance and make
	sure the graph has touch every node*/

path(Start, Fin, CurrentLoc, Visited, Costn, Total) :-
	aresta(Start, Fin, Distance), 
    reverse([Fin|CurrentLoc], Visited), 
    len(Visited, Q),
    all_edges(_,Len),
    (Q =\= Len+1 -> Total is 100000; Total is Costn + Distance).

/*This is called to find the shortest path, takes all the paths, collects them in holder.
	Then calls pick on that holder which picks the shortest path and returns it*/

shortest_path(Path, Start):-setof(Cost-Path, best_path(Start,Path,Cost), Holder),pick(Holder,Path).

/* Is called, compares 2 distances. If cost is smaller than bcost, no need to go on. Cut it.*/

best(Cost-Holder,Bcost-_,Cost-Holder):- Cost<Bcost,!.
best(_,X,X).

/*Takes the top path and distance off of the holder and recursively calls it.*/

pick([Cost-Holder|R],X):- pick(R,Bcost-Bholder),best(Cost-Holder,Bcost-Bholder,X),!.
pick([X],X).


/*HELPER FUCTIONS*/
divide_pair(Start, Cost,Path) :-  shortest_path(Cost_Path, Start),
                            Cost_Path = Cost-Path.

pegar_cabeca([Cabeca|_], Cabeca).

pegar_cauda([_|Cauda], Cauda).





/*MAIN*/
:- dynamic pos/1.
:- dynamic pos_memoria/1.
:- dynamic peso_temp/2.

:- dynamic lixo/2.

:- dynamic mochila/2.
:- dynamic energia/2.

peso_temp(0,100000).

lixo(2,3).

lixao(1).
posto_recarga(2).
posto_recarga(3).

pos(1).

mochila(0,2).
energia(10,10).

estado_mochila :- mochila(Qtd_Lixo,Limite_Mochila),
                    format("A mochila tem ~w/~w",[Qtd_Lixo, Limite_Mochila]), !.

estou :- pos(X),
        format("O robo esta na pos ~w",[X]).

ir(X,Arestas,MenorPeso) :-
    pos(R),
    findall(P, caminho(R,X,_,P), List),
	min_list(List,MenorPeso),
    retract(pos(R)),
    asserta(pos(X)),
    caminho(R,X,Arestas,MenorPeso), !.


retornar_lixao :- pos(Atual),
                lixao(Lixao),
                asserta(pos_memoria(Atual)),
                ir(Lixao, _, _),
                write("O lixeiro voltou para o lixao").

retornar_rota :- pos_memoria(Ultima),
                retract(pos_memoria(_)),
                ir(Ultima,_,_),
                format("O lixeiro retornou para o local ~w", [Ultima]).

posto_proximo :- pos(Atual),
                findall(Postos, posto_recarga(Postos), Result),
                pegar_cabeca(Result, Cabeca),
                findall(Posto, caminho(Atual,Cabeca,_,Posto), List),
	            min_list(List,MenorPeso),
                retract(peso_temp(_,_)),
                asserta(peso_temp(Cabeca, MenorPeso)),
                pegar_cauda(Result, Cauda),
                posto_proximo(Cauda).

posto_proximo([]) :-peso_temp(Vertice, _),
                    retract(peso_temp(_,_)),
                    asserta(peso_temp(Vertice,100000)),
                    format("O posto mais proximo eh: ~w", [Vertice]).
                    

posto_proximo(Lista) :- pos(Atual),
                pegar_cabeca(Lista, Cabeca),
                findall(Posto, caminho(Atual,Cabeca,_,Posto), List),
	            min_list(List,MenorPeso),
                peso_temp(_, PesoAnterior),
                MenorPeso < PesoAnterior,
                retract(peso_temp(_,_)),
                asserta(peso_temp(Cabeca, MenorPeso)),
                pegar_cauda(Lista, Cauda),
                posto_proximo(Cauda).

posto_proximo(Lista) :- pos(Atual),
                pegar_cabeca(Lista, Cabeca),
                findall(Posto, caminho(Atual,Cabeca,_,Posto), List),
	            min_list(List,MenorPeso),
                peso_temp(_, PesoAnterior),
                MenorPeso >= PesoAnterior,
                pegar_cauda(Lista, Cauda),
                posto_proximo(Cauda).

ir_posto_proximo :- pos(Atual),
                    peso_temp(Vertice, _),
                    retract(peso_temp(_,_)),
                    asserta(peso_temp(0,100000)),
                    asserta(pos_memoria(Atual)),
                    ir(Vertice,_,_),
                    format("O lixeiro se moveu para o posto ~w", [Vertice]).

rota :- pos(X),
        divide_pair(X, Cost, Path),
        Cost < 100000,
        format("A melhor rota eh: ~w", [Path]).

seguir_rota :- pos(X),
                divide_pair(X, Cost, Path),
                Cost < 100000,
                pegar_cauda(Path, Cauda),
                pegar_cabeca(Cauda, Cabeca),
                aresta(X,Cabeca, Edge_Cost),
                energia(Energia_Atual,Limite),
                Edge_Cost =< Energia_Atual,
                Perda is Energia_Atual - Edge_Cost,
                retract(energia(_,_)),
                asserta(energia(Perda, Limite)),
                retract(pos(X)),
                asserta(pos(Cabeca)),
                format("O lixeiro se moveu da pos ~w para a pos ~w", [X,Cabeca]).

seguir_rota :- pos(X),
                divide_pair(X, Cost, Path),
                Cost < 100000,
                pegar_cauda(Path, Cauda),
                pegar_cabeca(Cauda, Cabeca),
                aresta(X,Cabeca, Edge_Cost),
                energia(Energia_Atual,_),
                Edge_Cost > Energia_Atual,
                write("Pouca energia, favor ir ate um posto de recarga com as baterias reservas.").


recarregar :- pos(X),
                posto_recarga(X),
                energia(_, Limite),
                retract(energia(_, _)),
                asserta(energia(Limite, Limite)),
                format("Energia recarregada: ~w/~w", [Limite,Limite]).

mostrar_energia :- energia(Atual,Limite),
                    format("Energia: ~w/~w", [Atual, Limite]).

lixo_local :- pos(Atual),
                lixo(Atual,Lixo),
                format("Ha ~w lixos na pos ~w ", [Lixo,Atual]), !.

coletar_lixo :- pos(Atual),
                lixo(Atual,Qtd_Lixo),
                Qtd_Lixo >= 1,
                mochila(Qtd_Mochila, Limite_Mochila),
                Qtd_Mochila < Limite_Mochila,
                Qtd_Mochila_Nova is Qtd_Mochila + 1,
                Qtd_Lixo_Novo is Qtd_Lixo - 1,
                Qtd_Lixo_Novo =< Limite_Mochila,
                retract(lixo(_,_)),
                asserta(lixo(Atual, Qtd_Lixo_Novo)),
                retract(mochila(_,_)),
                asserta(mochila(Qtd_Mochila_Nova, Limite_Mochila)),
                format("O lixeiro coletou um lixo da pos ~w ", [Atual]),
                nl,
                lixo_local,
                nl,
                estado_mochila.

coletar_lixo :- mochila(Qtd_Mochila, Limite_Mochila),
                Qtd_Mochila >= Limite_Mochila,
                write("O lixeiro esta carregando lixo demais, va ate o lixao e descarregue o lixo.").


descarregar_lixo :- pos(Atual),
                    lixao(Lixao),
                    mochila(Qtd_Mochila, Limite_Mochila),
                    Lixao = Atual,
                    retract(mochila(_,_)),
                    asserta(mochila(0, Limite_Mochila)),
                    format("O lixeiro descarregou ~w lixos no lixao", [Qtd_Mochila]).
                

