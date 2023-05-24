:- dynamic lampada/1.

lampada(ligada).

liga :- retract(lampada(_)), asserta(lampada(ligada)).
desliga :- retract(lampada(_)), asserta(lampada(desligada)).
