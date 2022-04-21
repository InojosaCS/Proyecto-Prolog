:- discontiguous([inicial/2,final/2,movimiento/3,moverse/4,legal/2]).

dls(Problema,(Estado,P,Cota),Historia,[Movimiento|Movimientos]) :-
  P < Cota,
	movimiento(Problema,Estado,Movimiento),
	moverse(Problema,Estado,Movimiento,Proximo),
	legal(Problema,Proximo),
	\+ member(Proximo,Historia),
  P1 is P + 1,
	dls(Problema,(Proximo,P1,Cota),[Proximo|Historia],Movimientos).
dls(Problema,(Estado,Profundidad,Profundidad),_,[]) :- final(Problema,Estado), !.

iddfs(Problema,Estado,Movimientos) :- 
  iddfs(Problema,Estado,Movimientos,0).
iddfs(Problema,Estado,Movimientos,N) :- 
  dls(Problema,(Estado,0,N),[Estado],Movimientos), !.
iddfs(P,E,M,N) :- N1 is N+1, iddfs(P,E,M,N1).

resolver(Problema,Movimientos) :- 
  inicial(Problema,Estado),
  iddfs(Problema,Estado,Movimientos).

simular(Problema) :- 
  inicial(Problema,Estado),
  iddfs(Problema,Estado,Movimientos),
  mostrar(Problema,Estado,Movimientos).

vagones(X,Meta,Operaciones) :-
  Estado = vagones(abajo,X,[],[],Meta),
  iddfs(vagones,Estado,Operaciones).

% Push above
movimiento(vagones,vagones(_,Izquierda,_,_,_),Movimiento) :-
  length(Izquierda, N),
  insertar_trenes(arriba,Movimiento,N).
% Push below
movimiento(vagones,vagones(_,Izquierda,_,_,_),Movimiento) :-
  length(Izquierda, N),
  insertar_trenes(abajo,Movimiento,N).
% Pop above
movimiento(vagones,vagones(_,Izquierda,_,_,_),Movimiento) :-
  length(Izquierda, N),
  quitar_trenes(arriba,Movimiento,N).
% Pop below
movimiento(vagones,vagones(_,Izquierda,_,_,_),Movimiento) :-
  length(Izquierda, N),
  quitar_trenes(abajo,Movimiento,N).
  
insertar_trenes(Destino,Movimiento,N) :-
  N > 0,
  Movimiento = push(Destino,N).
insertar_trenes(Destino,Movimiento,N) :-
  N > 0, N1 is N-1,
  insertar_trenes(Destino,Movimiento,N1).
quitar_trenes(Destino,Movimiento,N) :-
  N > 0,
  Movimiento = pop(Destino,N).
quitar_trenes(Destino,Movimiento,N) :-
  N > 0, N1 is N-1,
  quitar_trenes(Destino,Movimiento,N1).

toma(0,_,Ys) :- Ys = [], !.
toma(N,[H|Xs],[H|Ys]) :-
  N1 is N-1,
  toma(N1,Xs,Ys).


final(vagones,vagones(_,Meta,_,_,Meta)).

%%%%%%%%%%%%%%%%%%%%%%%%%% EJEMPLOS PARA LA BASE DE DATOS %%%%%%%%%%%%%%%%%%%%%%
/*
 * Problema del lobo, la chivo y el repollo.
 *
 * Representaremos el Estado del problema como
 *
 *    lcr(Bote,OrillaIzquierda,OrillaDerecha)
 *
 * donde Bote indica la posición como izquierda o derecha, mientras que
 * OrillaIzquierda y OrillaDerecha son listas con atomes (lobo, chivo
 * y repollo) que indican la carga que * hay en cada orilla. Ambas
 * listas se mantendrán ordenadas según lobo < chivo < repollo.
 *
 */
inicial(lcr,lcr(izquierda,[lobo,chivo,repollo],[])).

final(lcr,lcr(derecha,[],[lobo,chivo,repollo])).

movimiento(lcr,lcr(izquierda,I,_),Carga) :- member(Carga,I).
movimiento(lcr,lcr(derecha,_,D),Carga)   :- member(Carga,D).
movimiento(lcr,lcr(_,_,_),solo).

moverse(lcr,lcr(B,I,D),Carga,lcr(B1,I1,D1)) :-
   cambios_en_bote(B,B1),
   cambios_en_orillas(Carga,B,I,D,I1,D1).

cambios_en_bote(izquierda,derecha).        
cambios_en_bote(derecha,izquierda).
        
cambios_en_orillas(solo,_,I,D,I,D).
cambios_en_orillas(Carga,izquierda,I,D,I1,D1) :- 
    select(Carga,I,I1), insert(Carga,D,D1).
cambios_en_orillas(Carga,derecha,I,D,I1,D1) :- 
    select(Carga,D,D1), insert(Carga,I,I1).

insert(X,[Y|Ys],[X,Y|Ys]) :-
    precede(X,Y).
insert(X,[Y|Ys],[Y|Zs]) :-
    precede(Y,X), insert(X,Ys,Zs).
insert(X,[],[X]).

precede(lobo,_).
precede(_,repollo).

legal(lcr,lcr(izquierda,_,D)) :- \+ peligrosa(D).
legal(lcr,lcr(derecha,I,_))   :- \+ peligrosa(I).

peligrosa(Orilla) :- member(lobo,Orilla), member(chivo,Orilla).
peligrosa(Orilla) :- member(chivo,Orilla), member(repollo,Orilla).

mostrar(lcr,_,[])     :- !.
mostrar(lcr,E,[M|Ms]) :-
  mostrar_estado(lcr,E),
  moverse(lcr,E,M,N),
  mostrar_movimiento(lcr,E,M), nl,
  mostrar(lcr,N,Ms), !.

mostrar_estado(lcr,lcr(_,I,D)) :-
  write('Orilla Izquierda: '), write(I), nl,
  write('Orilla Derecha:   '), write(D), nl. 

mostrar_movimiento(lcr,lcr(B,_,_),solo) :- 
  write(solo),nl,
  mostrar_bote(B,solo).
mostrar_movimiento(lcr,lcr(B,_,_),Carga) :-
  write(Carga),nl,
  mostrar_bote(B,Carga).

mostrar_bote(Desde,Con) :-
  opuesto(Desde,Hacia),
  write('Desde '), write(Desde), write(' hacia '), write(Hacia),
  write('Carga: '), write(Con), nl.

opuesto(izquierda,derecha).
opuesto(derecha,izquierda).

/*
 * Problema del paseo del caballo en un tablero de ajedrez de
 * dimensiones arbitrarias.
 *
 * Representaremos el Estado del problema como
 *
 *    knight(F,C,MaxF,MaxC)
 *
 * donde F y C indican la fila y columna actual del caballo en el
 * tablero, mientras que MaxF y MaxC indican las dimensiones máximas
 * del tablero.
 *
 * Los movimientos posibles de un caballo están definidos por la lista
 * de cambios de coordenadas (en el sentido de las agujas del reloj)
 *
 * [ [2,1], [1,2], [-1,2], [-2,1], [-2,-1], [-1,-2], [1,-2], [-1,2] ]
 */

inicial(knight,knight(1,1,5,10)).
final(knight,knight(5,7,5,10)).

movimiento(knight,_,Move) :-
  member(Move, [[2,1],[1,2],[-1,2],[-2,1],[-2,-1],[-1,-2],[1,-2],[-1,2]]).
  
moverse(knight,knight(F,C,MaxF,MaxC),[DF,DC],knight(NF,NC,MaxF,MaxC)) :-
  NF is F + DF,
  NC is C + DC.

legal(knight,knight(F,C,MaxF,MaxC)) :-
  F >= 1, F =< MaxF,
  C >= 1, C =< MaxC.