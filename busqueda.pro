:- discontiguous([inicial/2,final/2,movimiento/3,moverse/4,legal/3]).

dls(Problema,(Estado,P,Cota),Historia,[Movimiento|Movimientos]) :-
  P < Cota,
  movimiento(Problema,Estado,Movimiento),
  moverse(Problema,Estado,Movimiento,Proximo),
  legal(Problema,Historia,Proximo),
  \+ member(Proximo,Historia),
  P1 is P + 1,
  dls(Problema,(Proximo,P1,Cota),[Proximo|Historia],Movimientos).
dls(Problema,(Estado,Profundidad,Profundidad),_,[]) :- final(Problema,Estado), !.

iddfs(Problema,Estado,Movimientos) :- 
  iddfs(Problema,Estado,Movimientos,0).
iddfs(Problema,Estado,Movimientos,N) :-
  dls(Problema,(Estado,0,N),[Estado],Movimientos), !.
iddfs(P,E,M,N) :-
  N1 is N+1, iddfs(P,E,M,N1).

resolver(Problema,Movimientos) :- 
  inicial(Problema,Estado),
  iddfs(Problema,Estado,Movimientos).

simular(Problema) :- 
  inicial(Problema,Estado),
  iddfs(Problema,Estado,Movimientos),
  mostrar(Problema,Estado,Movimientos).

vagones(X,Meta,Operaciones) :-
  Estado = vagones(X,[],[],Meta),
  iddfs(vagones,Estado,Operaciones).

% Push above
movimiento(vagones,vagones(Izquierda,_,_,_),Movimiento) :-
  length(Izquierda, N),
  insertar_trenes(above,Movimiento,N).
% Push below
movimiento(vagones,vagones(Izquierda,_,_,_),Movimiento) :-
  length(Izquierda, N),
  insertar_trenes(below,Movimiento,N).
% Pop above
movimiento(vagones,vagones(_,Arriba,_,_),Movimiento) :-
  length(Arriba, N),
  quitar_trenes(above,Movimiento,N).
% Pop below
movimiento(vagones,vagones(_,_,Abajo,_),Movimiento) :-
  length(Abajo, N),
  quitar_trenes(below,Movimiento,N).
  
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

moverse(vagones,vagones(Izq,Arriba,Ab,M),push(above,N),vagones(Nizq,Narriba,Ab,M)) :-
  length(Izq, LenIzq),
  QuedanEnLaIzq is LenIzq - N,
  toma(QuedanEnLaIzq,Izq,Nizq,Ainsertar),
  append(Ainsertar,Arriba,Narriba).
moverse(vagones,vagones(Izq,Arr,Abajo,M),push(below,N),vagones(Nizq,Arr,Nabajo,M)) :-
  length(Izq, LenIzq),
  QuedanEnLaIzq is LenIzq - N,
  toma(QuedanEnLaIzq,Izq,Nizq,Ainsertar),
  append(Ainsertar,Abajo,Nabajo).
moverse(vagones,vagones(Izq,Arriba,Ab,M),pop(above,N),vagones(Nizq,Narriba,Ab,M)) :-
  toma(N,Arriba,Ainsertar,Narriba),
  append(Izq,Ainsertar,Nizq).
moverse(vagones,vagones(Izq,Arr,Abajo,M),pop(below,N),vagones(Nizq,Arr,Nabajo,M)) :-
  toma(N,Abajo,Ainsertar,Nabajo),
  append(Izq,Ainsertar,Nizq).

legal(vagones,_,_).

% Toma(N,L,X,R) Toma los N primeros elementos de L y los pone en X, y los restantes en R
toma(0,Rs,[],Rs) :- !.
toma(N,[H|Xs],[H|Ys],Rs) :-
  N1 is N-1,
  toma(N1,Xs,Ys,Rs).

final(vagones,vagones(Meta,_,_,Meta)).

%%%%%%%%%%%%%%%%%%%%%%%%%% Control Remoto %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% Control Remoto %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% Control Remoto %%%%%%%%%%%%%%%%%%%%%%
canales(Lista,Canales,Operaciones) :-
  Estado = canales(Lista,Canales),
  iddfs(canales,Estado,Operaciones).

movimiento(canales,canales(_,_),Movimiento) :-
  generar_numero_tv(1,Movimiento).

generar_numero_tv(Movimiento,Movimiento) :- 
  Movimiento =< 4.
generar_numero_tv(N,Movimiento) :-
  N1 is N+1,
  N1 =< 4,
  generar_numero_tv(N1,Movimiento).

moverse(canales,canales(Lista,Canales),Movimiento,canales(NuevaLista,Canales)) :-
  aumentar_Nth_posicion(1,Movimiento,Canales,Lista,NuevaLista).

% aumentar_Nth_posicion(1,L,N,NuevaLista) Unifica NuevaLista con 
% la lista L salvo que el Nth elemento se le suma 1 y se le saca el mod Canales .
aumentar_Nth_posicion(N,N,Canales,[H|T],[X|T]) :- X is H mod Canales + 1, !.
aumentar_Nth_posicion(I,N,Canales,[H|T],[H|T1]) :- 
  I1 is I+1, aumentar_Nth_posicion(I1,N,Canales,T,T1).

legal(canales,[_],canales(_,_)). 
legal(canales,[canales(H0,_),canales(H1,_)|_],canales(L,_)) :-
  diff(H0,H1,1,N),
  diff(H0,L,1,M),
  M \= N.

diff([X|_],[Y|_],I,I) :- X \= Y, !.
diff([_|T],[_|S],I,N) :- I1 is I+1, I1 =< 4, diff(T,S,I1,N).

final(canales,canales(L,_)) :-
  lista_igual(_,L).

lista_igual(_,[]).
lista_igual(H,[H|T]) :- lista_igual(H,T).
