:- discontiguous([inicial/2,final/2,movimiento/3,moverse/4,legal/3,mostrar/3]).

dls(Problema,Estado,Historia,[Movimiento|Movimientos],Prof) :-
  Prof > 0,
  movimiento(Problema,Estado,Movimiento),
  moverse(Problema,Estado,Movimiento,Proximo),
  legal(Problema,Historia,Proximo),
  \+ member(Proximo,Historia),
  P1 is Prof - 1,
  dls(Problema,Proximo,[Proximo|Historia],Movimientos,P1).
dls(Problema,Estado,_,[],0) :- final(Problema,Estado), !.

iddfs(Problema,Estado,Movs) :- iddfs(Problema,Estado,Movs,0).
iddfs(Problema,Estado,Movs,N) :- dls(Problema,Estado,[Estado],Movs,N), !.
iddfs(P,E,M,N) :- N1 is N+1, iddfs(P,E,M,N1).

resolver(Problema,Movimientos) :- 
  inicial(Problema,Estado),
  iddfs(Problema,Estado,Movimientos).

simular(Problema) :- 
  inicial(Problema,Estado),
  iddfs(Problema,Estado,Movimientos),
  mostrar(Problema,Estado,Movimientos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Patio de Operaciones %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% El estado se representa como: vagones(Izq,Arriba,Abajo,Meta) donde:
% Izq: Son los vagones en la base de la Y
% Arriba: Son los vagones en el brazo superior
% Abajo: Son los vagones en el brazo inferior
% Meta: Es la permutación de vagones que queremos lograr
vagones(X,Meta,Operaciones) :-
  iddfs(vagones,vagones(X,[],[],Meta),Operaciones).

% Para representar un estado inicial de vagones siga el siguiente formato:
% vagones([lista actual],[],[],[Lista deseada])
inicial(vagones,vagones([a,b,c],[],[],[b,c,a])).

% Para representar un estado final de vagones siga el siguiente formato:
% vagones([lista deseada],[],[],[Lista deseada])
final(vagones,vagones(Meta,_,_,Meta)).

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
  N > 0, Movimiento = push(Destino,N).
insertar_trenes(Destino,Movimiento,N) :-
  N > 0, N1 is N-1, 
  insertar_trenes(Destino,Movimiento,N1).
quitar_trenes(Destino,Movimiento,N) :-
  N > 0, Movimiento = pop(Destino,N).
quitar_trenes(Destino,Movimiento,N) :-
  N > 0, N1 is N-1,
  quitar_trenes(Destino,Movimiento,N1).

% Toma(N,L,X,R) Toma los N primeros elementos de L y los pone en X, y los restantes en R
toma(0,Rs,[],Rs) :- !.
toma(N,[H|Xs],[H|Ys],Rs) :- N1 is N-1, toma(N1,Xs,Ys,Rs).

moverse(vagones,vagones(Izq,Arriba,Ab,M),push(above,N),vagones(Nizq,Narriba,Ab,M)) :-
  length(Izq, LenIzq), QuedanEnLaIzq is LenIzq - N,
  toma(QuedanEnLaIzq,Izq,Nizq,Ainsertar), append(Ainsertar,Arriba,Narriba).
moverse(vagones,vagones(Izq,Arr,Abajo,M),push(below,N),vagones(Nizq,Arr,Nabajo,M)) :-
  length(Izq, LenIzq),QuedanEnLaIzq is LenIzq - N,
  toma(QuedanEnLaIzq,Izq,Nizq,Ainsertar), append(Ainsertar,Abajo,Nabajo).
moverse(vagones,vagones(Izq,Arriba,Ab,M),pop(above,N),vagones(Nizq,Narriba,Ab,M)) :-
  toma(N,Arriba,Ainsertar,Narriba), append(Izq,Ainsertar,Nizq).
moverse(vagones,vagones(Izq,Arr,Abajo,M),pop(below,N),vagones(Nizq,Arr,Nabajo,M)) :-
  toma(N,Abajo,Ainsertar,Nabajo), append(Izq,Ainsertar,Nizq).

% Cómo se busca la menor cantidad de pasos, no se debe hacer _(above,_) luego 
% de  un _(above,_), de igual forma tampoco un _(below,_) seguido de un _(below,_)
% mas que legal, es una optimizacion, en caso de querer solo legal poner legal(vagones,_,_)
legal(vagones,[_],_).
legal(vagones,[H1,H2|_],H0) :-
  cambio(H1,H2,X),
  cambio(H0,H1,Y),
  X \= Y.

% Detecta dónde hubo un cambio entre dos estados de vagones (arriba o abajo)
cambio(vagones(_,Arriba,Ab,_),vagones(_,Arriba1,Ab,_),arriba) :- Arriba \= Arriba1, !.
cambio(vagones(_,Ar,Abajo,_),vagones(_,Ar,Abajo1,_),abajo) :- Abajo \= Abajo1, !.

mostrar(vagones,vagones(Izq,Arriba,Abajo,Meta),Movimientos) :-
  length(Izq, N0),
  N is N0 * 2 + 7,
  write('Llegada del tren, por la base de la "Y".'), nl,
  mostrar(vagones,vagones(Izq,Arriba,Abajo,Meta),Movimientos,N),
  write('El tren tiene la permutación deseada.'),!.
mostrar(vagones,vagones(Izq,Arriba,Abajo,_),[],Aux) :- 
  imprime_vagones(Izq,Arriba,Abajo,Aux),!.
mostrar(vagones,vagones(Izq,Arriba,Ab,M),[push(above,N)|T],Aux) :-
  length(Izq, LenIzq), QuedanEnLaIzq is LenIzq - N,
  toma(QuedanEnLaIzq,Izq,Nizq,Ainsertar), append(Ainsertar,Arriba,Narriba),
  imprime_vagones(Izq,Arriba,Ab,Aux),
  write('Retrocede al brazo superior, des'),
  enganchar(N,Ainsertar),
  write(',y regresa a la base de la "Y".'), nl,
  mostrar(vagones,vagones(Nizq,Narriba,Ab,M),T,Aux).
mostrar(vagones,vagones(Izq,Arr,Abajo,M),[push(below,N)|T],Aux) :-
  length(Izq, LenIzq), QuedanEnLaIzq is LenIzq - N,
  toma(QuedanEnLaIzq,Izq,Nizq,Ainsertar), append(Ainsertar,Abajo,Nabajo),
  imprime_vagones(Izq,Arr,Abajo,Aux),
  write('Retrocede al brazo inferior, des'),
  enganchar(N,Ainsertar),
  write(',y regresa a la base de la "Y".'), nl,
  mostrar(vagones,vagones(Nizq,Arr,Nabajo,M),T,Aux).
mostrar(vagones,vagones(Izq,Arriba,Ab,M),[pop(above,N)|T],Aux) :-
  toma(N,Arriba,Ainsertar,Narriba), append(Izq,Ainsertar,Nizq),
  imprime_vagones(Izq,Arriba,Ab,Aux),
  write('Retrocede al brazo superior, '),
  enganchar(N,Ainsertar), 
  write(', y regresa a la base de la "Y".'), nl,
  mostrar(vagones,vagones(Nizq,Narriba,Ab,M),T,Aux).
mostrar(vagones,vagones(Izq,Arr,Abajo,M),[pop(below,N)|T],Aux) :-
  toma(N,Abajo,Ainsertar,Nabajo),append(Izq,Ainsertar,Nizq),
  imprime_vagones(Izq,Arr,Abajo,Aux),
  write('Retrocede al brazo inferior, '),
  enganchar(N,Ainsertar), 
  write(', y regresa a la base de la "Y".'), nl,
  mostrar(vagones,vagones(Nizq,Arr,Nabajo,M),T,Aux).

enganchar(1,Tren) :- 
  write('engancha 1 vagón '), write(Tren),!.
enganchar(N,Trenes) :- 
  write('engancha '), write(N), write(' vagones '),
  write(Trenes),!.

imprime_vagones(A,B,C,N) :-
  imprime_brazo(B,N,arriba),
  imprime_base(A,N),
  imprime_brazo(C,N,abajo), nl.
imprime_brazo(L,N,arriba) :-
  put(N+2,' '), write('_ '), write(L), 
  length(L, M), 
  EspacioDeLista is 2*M + 5,
  N1 is N-EspacioDeLista, write(' '), put(N1,'_'), nl.
imprime_brazo(L,N,abajo) :-
  put(N+1,' '), write('\\'), write('_ '), write(L), 
  length(L, M), 
  EspacioDeLista is 2*M + 5,
  N1 is N-EspacioDeLista, write(' '), put(N1,'_'), nl.
imprime_base(L,N) :-
  put(N+1,' '), write('/'), nl,
  write('-- '), write(L), write(' '), 
  length(L, M),
  EspacioDeLista is 2*M + 5,
  N1 is N-EspacioDeLista, put(N1,'-'), write('+'), nl.

put(0,_) :- !.
put(N,T) :- write(T), N1 is N-1, put(N1,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%     Control Remoto    %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Estado: canales(Lista,NroDeCanales). 
% Dónde lista son los canales en los que se encuentra cada TV
% NroDeCanales es el número de canales que tiene el televisor. 
canales(Lista,Canales,Operaciones) :-
  iddfs(canales,canales(Lista,Canales),Operaciones).

% Para representar el estado inicial siga el siguiente formato:
% canales(Lista,NroDeCanales). Dónde lista son los canales iniciales de los TVs 
inicial(canales,canales([3,1,3,3],5)).
final(canales,canales(L,_)) :- lista_igual(_,L).

movimiento(canales,canales(_,_),Movimiento) :- select(Movimiento,[1,2,3,4],_).

moverse(canales,canales(Lista,Canales),Movimiento,canales(NuevaLista,Canales)) :-
  aumentar_Nth_posicion(1,Movimiento,Canales,Lista,NuevaLista).

% aumentar_Nth_posicion(1,L,N,NuevaLista) Unifica NuevaLista con 
% la lista L salvo que el Nth elemento se le saca el mod Canales y se le suma 1.
aumentar_Nth_posicion(N,N,Canales,[H|T],[X|T]) :- X is H mod Canales + 1, !.
aumentar_Nth_posicion(I,N,Canales,[H|T],[H|T1]) :- 
  I1 is I+1, aumentar_Nth_posicion(I1,N,Canales,T,T1).

diff([X|_],[Y|_],I,I) :- X \= Y, !.
diff([_|T],[_|S],I,N) :- I1 is I+1, I1 =< 4, diff(T,S,I1,N).

lista_igual(_,[]).
lista_igual(H,[H|T]) :- lista_igual(H,T).

legal(canales,[_],canales(_,_)). 
legal(canales,[canales(H0,_),canales(H1,_)|_],canales(L,_)) :-
  diff(H0,H1,1,N), diff(H0,L,1,M),
  M \= N.

mostrar(canales,canales([L1|_],_),[]) :- 
  write('Y finalmente todos los televisores quedan sintonizando el canal '),
  write(L1), nl,!.
mostrar(canales,canales([L1,L2,L3,L4],Canales),[N|T]) :-
  write('Los televisores están sintonizando los canales: '), nl,
  write('TV1: '), write(L1), write(' | TV2: '), write(L2), 
  write(' | TV3: '), write(L3), write(' | TV4: '), write(L4), nl,
  write('Y se oprime el botón next del TV'), write(N),
  write(' y los nos queda:'), nl,
  aumentar_Nth_posicion(1,N,Canales,[L1,L2,L3,L4],[N1,N2,N3,N4]),
  write('TV1: '), write(N1), write(' | TV2: '), write(N2),
  write(' | TV3: '), write(N3), write(' | TV4: '), write(N4), nl, nl,
  mostrar(canales,canales([N1,N2,N3,N4],Canales),T).

