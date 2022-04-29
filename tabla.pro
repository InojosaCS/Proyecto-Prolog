:- op(2, fy, :~:).
:- op(3, yfx, :\/:).
:- op(4, yfx, :/\:).
:- op(5, xfx, isb).

:- dynamic t/1.
:- dynamic f/1.

isb(t, P :\/: _) :- isb(t, P), !.
isb(t, _ :\/: Q) :- isb(t, Q), !.
isb(f, _ :\/: _) :- !.
isb(t, P :/\: Q) :- isb(t, P), isb(t, Q), !.
isb(f, _ :/\: _) :- !.
isb(t, :~: P) :- isb(f, P), !.
isb(f, :~: P) :- isb(t, P), !.
isb(t, P) :- t(P), !.
isb(f, P) :- f(P), !.

getvars(F, [F]) :- atomic(F), !.
getvars(F, V)   :- F =.. [_, F1], getvars(F1, V), !.
getvars(F, V)   :- F =.. [_, F1, F2], getvars(F1, V1), getvars(F2, V2), union(V1, V2, V), !.

getvalues(0, []) :- !.
getvalues(N, [V|Vs]) :- member(V, [t, f]), N1 is N-1, getvalues(N1, Vs).

assoc([], []) :- !.
assoc([Val|Vals], [Var|Vars]) :- C =.. [Val, Var], asserta(C), assoc(Vals, Vars), !.

rem([]) :- !.
rem([V|Vs]) :- C =.. [V, _], retractall(C), rem(Vs), !.

solve(F, Vars, Vals, R) :- getvars(F, Vars), length(Vars, N), getvalues(N, Vals), assoc(Vals, Vars), R isb F, rem(Vals).

truthtable :-
    repeat,
    write('formula: '),
    flush_output,
    read(F),
    truthtable(F).

truthtable(bye).
truthtable(F) :-
    getvars(F, Vars),
    print_head(Vars),
    print_body(F),
    fail.

print_head(Vars) :-
    append([Vars, ['F']], Cells),
    length(Cells, N),
    print_horizontal_line(N), nl,
    print_cells(Cells), nl,
    print_horizontal_line(N), nl.

print_body(F) :-
    solve(F, _, Vals, R),
    append([Vals, [R]], Cells),
    length(Cells, N),
    print_cells(Cells), nl,
    print_horizontal_line(N), nl,
    fail.

print_cells([]) :- write('|'), !.
print_cells([C|Cs]) :- write('|'), write(' '), write(C), write(' '), print_cells(Cs), !.

print_horizontal_line(0) :- write('+'), !.
print_horizontal_line(N) :-
    write('+---'),
    N1 is N-1,
    print_horizontal_line(N1), !.
