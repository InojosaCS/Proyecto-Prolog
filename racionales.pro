:- op(100, xfx, \\).
:- op(700, xfx, isr).

A \\ B :- integer(A), integer(B).

simplify2(0 \\ _, 0) :- !.
simplify2(X \\ 1, X) :- !.
simplify2(X, X) :- !.

simplify(not_a_number, not_a_number) :- !.
simplify(infinity, infinity) :- !.
simplify(W \\ X, R) :- D is gcd(W, X), Y is W / D, Z is X / D, simplify2(Y \\ Z, R), !.
simplify(X, X) :- !.

addr(not_a_number, _, not_a_number) :- !.
addr(_, not_a_number, not_a_number) :- !.
addr(infinity, _, infinity) :- !.
addr(_, infinity, infinity) :- !.
addr(W \\ Y, X \\ Z, N \\ D) :- N is W * Z  +  Y * X, D is Y * Z, !.
addr(X, Y \\ Z, N \\ D) :- N is X * Z  +  Y, D is Z, !.
addr(X \\ Y, Z, N \\ D) :- N is X  +  Y * Z, D is Y, !.
addr(A, B, R) :- R is A + B, !.

subr(not_a_number, _, not_a_number) :- !.
subr(_, not_a_number, not_a_number) :- !.
subr(infinity, _, infinity) :- !.
subr(_, infinity, infinity) :- !.
subr(A, B \\ C, R) :- B1 is -B, addr(A, B1 \\ C, R), !.
subr(A, B, R) :- B1 is -B, addr(A, B1, R), !.

invr(not_a_number, not_a_number) :- !.
invr(infinity, infinity) :- !.
invr(0, infinity) :- !.
invr(A \\ B, B \\ A) :- !.
invr(X, 1 \\ X) :- !.

mulr(not_a_number, _, not_a_number) :- !.
mulr(_, not_a_number, not_a_number) :- !.
mulr(infinity, _, infinity) :- !.
mulr(_, infinity, infinity) :- !.
mulr(W \\ Y, X \\ Z, N \\ D) :- N is W * X, D is Y * Z, !.
mulr(X, Y \\ Z, N \\ D) :- N is X * Y, D is Z, !.
mulr(X \\ Y, Z, N \\ D) :- N is X * Z, D is Y, !.
mulr(A, B, R) :- R is A * B, !.

divr(not_a_number, _, not_a_number) :- !.
divr(_, not_a_number, not_a_number) :- !.
divr(infinity, _, infinity) :- !.
divr(_, infinity, infinity) :- !.
divr(A, B, R) :- invr(B, B1), mulr(A, B1, R).

process(not_a_number, 0 \\ 0) :- !.
process(infinity, _ \\ 0) :- !.
process(R, A + B) :- process(A1, A), process(B1, B), addr(A1, B1, R), !.
process(R, A - B) :- process(A1, A), process(B1, B), subr(A1, B1, R), !.
process(R, A * B) :- process(A1, A), process(B1, B), mulr(A1, B1, R), !.
process(R, A / B) :- process(A1, A), process(B1, B), divr(A1, B1, R), !.
process(R, ~A) :- process(A1, A), invr(A1, R), !.
process(R, R) :- !.

isr(R, X) :- process(R1, X), simplify(R1, R), !.

% isr(not_a_number, 0 \\ 0) :- !.
% isr(infinity, _ \\ 0) :- !.
% isr(R, A + B) :- isr(A1, A), isr(B1, B), addr(A1, B1, R1), simplify(R1, R), !.
% isr(R, A - B) :- isr(A1, A), isr(B1, B), subr(A1, B1, R1), simplify(R1, R), !.
% isr(R, A * B) :- isr(A1, A), isr(B1, B), mulr(A1, B1, R1), simplify(R1, R), !.
% isr(R, A / B) :- isr(A1, A), isr(B1, B), divr(A1, B1, R1), simplify(R1, R), !.
% isr(R, -A) :- isr(A1, A), invr(A1, R1), simplify(R1, R), !.
% isr(R, A) :- simplify(A, R), !.
