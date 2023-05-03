/*
 * Contiene predicados que funcionan como utilidades varias para los distintos archivos prolog del proyecto.
 */

:- module(prolutils, 
    [
        longitud/2,
        reemplazarPos/4,
        sumatoria/2,
        logBase2/2,
        potencia/3,
        menorP2MayorX/2,
        insertarInicio/3,
        sublista/3
    ]).

%largo de una lista
longitud([], 0).
longitud([_|T], X) :-
    longitud(T, Y),
    X is 1 + Y.


reemplazarPos(0, X, [_|T], [X|T]) :- !.
reemplazarPos(N, X, [H|T], [H|T2]) :-
    N > 0,
    N1 is N - 1,
    reemplazarPos(N1, X, T, T2).

sumatoria([X], X).
sumatoria([X|Xs], S) :-
    sumatoria(Xs, Y),
    S is X + Y.

logBase2(1,0). 
logBase2(N,R) :-
    N>1,
    N1 is N//2,
    logBase2(N1,R1),
    R is R1 +1.

potencia(X,0,1) :- X =\= 0.
potencia(X,Y,Z) :- 
    Y \= 0,
    A is Y -1, 
    potencia(X,A,B), 
    Z is B*X.

menorP2MayorX(X, Z) :-
    logBase2(X, Y),
    (potencia(2, Y, Z), X is Z;
    potencia(2, Y+1, Z)).

insertarInicio([], A, [A]).
insertarInicio([X | Xs], A, [A | Zs]):-
    insertarInicio(Xs, X, Zs).

/*
 * Toma como primer parámetro una 
 * lista de índices de con las 
 * posiciones de los valores de la 
 * lista original que conformarán la 
 * nueva lista.
 */
sublista([], [_| _], []).
sublista([P|Ps], [X|Xs], Sublista) :-
    nth0(P, [X|Xs], Elem),
    sublista(Ps, [X|Xs], SubSinP),
    insertarInicio(SubSinP, Elem, Sublista).
