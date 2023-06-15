/*
 * Contiene predicados que funcionan como utilidades varias para los distintos archivos prolog del proyecto.
 */

:- module(prolutils, 
    [
        longitud/2,
        reemplazar_pos/4,
        sumatoria/2,
        log_base_2/2,
        potencia/3,
        menor_p2_mayor_X/2,
        insertar_inicio/3,
        sublista/3,
        es_borde_der/3,
        es_borde_inf/3,
        es_borde_inf_der/3,
        es_borde_izq/3,
        es_borde_sup/3,
        es_borde_inf_izq/3,
        es_borde_sup_der/3,
        concatenar_listas_de_listas/2,
        concatenar/3,
        invertir/2,
        lista_no_ceros/2,
        lista_ceros/2,
        lista_numeros/2,
        esta/2
        
    ]).

%largo de una lista
longitud([], 0).
longitud([_|T], X) :-
    longitud(T, Y),
    X is 1 + Y.


reemplazar_pos(0, X, [_|T], [X|T]) :- !.
reemplazar_pos(N, X, [H|T], [H|T2]) :-
    N > 0,
    N1 is N - 1,
    reemplazar_pos(N1, X, T, T2).

sumatoria([X], X).
sumatoria([X|Xs], S) :-
    sumatoria(Xs, Y),
    S is X + Y.

log_base_2(1,0). 
log_base_2(N,R) :-
    N>1,
    N1 is N//2,
    log_base_2(N1,R1),
    R is R1 +1.

potencia(X,0,1) :- X =\= 0.
potencia(X,Y,Z) :- 
    Y \= 0,
    A is Y -1, 
    potencia(X,A,B), 
    Z is B*X.

menor_p2_mayor_X(X, Z) :-
    log_base_2(X, Y),
    (potencia(2, Y, Z), X is Z;
    potencia(2, Y+1, Z)).

insertar_inicio([], A, [A]).
insertar_inicio([X | Xs], A, [A | Zs]):-
    insertar_inicio(Xs, X, Zs).

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
    insertar_inicio(SubSinP, Elem, Sublista).

es_borde_inf_izq(Pos, Grilla, Columnas) :-
    length(Grilla, Long),
    Pos is Long - Columnas.

es_borde_inf_der(Pos, Grilla, _Columnas) :- 
    length(Grilla, Long),
    Pos is Long - 1.

es_borde_inf(Pos, Grilla, Columnas) :-
    length(Grilla, Long),
    Pos >= Long - Columnas,
    Pos =< Long - 1.

es_borde_der(Pos, _Grilla, Columnas) :-
    (Pos mod Columnas) =:= Columnas - 1.

es_borde_izq(Pos, _Grilla, Columnas) :-
    (Pos mod Columnas) =:= 0.

es_borde_sup(Pos, _Grilla, Columnas) :-
    Pos < Columnas.

es_borde_sup_der(Pos, _Grilla, Columnas) :-
    Pos is Columnas - 1.

%lista_ceros(+L,-ListCeros)
%obtiene la lista ListCeros que contiene todos los ceros de L
lista_ceros([],[]).
lista_ceros([X|XS],XSS):-
    X \= 0,
    lista_ceros(XS,XSS).
lista_ceros([0|XS],[0|XSS]):-
    lista_ceros(XS,XSS).

%lista_no_ceros(+L,-ListNoCeros)
%obtiene la lista ListListNoCerosCeros que contiene todos los elementos de L distintos de cero
lista_no_ceros([],[]).
lista_no_ceros([0|XS],XSS):-
    lista_no_ceros(XS,XSS), !.
lista_no_ceros([X|XS],[X|XSS]):-
    X \= 0,
    lista_no_ceros(XS,XSS).

%obtiene lista equivalente a concatenar otras dos
concatenar([],XS,XS).
concatenar([X|XS],Y,[X|XSS]):-
    concatenar(XS,Y,XSS).

%obtiene lista invertida
invertir([],[]).
invertir([X|XS], L):-
    invertir(XS,L1),
    concatenar(L1,[X],L).

%lista_numeros(+N,-Result)
%obtiene una lista Result que contiene los elementos del 0 al N-1 descendente 
lista_numeros(0, []).
lista_numeros(N, [X|Xs]) :-
    X is N-1,
    N1 is N-1,
    lista_numeros(N1, Xs), !.

%concatenar_listas_de_listas(+List,-Result)
%obtiene una lista Result que contiene los elementos de las listas de List 
concatenar_listas_de_listas([], []).
concatenar_listas_de_listas([X|XS], Grid):-
    concatenar_listas_de_listas(XS, Grid1),
    concatenar(X, Grid1, Grid).

esta(X, [X|_T]).
esta(X, [_|T]) :-
    esta(X, T).