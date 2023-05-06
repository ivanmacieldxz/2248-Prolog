:- module(proylcc, 
	[  
		join/4
	]).

:-use_module(proylcc:prolutils).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

squareScore(Grilla, Columnas, Camino, Square) :-
	%obtengo la lista de posiciones del camino y los valores de esas posiciones en la grilla
	listaPosiciones(Camino, Columnas, ListaPos),
	sublista(ListaPos, Grilla, ListaValoresCamino),
	%obtengo el puntaje a partir los valores de la lista de celdas que conforman el camino
	sumatoria(ListaValoresCamino, PuntajeParcial),
	menorP2MayorX(PuntajeParcial, Square).

join(Grid, NumOfColumns, Path, RGrids):-
	Grid = [_N | _Ns],
	generacionPrimerasGrillas(Grid, NumOfColumns, Path, [Grilla1, Grilla2]),
	generacionSegundasGrillas(Grilla2, NumOfColumns, [Grilla3, Grilla4]),
	RGrids = [Grilla1, Grilla2, Grilla3, Grilla4]. %TODO: final
	%TODO: append la tercer grilla correspondiente a la segunda parte de la animación a RGrids

%factorización del código que genera la primera parte del efecto
generacionPrimerasGrillas(Grilla, Columnas, Camino, GrillaResultante) :-
	%obtengo la lista de posiciones del camino y las celdas que representan como una lista
	listaPosiciones(Camino, Columnas, ListaPos),
	reemplazarPorVacio(ListaPos, Grilla,GrillaConCeldasVacias),
	reemplazoCeldaFinCamino(GrillaConCeldasVacias, Grilla, Columnas, Camino, GrillaCeldaFinReemplazada),
	GrillaResultante = [GrillaConCeldasVacias, GrillaCeldaFinReemplazada].

reemplazarPorVacio([], Grilla, Grilla).
reemplazarPorVacio([P| RestoListaPos], Grilla, GrillaConCeldasVacias) :-
	reemplazarPos(P, 0, Grilla, GrillaConAlgunosVacios),
	reemplazarPorVacio(RestoListaPos, GrillaConAlgunosVacios, GrillaConCeldasVacias).

reemplazoCeldaFinCamino(GrillaConCeldasVacias, GrillaOriginal, Columnas, Camino, GrillaResultante) :-
	%obtengo el puntaje, reutilizando la función actualSquare
	squareScore(GrillaOriginal, Columnas, Camino, Puntaje),
	%accedo al último elemento del camino (representa la celda de la grilla en la que terminó)
	longitud(Camino, LargoCamino),
	nth1(LargoCamino, Camino, CeldaPuntaje),
	%accedo a la celda de la grilla en la que terminó el recorrido
	posicionEnLista(CeldaPuntaje, Columnas, Pos),
	reemplazarPos(Pos, Puntaje, GrillaConCeldasVacias, GrillaResultante).


%posición del elemento de la grilla en la lista
posicionEnLista([I, J], NumColumns, Z) :-
    Z is I * NumColumns + J.

%lista de posiciones de una grilla de NumColumns columnas si estuviese representada como lista
listaPosiciones([], _, []).
listaPosiciones([X| Xs], NumColumns, Posiciones) :-
	posicionEnLista(X, NumColumns, Pos),
	listaPosiciones(Xs, NumColumns, SubListaPosiciones),
	insertarInicio(SubListaPosiciones, Pos, Posiciones).

/* Generación segunda parte del efecto (Aplicar gravedad e insertar nuevas celdas en Grilla))*/

%factorización del código que genera la segunda parte del efecto
generacionSegundasGrillas(Grid, NumOfColumns, GrillaEfectoFinal):-
    listas_columnas(Grid, NumOfColumns, ListaColumnas),
    findall(ListasGravedad,(member(ListSinG,ListaColumnas),efecto_gravedad_columna(ListSinG,ListasGravedad)),ListasColumnasGravedadAp),
    invertir(ListasColumnasGravedadAp, ListaOrdenada),
    obtener_grilla_de_columnas(ListaOrdenada, GrillaEfectoGravedad),
    generacion_nuevas_celdas(GrillaEfectoGravedad, GrillaCeldasNuevas),
    GrillaEfectoFinal = [GrillaEfectoGravedad,GrillaCeldasNuevas].

%listas_columnas(+Grid, +NumOfColumns, -ListaColumn)
%obtiene una lista ListaColumn que contiene las listas de los elementos de cada columna
listas_columnas(Grid, NumOfColumns, ListaColumn):-
    grillaPosiciones(Grid,0,0,NumOfColumns,GridPos),
    lista_numeros(NumOfColumns,Columnas),
    findall(List,(member(C,Columnas),lista_columna(C,NumOfColumns,Grid,GridPos,List)),ListaColumn).

%lista_columna(+Num, +NumOfColumns, +Grid, +GridPos, -Lista)
%obtiene una lista Lista que contiene los elementos de la columna Num
lista_columna(Num, NumOfColumns, Grid, GridPos, Lista):-
    findall(Elem,(member([X,Num],GridPos), elemento_de_Grid(Grid, NumOfColumns, X, Num, Elem)),Lista).

%efecto_gravedad_columna(+List, -ListEfectoGravedad)
%obtiene una lista ListEfectoGravedad que se le aplico el efecto de gravedad
efecto_gravedad_columna(List, ListEfectoGravedad):-
    lista_ceros(List,ListCeros),
    lista_no_ceros(List,ListNoCeros),
    concatenar(ListCeros,ListNoCeros, ListEfectoGravedad).

%obtener_grilla_de_columnas(+ListaColumn, -Grid)
%obtiene una grilla Grid conformada por las columnas de ListaColumn
obtener_grilla_de_columnas(ListaColumn, Grid):-
    member(L,ListaColumn), !,
    length(L, N),
    listas_filas(ListaColumn, N, L2),
    invertir(L2, ListaFilas),
    concatenar_listas_de_listas(ListaFilas, Grid).

%listas_filas(+ListaColumn, +N, -ListasF)
%obtiene la lista ListasF que contiene listas de las filas a partir de ListaColumn
listas_filas(_, 0, []).
listas_filas(L1, N, [H|T]) :-
    lista_fila(L1, N, H),
    N1 is N - 1,
    listas_filas(L1, N1, T), !.

%lista_fila(+ListaColumn, +N, -ListaF)
%obtiene la lista ListaF que contiene los elementos de una fila a partir de ListaColumn
lista_fila([], _, []).
lista_fila([H|T], N, []) :-
    length(H, M),
    M < N,
    lista_fila(T, N, []).
lista_fila([H|T], N, [E|L]) :-
    nth1(N, H, E),
    lista_fila(T, N, L).

%grillaPosiciones(+Grid, +F, +C, +NumOfColumns, -GridPos)
%obtiene la grilla GridPos que contiene todas las pos del estilo [X,Y] que Grid tiene
grillaPosiciones([],_F, _C, _NumOfColumns, []).
grillaPosiciones([_G|GS], F, C, NumOfColumns, [[F,C]|XS]):-
   C < NumOfColumns,
   NC is C + 1,
   grillaPosiciones(GS, F, NC, NumOfColumns, XS), !.
grillaPosiciones([_G|GS], F, C, NumColumns, [[F,C]|XS]):-
   C is NumColumns - 1,
   NF is F + 1,
   grillaPosiciones(GS, NF, 0, NumColumns, XS).

%elemento_de_Grid(+GridList, +NumColumns, +X, +Y, -Element) 
%obtiene el elemento que se encuentra la pos [X,Y] en la grilla GridList
elemento_de_Grid(GridList, NumColumns, X, Y, Element) :-
    nth0(Index, GridList, Element),
    X is div(Index, NumColumns),
    Y is mod(Index, NumColumns).

/*---------------------nuevos celdas aleatorias---------------------------------*/

%generacion_nuevas_celdas(+Grilla, -GrillaCompleta)
%obtiene una grilla GrillaCompleta a partir de Grilla, elementos 0 en Grilla tienen otros valores en GrillaCompleta
generacion_nuevas_celdas([],[]).
generacion_nuevas_celdas([X|XS], [X|XSS]):-
    X \= 0,
    generacion_nuevas_celdas(XS,XSS).
generacion_nuevas_celdas([0|XS], [X|XSS]):-
    generar_valor_random(X),
    generacion_nuevas_celdas(XS,XSS).

%generar_valor_random(-X)
%obtiene un número random entre 2, 4, 28, 16, 32 y 64
generar_valor_random(X):-
    random_member(X,[2,4,8,16,32,64]).

/*------------------------------------ yep -------------------------------------*/
boosterIguales(Grilla, Columnas, GrillaBoosterAplicado) :-
    length(Grilla, LargoGrilla),
    UltimoIndice is LargoGrilla - 1,
    listaListasPosicionesRevisar(UltimoIndice, Grilla, Columnas, ListaListasPosiciones),
    listaPosicionesReemplazar(UltimoIndice, Grilla, ListaListasPosiciones, PosicionesReemplazar),
    reemplazoCero(Grilla, PosicionesReemplazar, GrillaReemplazada),
	GrillaBoosterAplicado = [GrillaReemplazada].

listaListasPosicionesRevisar(0, Grilla, Columnas, Lista) :-
    listaPosicionesRevisar(0, Grilla, Columnas, ListaPos0),
    Lista = [ListaPos0].
listaListasPosicionesRevisar(PosActual, Grilla, Columnas, Lista) :-
    listaPosicionesRevisar(PosActual, Grilla, Columnas, ListaPosActual),
    PosSig is PosActual - 1,
    listaListasPosicionesRevisar(PosSig, Grilla, Columnas, ListaParcial),
    insertarFinal(ListaParcial, ListaPosActual, Lista).

listaPosicionesRevisar(Pos, Grilla, Columnas, Lista) :-
    length(Grilla, Long),
    Pos >= 0,
    Pos < Long,
    PosD is Pos + 1,
    PosA is Pos + Columnas,
    PosDA is PosA + 1,
    PosIA is PosA - 1,
    (
    	esBordeInfDer(Pos, Grilla, Columnas) ->
    	Lista = [];
        esBordeInf(Pos, Grilla, Columnas) ->
        Lista = [PosD];
    	esBordeDer(Pos, Grilla, Columnas) ->  
        Lista = [PosIA, PosA];
    	esBordeIzq(Pos, Grilla, Columnas) ->
        Lista = [PosD, PosA, PosDA];
    	Lista = [PosD, PosIA, PosA, PosDA]
    ).

listaPosicionesReemplazar(-1, _, _, []).
listaPosicionesReemplazar(Inicio, Grilla, ListaListasPosicionesRevisar, ListaFinal) :-
    Inicio >= 0,
    Sig is Inicio - 1,
    listaPosicionesReemplazar(Sig, Grilla, ListaListasPosicionesRevisar, ListaSinInicio),
    nth0(Inicio, ListaListasPosicionesRevisar, ListaPosicionesRevisarInicio),
    listaReemplazar(Inicio, Grilla, ListaPosicionesRevisarInicio, ListaDeInicio),
    append(ListaSinInicio, ListaDeInicio, ListaFinal).

%si la lista de posiciones a revisar está vacía, la lista resultante de posiciones a reemplazar es vacía
listaReemplazar(_Pos, _Grilla, [], []).
listaReemplazar(Pos, Grilla, [P|RestoPosicionesRevisar], ListaReemplazar) :-
    listaReemplazar(Pos, Grilla, RestoPosicionesRevisar, ListaParcial),
    (   
    	nth0(Pos, Grilla, ElementoEnPos),
        nth0(P, Grilla, ElementoEnP),
        ElementoEnPos is ElementoEnP ->  
    	insertarInicio(ListaParcial, P, ListaParcialSinPos),
        insertarInicio(ListaParcialSinPos, Pos, ListaReemplazar);
    	ListaReemplazar = ListaParcial
    ).

reemplazoCero(Grilla, [], Grilla).
reemplazoCero(Grilla, [P|RestoPosiciones], GrillaResultante):-
    reemplazoCero(Grilla, RestoPosiciones, GrillaSemiReemplazada),
    reemplazarPos(P, 0, GrillaSemiReemplazada, GrillaResultante).