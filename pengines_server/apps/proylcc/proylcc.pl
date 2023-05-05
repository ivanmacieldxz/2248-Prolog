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
	generacionPrimerasGrillas(Grid, NumOfColumns, Path, Parcial),
	RGrids = Parcial. %TODO: final
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

/* yep */
boosterIguales(Grilla, Columnas, GrillaBoosterAplicado) :-
    length(Grilla, LargoGrilla),
    UltimoIndice is LargoGrilla - 1,
    listaListasPosicionesRevisar(UltimoIndice, Grilla, Columnas, ListaListasPosiciones),
    listaPosicionesReemplazar(UltimoIndice, Grilla, ListaListasPosiciones, PosicionesReemplazar),
    reemplazoCero(Grilla, PosicionesReemplazar, GrillaBoosterAplicado).

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