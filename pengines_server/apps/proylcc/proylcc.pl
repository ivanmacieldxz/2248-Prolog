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

join(Grid, NumOfColumns, Path, RGrids):-
	Grid = [_N | _Ns],
	generacionPrimerasGrillas(Grid, NumOfColumns, Path, Parcial),
	RGrids = Parcial. %TODO: final
	%TODO: append la tercer grilla correspondiente a la segunda parte de la animación a RGrids

%factorización del código que genera la primera parte del efecto
generacionPrimerasGrillas(Grilla, Columnas, Camino, GrillaResultante) :-
	%obtengo la lista de posiciones del camino y las celdas que representan como una lista
	listaPosiciones(Camino, Columnas, ListaPos),
	sublista(ListaPos, Grilla, ListaValoresCamino),
	reemplazarPorVacio(ListaPos, Grilla,GrillaConCeldasVacias),
	reemplazoCeldaFinCamino(GrillaConCeldasVacias, Columnas, ListaValoresCamino, Camino, GrillaCeldaFinReemplazada),
	GrillaResultante = [GrillaConCeldasVacias, GrillaCeldaFinReemplazada].

reemplazarPorVacio([], Grilla, Grilla).
reemplazarPorVacio([P| RestoListaPos], Grilla, GrillaConCeldasVacias) :-
	reemplazarPos(P, 0, Grilla, GrillaConAlgunosVacios),
	reemplazarPorVacio(RestoListaPos, GrillaConAlgunosVacios, GrillaConCeldasVacias).

reemplazoCeldaFinCamino(Grilla, Columnas,  ListaPosACambiar, Camino, GrillaResultante) :-
	%obtengo el puntaje a partir los valores de la lista de celdas que conforman el camino
	sumatoria(ListaPosACambiar, PuntajeParcial),
	menorP2MayorX(PuntajeParcial, Puntaje),
	%accedo al último elemento del camino (representa la celda de la grilla en la que terminó)
	longitud(Camino, LargoCamino),
	nth1(LargoCamino, Camino, CeldaPuntaje),
	%accedo a la celda de la grilla en la que terminó el recorrido
	posicionEnLista(CeldaPuntaje, Columnas, Pos),
	reemplazarPos(Pos, Puntaje, Grilla, GrillaResultante).


%posición del elemento de la grilla en la lista
posicionEnLista([I, J], NumColumns, Z) :-
    Z is I * NumColumns + J.

%lista de posiciones de una grilla de NumColumns columnas si estuviese representada como lista
listaPosiciones([], _, []).
listaPosiciones([X| Xs], NumColumns, Posiciones) :-
	posicionEnLista(X, NumColumns, Pos),
	listaPosiciones(Xs, NumColumns, SubListaPosiciones),
	insertarInicio(SubListaPosiciones, Pos, Posiciones).