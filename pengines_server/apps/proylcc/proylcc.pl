:- module(proylcc, 
	[  
		join/4
	]).

:-use_module(proylcc:prolutils).

/**
 * preview(+Grilla, +Columnas, +Camino, -Prev)
 * -Prev es el valor del cuadrado que se colocaría en la grilla Grilla al final del camino Camino si se 
 * terminara el camino en ese momento. Columnas representa la cantidad de columnas de la grilla.
 */
preview(Grilla, Columnas, Camino, Prev) :-
	lista_posiciones(Camino, Columnas, ListaPos),
	square_score(Grilla, ListaPos, Prev).

/**
 * square_score(+Grilla, +Camino, -Square)
 * Square es el valor de la menor potencia de 2 que supera o iguala la sumatoria de los valores de las 
 * posiciones del camino Camino en la grilla Grilla.
 * Camino es una lista de elementos donde cada elemento es una posición de la grilla en formato de lista, 
 * en lugar de en formato de matriz, es decir, es de la forma [x, y, z, |Ps] en lugar de 
 * [[x, y], [x1, y2] | Ps]
 */
square_score(Grilla, Camino, Square) :-
	%obtengo los valores de las posiciones del camino en la grilla
	sublista(Camino, Grilla, ListaValoresCamino),
	%obtengo el puntaje a partir los valores de la lista que conforman el camino
	sumatoria(ListaValoresCamino, PuntajeParcial),
	menor_p2_mayor_X(PuntajeParcial, Square).

/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 
join(Grid, NumOfColumns, Path, RGrids):-
	% genero las primeras dos grillas del efecto
	generacion_primeras_grillas(Grid, NumOfColumns, Path, [Grilla1, Grilla2]),
	% genero las últimas dos grillas del efecto: donde la primera representa el efecto de gravedad aplicado
	% y la segunda rellena las posiciones vacías con valores
	generacion_segundas_grillas(Grilla2, NumOfColumns, [Grilla3, Grilla4]),
	RGrids = [Grilla1, Grilla2, Grilla3, Grilla4].


/**
 * generacion_primeras_grillas(+Grilla, +Columnas, +Camino, -GrillasResultantes)
 * GrillasResultantes es una lista conformada por dos listas, que representan dos grillas donde la primera 
 * se corresponde con la grilla original Grilla, pero con las celdas del camino Camino eliminadas y la segunda 
 * se corresponde con la primer grilla pero con la celda final del camino Camino reemplazada por el 
 * valor correspondiente. Camino es una lista de posiciciones con formato [x, y] (formato matricial, no de lista)
 */
generacion_primeras_grillas(Grilla, Columnas, Camino, GrillasResultantes) :-
	% obtengo la lista de posiciones del camino
	lista_posiciones(Camino, Columnas, ListaPos),
	% genero las dos grillas correspondientes del efecto, a partir de la lista de posiciones
	grillas_camino_reemplazado(ListaPos, Grilla, GrillasResultantes).

/**
 * reemplazar_por_vacio(+ListaPosiciones, +Grilla, -GrillaConCeldasVacias)
 * GrillaConCeldas vacías es una lista que representa una grilla igual que la grilla original Grilla, pero 
 * en la que las posiciones de la lista ListaPosiciones fueron eliminadas.
 */
reemplazar_por_vacio([], Grilla, Grilla).
reemplazar_por_vacio(ListaPosiciones, Grilla, GrillaConCeldasVacias) :-
	ListaPosiciones = [P| RestoListaPos],
	% reemplazo la posición actual P por 0, equivalente a eliminar la celda
	reemplazar_pos(P, 0, Grilla, GrillaConAlgunosVacios),
	% hago lo mismo para el resto de posiciones en la lista, a partir de la grilla con la celda P eliminada
	reemplazar_por_vacio(RestoListaPos, GrillaConAlgunosVacios, GrillaConCeldasVacias).

/**
 * reemplazo_celda_fin_camino(+GrillaConCeldasVacias, +GrillaOriginal, +Camino, -GrillaResultante)
 * GrillaResultante es la grilla GrillaConCeldasVacias, pero con la última posición de la lista de posiciones 
 * Camino reemplazada por la menor potencia de dos mayor o igual a la sumatoria de los valores contenidos en 
 * las celdas del camino en la grilla original (sin posiciones eliminadas) GrillaOriginal.
 */
reemplazo_celda_fin_camino(GrillaConCeldasVacias, GrillaOriginal, Camino, GrillaResultante) :-
	%obtengo el puntaje, reutilizando la función actualSquare
	square_score(GrillaOriginal, Camino, Puntaje),
	%accedo al último elemento del camino (representa la celda de la grilla en la que terminó)
	longitud(Camino, LargoCamino),
	nth1(LargoCamino, Camino, CeldaPuntaje),
	% reemplazo esa posición por el valor correspondiente al puntaje
	reemplazar_pos(CeldaPuntaje, Puntaje, GrillaConCeldasVacias, GrillaResultante).


/**
 * posicion_en_lista([I, J], NumColumns, Z)
 * [I, J] es una lista correspondiente a la posición de un elemento en una matriz de NumColumns
 * Z representa la posicion de ese par si perteneciera a una lista (posición en formato de lista en lugar de formato matricial)
 */
posicion_en_lista([I, J], NumColumns, Z) :-
    Z is I * NumColumns + J.

%lista de posiciones de una grilla de NumColumns columnas si estuviese representada como lista
/**
 * lista_posiciones(+ListaPosicionesMatricial, +NumColumns, -Posiciones)
 * Posiciones es la lista de posiciones en formato de lista que se corresponde con la lista de posiciones en formato 
 * matricial ListaPosicionesMatricial, en una grilla de NumColumns columnas.
 */
lista_posiciones([], _, []).
lista_posiciones(ListaPosicionesMatricial, NumColumns, Posiciones) :-
	ListaPosicionesMatricial = [X| Xs],
	posicion_en_lista(X, NumColumns, Pos),
	lista_posiciones(Xs, NumColumns, SubListaPosiciones),
	insertar_inicio(SubListaPosiciones, Pos, Posiciones).

/* Generación segunda parte del efecto (Aplicar gravedad e insertar nuevas celdas en Grilla))*/

%factorización del código que genera la segunda parte del efecto
generacion_segundas_grillas(Grid, NumOfColumns, GrillaEfectoFinal):-
    listas_columnas(Grid, NumOfColumns, ListaColumnas),
    findall(ListasGravedad,(member(ListSinG,ListaColumnas),efecto_gravedad_columna(ListSinG,ListasGravedad)),ListasColumnasGravedadAp),
    invertir(ListasColumnasGravedadAp, ListaOrdenada),
    obtener_grilla_de_columnas(ListaOrdenada, GrillaEfectoGravedad),
    generacion_nuevas_celdas(GrillaEfectoGravedad, GrillaCeldasNuevas),
    GrillaEfectoFinal = [GrillaEfectoGravedad,GrillaCeldasNuevas].

%listas_columnas(+Grid, +NumOfColumns, -ListaColumn)
%obtiene una lista ListaColumn que contiene las listas de los elementos de cada columna
listas_columnas(Grid, NumOfColumns, ListaColumn):-
    grilla_posiciones(Grid,0,0,NumOfColumns,GridPos),
    lista_numeros(NumOfColumns,Columnas),
    findall(List,(member(C,Columnas),lista_columna(C,NumOfColumns,Grid,GridPos,List)),ListaColumn).

%lista_columna(+Num, +NumOfColumns, +Grid, +GridPos, -Lista)
%obtiene una lista Lista que contiene los elementos de la columna Num
lista_columna(Num, NumOfColumns, Grid, GridPos, Lista):-
    findall(Elem,(member([X,Num],GridPos), elemento_de_grid(Grid, NumOfColumns, X, Num, Elem)),Lista).

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

%grilla_posiciones(+Grid, +F, +C, +NumOfColumns, -GridPos)
%obtiene la grilla GridPos que contiene todas las pos del estilo [X,Y] que Grid tiene
grilla_posiciones([],_F, _C, _NumOfColumns, []).
grilla_posiciones([_G|GS], F, C, NumOfColumns, [[F,C]|XS]):-
   C < NumOfColumns,
   NC is C + 1,
   grilla_posiciones(GS, F, NC, NumOfColumns, XS), !.
grilla_posiciones([_G|GS], F, C, NumColumns, [[F,C]|XS]):-
   C is NumColumns - 1,
   NF is F + 1,
   grilla_posiciones(GS, NF, 0, NumColumns, XS).

%elemento_de_grid(+GridList, +NumColumns, +X, +Y, -Element) 
%obtiene el elemento que se encuentra la pos [X,Y] en la grilla GridList
elemento_de_grid(GridList, NumColumns, X, Y, Element) :-
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

/*------------------------------------ Efecto booster -------------------------------------*/
/**
 * booster_iguales(+Grilla, +Columnas, -Grillas)
 * Aplica a la grilla Grilla con Columnas número de columnas el efecto Booster.
 * Grillas es una lista de Grillas que en conjunto conforman el efecto del Booster.
 * Por cada camino eliminado se añaden dos grillas a grillas, la primera representando la eliminación 
 * del camino y la segunda representa el camino eliminado pero con la celda de más abajo a la derecha 
 * reemplazada por el número correspondiente a la menor potencia de dos mayor o igual a la sumatoria 
 * de los valores del camino.
 */
booster_iguales(Grilla, Columnas, Grillas) :-
    %todos los caminos que el jugador podría formar
    listado_caminos(Grilla, Columnas, PreListadoBooster),
    %elimino los caminos redundantes
    eliminar_subcaminos(PreListadoBooster, PreListadoBooster, ListadoBooster),
    %obtengo las grillas de la animación del efecto
    grillas_efecto_booster(Grilla, Columnas, ListadoBooster, GrillasBoosterAplicado),
	%obtengo la grilla final del efecto del booster
	length(GrillasBoosterAplicado, LBoostAp),
	nth1(LBoostAp, GrillasBoosterAplicado, UltimaGrillaBooster),
	%y la uso como base para aplicarle el efecto de gravedad
	generacion_segundas_grillas(UltimaGrillaBooster, Columnas, GrillaEfectoFinal),
	%finalmente, agrego las grillas del efecto de gravedad
	append(GrillasBoosterAplicado, GrillaEfectoFinal, Grillas).

/**
 * listado_caminos(+Grilla, +Columnas, -Listado)
 * Listado es el listado de caminos conformables en la grilla, partiendo por la casilla inferior 
 * derecha de la grilla Grilla, de Columnas número de columnas. Cada camino es una lista de posiciones.
 */
listado_caminos(Grilla, Columnas, Listado) :-
	%obtengo la longitud de la grilla
    length(Grilla, LGrilla),
    PosInicial is LGrilla - 1,
	%empiezo a conformar los caminos desde la última posición
    listado_caminos_rec(PosInicial, Grilla, Columnas, Listado).

/**
 * listado_caminos_rec(+Pos, +Grilla, +Columnas, -Listado)
 * Listado es una lista que contiene todos los posibles caminos a formar en la grilla Grilla, de Columnas número de 
 * columnas, iniciando por la posición Pos y recorriendo todas las posiciones hasta llegar a la primera.
 * Listado contiene caminos que son subcaminos de otros, pero no caminos conformados por una única posición
 */
listado_caminos_rec(0, _, _, []).
listado_caminos_rec(Pos, Grilla, Columnas, Listado) :-
    %formo el camino para la posición actual
    camino(Pos, Grilla, Columnas, CaminoPos),
    %paso al siguiente camino
    PosSig is Pos - 1,
    listado_caminos_rec(PosSig, Grilla, Columnas, ListadoSinPos),
    (   
        %si no es un camino unitario, lo añado a la lista
    	length(CaminoPos, 1) ->  
    	append(ListadoSinPos, [], Listado);
    	append(ListadoSinPos, [CaminoPos], Listado)
    ).

/**
 * eliminar_subcaminos(+ListaCaminos, +ListaOriginal, -SinSubcaminos)
 * SinSubcaminos es la lista de caminos posibles, producto de eliminar los caminos que son subcaminos de otros de 
 * ListaCaminos. ListaOriginal es la lista original, con todos los subcaminos posibles, que se utiliza de referencia.
 */
% el parámetro ListaOriginal es importante, porque a pesar de que no se utiliza en la recursión, es utilizado en el caso base
eliminar_subcaminos([], ListaOriginal, ListaOriginal).
eliminar_subcaminos(ListaCaminos, ListaOriginal, SinSubcaminos) :-
    ListaCaminos = [Camino|RestoCaminos],
    eliminar_subcaminos(RestoCaminos, ListaOriginal, CasiSinSubcaminos),
    eliminar_subcamino(Camino, CasiSinSubcaminos, RestoCaminos, SinSubcaminos).

/**
 * eliminar_subcamino(+Camino, +ListaOriginal, +ListaCaminosRevisar, -ListaSinSubcamino)
 * ListaSinSubcamino es la lista de caminos ListaOriginal sin el camino Camino, si este resulta ser un subcamino que se encuentra 
 * en la ListaCaminosRevisar. Notar que ListaCaminosRevisar es igual a ListaOriginal en la primera llamada cuando se llama desde 
 * fuera.
 */
% al igual que en eliminar_subcaminos, ListaOriginal se utiliza en el caso base
eliminar_subcamino(_, ListaOriginal, [], ListaOriginal).
eliminar_subcamino(Camino, ListaOriginal, ListaCaminosRevisar, ListaSinSubcamino) :-
    ListaCaminosRevisar = [SuperCamino|_],
    %P es donde termina el subcamino en realidad (elemento de más abajo a la derecha)
    Camino = [P|_Ps],
    %entonces si P aparece en alguno de los otros caminos, en esta llamada, en camino, significa que Camino es un subcamino
    esta(P, SuperCamino) ->  
    %entonces lo elimino de la lista y termino la recursión
    select(Camino, ListaOriginal, ListaSinSubcamino);
    ListaCaminosRevisar = [_|RestoCaminos],
    %si no aparece, trato de buscarlo en el resto de caminos
    eliminar_subcamino(Camino, ListaOriginal, RestoCaminos, ListaSinSubcamino).

/**
 * grillas_efecto_booster(+Grilla, +Columnas, +ListadoBooster, -Grillas)
 * Grillas es una lista de grillas conformada las Grillas que conforman el efecto aplicado a la grilla Grilla de Columnas 
 * cantidad de columnas a partir de la lista de caminos ListadoBooster formado por el boster.
 * Grillas se conforma por 2 grillas por cada camino formado por el efecto, siendo la primera de las grillas correspondiente 
 * al camino eliminado y la segunda al camino eliminado con la posición de más abajo a la derecha reemplazada por el valor 
 * correspondiente.
 * Después del primer par, el resto de los pares se forman de manera incremental a partir de la última grilla de los pares 
 * anteriores, es decir:
 * - la grilla primera de las grillas correspondientes al segundo camino tendrá el camino de más arriba a la izquierda con 
 *   sus celdas eliminadas y la última reemplazada y este segundo camino con las celdas eliminadas.
 * - la segunda grilla del segundo camino tendrá la todo lo de la anterior, más la posición de más abajo a la izquierda 
 *   reemplazada por el valor correspondiente.
 * Y así sucesivamente, hasta la última de las grillas (primera del ListadoBooster, en realidad)
 */
grillas_efecto_booster(Grilla, _Columnas, [], [Grilla]).
grillas_efecto_booster(Grilla, Columnas, ListadoBooster, Grillas) :-
    ListadoBooster = [Camino|RestoCaminos],
    %invierto al camino, dado que grillasCaminoReemplazado asume que la posición en la que termina el camino es la última 
    %y en la lista recibida por parámetro está al principio
    invertir(Camino, CaminoCeldaPuntajeAlFinal),
    %obtengo las grillas del camino
    grillas_camino_reemplazado(CaminoCeldaPuntajeAlFinal, Grilla, [GrillaSinCamino, GrillaFinCaminoReemplazado]),
    %utilizo la última de las grillas como referencia para continuar la recursión
    grillas_efecto_booster(GrillaFinCaminoReemplazado, Columnas, RestoCaminos, GrillasSinPrimerCamino),
    %añado a la primera de las grillas que formé, el resto de las grillas
    append([GrillaSinCamino], GrillasSinPrimerCamino, Grillas).

/**
 * grillas_camino_reemplazado(+Camino, +Grilla, -ParDeGrillas)
 * Retorna Par de grillas como una lista compuesta de dos grillas, donde la primera se corresponde con el camino Camino 
 * eliminado de la grilla Grilla y la segunda se corresponde con la primera pero con la celda del final del camino 
 * reemplazada por la menor potencia de dos mayor o igual a la sumatoria de los valores de las posiciones que conforman
 * el camino en la grilla.
 */
grillas_camino_reemplazado(Camino, Grilla, ParDeGrillas) :-
    reemplazar_por_vacio(Camino, Grilla, GrillaConCeldasVacias),
	reemplazo_celda_fin_camino(GrillaConCeldasVacias, Grilla, Camino, GrillaCeldaFinReemplazada),
    ParDeGrillas = [GrillaConCeldasVacias, GrillaCeldaFinReemplazada].


% obtengo la lista de posiciones que conforman el camino de posiciones con mismo valor que posX en la grilla
% ListaPosicionesRevisadas es la lista de posiciones revisadas en la grilla hasta el momento y
% ListaPosicionesRevisadasAlFinal es la lista de posiciones revisadas después de formar el camino
% (incluye las posiciones del camino)
/**
 * camino(+PosX, +Grilla, +Columnas, -ListaPosCamino)
 * ListaPosCamino es la lista de posiciones que conforman el camino arrancando por PosX, en la grilla de Columnas 
 * cantidad de columnas.
 */
camino(PosX, Grilla, Columnas, ListaPosCamino) :-
	%obtengo la lista de adyacentes (siempre son posiciones anteriores a posX)
    lista_adyacentes(PosX, Grilla, Columnas, ListaAdyacentes),
    %añado x a la lista del camino, asegurándome que la posición de más abajo a la derecha quede primera
    ListaParcial = [PosX],
    %conformo el camino verificando cada adyacente
    camino_rec(PosX, Grilla, Columnas, ListaAdyacentes, ListaParcial, ListaPosCamino).

/**
 * camino_rec(+PosX, +Grilla, +Columnas, +ListaAdyacentes, +ListaCaminoActual, -ListaPosCamino)
 * ListaPosCamino es la lista de posiciones que conforman el camino que comienza en PosX en la grilla Grilla, de 
 * Columnas numero de columnas, a partir de su lista de posiciones adyacentes ListaAdyacentes continuando el camino 
 * de ListaCaminoActual.
 */
camino_rec(_PosX, _Grilla, _Columnas, [], ListaCaminoActual, ListaCaminoActual).
camino_rec(PosX, Grilla, Columnas, ListaAdyacentes, ListaCaminoActual, ListaPosCamino) :-
    ListaAdyacentes = [A|As],
    %obtengo el valor de PosX en la grilla
    nth0(PosX, Grilla, ValorPosX),
    %obtengo el valor de A en la grilla
   	nth0(A, Grilla, ValorPosA),
    (
    	%si los valores coinciden
    	ValorPosX is ValorPosA, not(esta(A, ListaCaminoActual)) ->  
    	%obtengo la lista de adyacentes
    	lista_adyacentes(A, Grilla, Columnas, ListaAdyacentesA),
        %añado A a la lista del camino
        append(ListaCaminoActual, [A], ListaParcialConA),
        %verifico cada adyacente
    	camino_rec(A, Grilla, Columnas, ListaAdyacentesA, ListaParcialConA, SubcaminoDesdeA),
        CaminoParcial = SubcaminoDesdeA;
    	%sino sigo con la recursión con el camino actual
    	CaminoParcial = ListaCaminoActual
    ),
    camino_rec(PosX, Grilla, Columnas, As, CaminoParcial, ListaPosCamino).

/**
 * lista_adyacentes(+Pos, +Grilla, +Columnas, -Lista)
 * Lista es la lista de posiciones adyacentes a la posición Pos en la grilla Grilla de Columnas cantidad de 
 * columnas.
 */
lista_adyacentes(Pos, Grilla, Columnas, Lista) :-
    length(Grilla, Long),
    Pos >= 0,
    Pos < Long,
    %Calculo las posiciones adyacentes que me podrían llegar a interesar
    PosI is Pos - 1,
    PosA is Pos - Columnas,
    PosDA is PosA + 1,
    PosIA is PosA - 1,
    PosIAb is PosI + Columnas,
    %añado las correspondientes, según la ubicación de pos en la grilla
    (
    	Pos is 0 ->
    	Lista = [];
        es_borde_sup(Pos, Grilla, Columnas) ->
        Lista = [PosIAb, PosI];
    	es_borde_izq(Pos, Grilla, Columnas) ->  
        Lista = [PosDA, PosA];
    	es_borde_inf_der(Pos, Grilla, Columnas) ->  
    	Lista = [PosI, PosA, PosIA];
    	es_borde_der(Pos, Grilla, Columnas) ->
        Lista = [PosIAb, PosI, PosA, PosIA];
    	es_borde_inf(Pos, Grilla, Columnas) ->  
    	Lista = [PosI, PosDA, PosA, PosIA];
    	Lista = [PosIAb, PosI, PosDA, PosA, PosIA]
    ).
