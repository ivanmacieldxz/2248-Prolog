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
 * posiciones del camino Camino en laf grilla Grilla.
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

/**
 * generacion_segundas_grillas(+Grid, +NumOfColumns, -GrillaEfectoFinal)
 * GrillasEfectoFinal es una lista conformada por dos listas, que representan dos grillas donde la primera 
 * se corresponde con la grilla original Grilla, pero con el efecto de gravedad aplicado; y la ultima 
 * correspondiendose con la grilla anterior, pero con las celdas vacias completas con número aleatoreos.
 */
generacion_segundas_grillas(Grid, NumOfColumns, GrillaEfectoFinal):-
    listas_columnas(Grid, NumOfColumns, ListaColumnas),
    findall(ListasGravedad,(member(ListSinG,ListaColumnas),efecto_gravedad_columna(ListSinG,ListasGravedad)),ListasColumnasGravedadAp),
    invertir(ListasColumnasGravedadAp, ListaOrdenada),
    obtener_grilla_de_columnas(ListaOrdenada, GrillaEfectoGravedad),
    generacion_nuevas_celdas(GrillaEfectoGravedad, GrillaCeldasNuevas),
    GrillaEfectoFinal = [GrillaEfectoGravedad,GrillaCeldasNuevas].

/**
 *  listas_columnas(+Grid, +NumOfColumns, -ListaColumn)
 *  ListaColumn es una lista que contiene las listas de los elementos de cada columnas de la grilla Grid.
 *  Si Grid tiene NumOfColumnas cantidad de columnas, ListaColumn tendra NumOfColumns cantidad de listas.
 */
listas_columnas(Grid, NumOfColumns, ListaColumn):-
    grilla_posiciones(Grid,0,0,NumOfColumns,GridPos),
    lista_numeros(NumOfColumns,Columnas),
    %Para cada columna de la grilla, busca las listas que contienen los elementos pertenencientes a una columna.
    findall(List,(member(C,Columnas),lista_columna(C,NumOfColumns,Grid,GridPos,List)),ListaColumn).

/**
*  lista_columna(+Num, +NumOfColumns, +Grid, +GridPos, -Lista)
*  Lista es una lista que contiene los elementos de la columna Num de la grilla Grid.
*  GridPos es una grilla que contiene las pos en formato matricial correspondientes a Grid.
*/
lista_columna(Num, NumOfColumns, Grid, GridPos, Lista):-
    %Para cada elemento ubicada en la misma columna Num, agregarla a la lista Lista 
    findall(Elem,(member([X,Num],GridPos), elemento_de_grid(Grid, NumOfColumns, X, Num, Elem)),Lista).

/**
*  efecto_gravedad_columna(+List, -ListEfectoGravedad)
*  LitEfectoGravedad es una lista que posee los mismo elementos que la lista List pero con
*  con el efecto de gravedad aplicado: los ceros pasan a estar en las primeras pos y los 
*  distintos a cero en las ultimas.
*/
efecto_gravedad_columna(List, ListEfectoGravedad):-
    lista_ceros(List,ListCeros),
    lista_no_ceros(List,ListNoCeros),
    concatenar(ListCeros,ListNoCeros, ListEfectoGravedad).

/**
*  obtener_grilla_de_columnas(+ListaColumn, -Grid)
*  A partir de la lista de columnas ListaColumn, se obtiene su grilla equivalente Grid.
*  La grilla Grid es la concatenacion de las listas de columnas.
*/
obtener_grilla_de_columnas(ListaColumn, Grid):-
    %Obtengo el tamaño de una de las listas de ListaColumn
    member(L,ListaColumn), !,
    length(L, N),
    %Obtengo las listas de las filas
    listas_filas(ListaColumn, N, L2),
    %Como L2 está al revés, invertimos la lista
    invertir(L2, ListaFilas),
    %concatenamos todas las listas y obtenemos la grilla Grid
    concatenar_listas_de_listas(ListaFilas, Grid).

/**
*  listas_filas(+ListaColumn, +N, -ListasF)
*  Se obtiene la lista de las N filas de una grilla a partir de una lista que contiene
*  las columnas de la misma. Se busca desde la fila N a la fila 0.
*/
%caso base
listas_filas(_, 0, []).
%caso recursivo
listas_filas(L1, N, [H|T]) :-
    %busca la lista de elementos de la fila N
    lista_fila(L1, N, H),
    N1 is N - 1,
    %busca fila anterior
    listas_filas(L1, N1, T), !.

/**
*  lista_fila(+ListaColumn, +N, -ListaF)
*  ListaF es una lista que contiene los elementos de pos N en las listas de 
*  ListaColumn
*/
%caso base
lista_fila([], _, []).
%caso recursivo
lista_fila([H|T], N, [E|L]) :-
    %agregamos elemento N pos de lista H en ListaF
    nth1(N, H, E),
    lista_fila(T, N, L).

/**
*  grilla_posiciones(+Grid, +F, +C, +NumOfColumns, -GridPos)
*  GridPos es una grilla que contiene todas las pos del estilo [X,Y] correspondiente a
*  la grilla Grid a partir de la posición [F, C]
*/
%caso base
grilla_posiciones([],_F, _C, _NumOfColumns, []).
%caso recursivo 1
grilla_posiciones([_G|GS], F, C, NumOfColumns, [[F,C]|XS]):-
   %mientras todavia no estemos en la ultima columna
   C < NumOfColumns,
   NC is C + 1,
   grilla_posiciones(GS, F, NC, NumOfColumns, XS), !.
%caso recursivo 2
grilla_posiciones([_G|GS], F, C, NumColumns, [[F,C]|XS]):-
   %si estamos en la ultima columna
   C is NumColumns - 1,
   %pasamos a siguiente fila y volvemos a la columna inicial de la grilla
   NF is F + 1,
   grilla_posiciones(GS, NF, 0, NumColumns, XS).

/**
*  elemento_de_grid(+GridList, +NumColumns, +X, +Y, -Element) 
*  Element se corresponde con el elemento que se encuentra en la pos [X,Y] en la grilla
*  GridList de NumColumns cantidad de columnas
*/
elemento_de_grid(GridList, NumColumns, X, Y, Element) :-
    %el elemento ubicado en la pos Index en la lista GridList debe ser igual a:
    nth0(Index, GridList, Element),
    X is div(Index, NumColumns),
    Y is mod(Index, NumColumns).

/*-------------------------- Nuevos Celdas Aleatorias  ---------------------------------*/

/**
*  generacion_nuevas_celdas(+Grilla, -GrillaCompleta)
*  GrillaCompleta es una grilla que contiene los mismos elementos de Grilla, pero los que estaban
*  en cero en esta ultima, pasan a tener otro valor en la grilla resultante.
*/
%caso base
generacion_nuevas_celdas([],[]).
%caso recursivo 1
generacion_nuevas_celdas([X|XS], [X|XSS]):-
    %Si X es distinto de cero, pertenece a las dos grillas en la misma pos
    X \= 0,
    generacion_nuevas_celdas(XS,XSS).
%caso recursivo 2
generacion_nuevas_celdas([0|XS], [X|XSS]):-
    %Si ahora hay elemento igual a cero en Grilla
    %El valor X de GrillaCompleta pasa a ser un número random
    generar_valor_random(X),
    generacion_nuevas_celdas(XS,XSS).

/**
*  generar_valor_random(-X)
*  Siguiendo la lógica del juego, los unicos números random que se pueden obtener son:
*  el 2, 4, 8, 16, 32 o 64
*/
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

/*
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
*/


%-----------------------------------------------------------------------------------

booster_iguales(Grilla, Columnas, Grillas) :-
    %todos los caminos que el jugador podría formar
    lista_listas_caminos_conformables(Grilla, Columnas, ListaCaminos),
    %obtengo las grillas de la animación del efecto
    grillas_efecto_booster(Grilla, Columnas, ListaCaminos, GrillasBoosterAplicado),
	%obtengo la grilla final del efecto del booster
	length(GrillasBoosterAplicado, LBoostAp),
	nth1(LBoostAp, GrillasBoosterAplicado, UltimaGrillaBooster),
	%y la uso como base para aplicarle el efecto de gravedad
	generacion_segundas_grillas(UltimaGrillaBooster, Columnas, GrillaEfectoFinal),
	%finalmente, agrego las grillas del efecto de gravedad
	append(GrillasBoosterAplicado, GrillaEfectoFinal, Grillas).

lista_listas_caminos_conformables(Matriz, Columnas, Lista_listas) :-
    length(Matriz, Tam_matriz),
    Pos_ini is Tam_matriz - 1,
    lista_listas_caminos_conformables_rec(Pos_ini, Matriz, Columnas, [], Lista_listas_duplicados_y_subcaminos),
    lista_listas_sin_duplicados_subcaminos(Lista_listas_duplicados_y_subcaminos, Lista_listas_duplicados_y_subcaminos, Lista_listas_sin_d_ni_s),
    lista_listas_mayor_final(Lista_listas_sin_d_ni_s, Lista_listas_sin_d_ni_s, Lista_listas_mayor_final),
    lista_listas_sin_unitarias(Lista_listas_mayor_final, Lista_listas_mayor_final, Lista_listas).

lista_listas_mayor_final([], Lista_referencia, Lista_referencia).
lista_listas_mayor_final([L|T], Lista_referencia, Lista_final) :-
    obtener_mayor(L, Mayor),
    reemplazar_al_final(Mayor, L, L_mayor_al_final),
    replace(L, L_mayor_al_final, Lista_referencia, Lista_parcial),
    lista_listas_mayor_final(T, Lista_parcial, Lista_final).

lista_listas_sin_unitarias([], Lista_listas_referencia, Lista_listas_referencia).
lista_listas_sin_unitarias([L|T], Lista_listas_referencia, Lista_listas) :-
    length(L, TamL),
    (
        TamL < 2 ->
        select(L, Lista_listas_referencia, Lista_listas_parcial);
        Lista_listas_parcial = Lista_listas_referencia
    ),
    lista_listas_sin_unitarias(T, Lista_listas_parcial, Lista_listas).

reemplazar_al_final(X, Lista, Lista_X_al_final):-
    select(X, Lista, Lista_sin_X),
    append(Lista_sin_X, [X], Lista_X_al_final).

% Predicado principal para obtener el mayor elemento de una lista
obtener_mayor(Lista, Mayor) :-
    % Llamamos al predicado auxiliar con la lista y un valor inicial menor que cualquier elemento posible
    obtener_mayor_aux(Lista, 0, Mayor).

% Caso base: cuando la lista está vacía, el mayor es el valor acumulado actual
obtener_mayor_aux([], MayorActual, MayorActual).
obtener_mayor_aux([H|T], MayorActual, Mayor) :-
    % Si la cabeza de la lista es mayor que el valor acumulado actual,
    % actualizamos el valor acumulado
    (H > MayorActual ->
        NuevoMayorActual = H;
        NuevoMayorActual = MayorActual
    ),
    % Llamamos recursivamente al predicado con el resto de la lista
    obtener_mayor_aux(T, NuevoMayorActual, Mayor).

% primera lista es la original, segunda lista es la actual, última lista es el resultado final
lista_listas_sin_duplicados_subcaminos([], Lista_listas_parcial, Lista_listas_parcial).
lista_listas_sin_duplicados_subcaminos([L|T], Lista_listas_parcial, Lista_listas_final) :-
    (
    	%quito a L de la lista parcial
    	select(L, Lista_listas_parcial, Lista_referencia_sin_L),
    	%si existe una lista en la de referencia, que no sea L, tal que L tenga elementos en común:
    	member(Lista, Lista_referencia_sin_L), intersection(L, Lista, Interseccion), not(Interseccion = []) ->  
    	(
        	%elimino de L los elementos que tengan en común
            eliminar_sublista(Interseccion, L, Elementos_solo_en_L),
            %conformo la unión añadiendo en lista los elementos que no tienen en común
            append(Lista, Elementos_solo_en_L, Union_listas),
            %reemplazo lista en la lista de listas por su unión con L
            replace(Lista, Union_listas, Lista_referencia_sin_L, Lista_listas_parcial_sin_L)
        );
    	Lista_listas_parcial_sin_L = Lista_listas_parcial
    ),
    lista_listas_sin_duplicados_subcaminos(T, Lista_listas_parcial_sin_L, Lista_listas_final).
    

/*
 * Se vuelve verdadero si Lista_listas_final es la lista que contiene todos los caminos conformables por 
 * bloques de igual número en la grilla.
 * Los caminos de Lista_listas_final pueden estar repetidos o tener subcaminos.
 * */
lista_listas_caminos_conformables_rec(Pos, _Matriz, _Columnas, Lista_listas_final, Lista_listas_final) :-
    Pos is -1.
lista_listas_caminos_conformables_rec(Pos, Matriz, Columnas, Lista_listas_parcial, Lista_listas_final) :-
    (
    	%si hay alguna lista en la lista de listas que la contenga
    	member(Lista, Lista_listas_parcial), member(Pos, Lista) ->
    	%entonces dejo la lista de listas como está
    	Parcial_sin_adyacentes = Lista_listas_parcial;
    	%sino, conformo la lista que contiene a pos y la añado a la lista de listas
    	Lista = [Pos],
        append(Lista_listas_parcial, [Lista], Parcial_sin_adyacentes)
    ),
    lista_parcial_con_adyacentes(Pos, Matriz, Columnas, Lista, Lista_pos_con_adyacentes),
	PosSig is Pos - 1,
    replace(Lista, Lista_pos_con_adyacentes, Parcial_sin_adyacentes, Parcial_con_adyacentes),
    lista_listas_caminos_conformables_rec(PosSig, Matriz, Columnas, Parcial_con_adyacentes, Lista_listas_final).

eliminar_sublista(_, [], []).
eliminar_sublista([], [X|T], [X|T]).
eliminar_sublista([X|T], Lista, Resultado) :-
    Lista = [_H|_T1],
    delete(Lista, X, Lista_sin_x),
    eliminar_sublista(T, Lista_sin_x, Resultado).

% Predicado para obtener la intersección de dos listas
intersection([], _, []).
intersection([H | T], L2, [H | Result]) :- 
    member(H, L2),
    intersection(T, L2, Result).
intersection([_ | T], L2, Result) :- 
    intersection(T, L2, Result).


lista_parcial_con_adyacentes(Pos, Matriz, Columnas, Sin_adyacentes, Con_adyacentes) :-
    lista_adyacentes(Pos, Matriz, Columnas, Adyacentes),
    nth0(Pos, Matriz, Valor),
    lista_expandida_con_iguales(Valor, Adyacentes, Matriz, Sin_adyacentes, Con_adyacentes).

lista_expandida_con_iguales(_, [], _, Sin_adyacentes, Sin_adyacentes).
lista_expandida_con_iguales(Valor, [X|T], Matriz, Sin_adyacentes, Con_adyacentes) :-
    nth0(X, Matriz, ValorX),
    (
    	Valor = ValorX, not(member(X, Sin_adyacentes)) ->
    	append(Sin_adyacentes, [X], Sin_adyacentes_con_X);
    	Sin_adyacentes_con_X = Sin_adyacentes
    ),
    lista_expandida_con_iguales(Valor, T, Matriz, Sin_adyacentes_con_X, Con_adyacentes).

replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- dif(H,O), replace(O, R, T, T2).

lista_adyacentes(Pos, Grilla, Columnas, Lista) :-
    length(Grilla, Long),
    Pos >= 0,
    Pos < Long,
  	%Posiciones adyacentes
    PosI is Pos - 1,
    PosD is Pos + 1,
    PosA is Pos - Columnas,
    PosAb is Pos + Columnas,
    PosDA is PosA + 1,
    PosIA is PosA - 1,
    PosIAb is PosAb - 1,
    PosDAb is PosAb + 1,
    %añado las correspondientes, según la ubicación de pos en la grilla
    (
    	%borde superior izquierdo
    	Pos is 0 ->
    	Lista = [PosD, PosAb, PosDAb];
    	es_borde_sup_der(Pos, Grilla, Columnas) ->  
    	Lista = [PosI, PosIAb, PosAb];
        es_borde_sup(Pos, Grilla, Columnas) ->
        Lista = [PosI, PosD, PosIAb, PosAb, PosDAb];
    	es_borde_inf_der(Pos, Grilla, Columnas) ->  
    	Lista = [PosIA, PosA, PosI];
    	es_borde_inf_izq(Pos, Grilla, Columnas) ->  
    	Lista = [PosA, PosDA, PosD];
    	es_borde_inf(Pos, Grilla, Columnas) ->  
    	Lista = [PosIA, PosA, PosDA, PosI, PosD];
    	es_borde_izq(Pos, Grilla, Columnas) ->  
    	Lista = [PosA, PosDA, PosD, PosAb, PosDAb];
    	es_borde_der(Pos, Grilla, Columnas) ->  
    	Lista = [PosIA, PosA, PosI, PosIAb, PosAb];
    	Lista = [PosIA, PosA, PosDA, PosI, PosD, PosIAb, PosAb, PosDAb]
    ).

%-----------------------------------------------------------------------------------
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
    %invertir(Camino, CaminoCeldaPuntajeAlFinal),
    %obtengo las grillas del camino
    grillas_camino_reemplazado(Camino, Grilla, [GrillaSinCamino, GrillaFinCaminoReemplazado]),
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
 *
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
*/