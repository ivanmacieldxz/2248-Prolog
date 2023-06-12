import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import { joinResult, numberToColor } from './util';
import Square from './Square';
import Booster from './Booster';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);

  useEffect(() => {
    // This is executed just once, after the first render.
    PengineClient.init(onServerReady);
  }, []);

  /**
   * Called when the server was successfully initialized
   */
  function onServerReady(instance) {
    pengine = instance;
    const queryS = 'init(Grid, NumOfColumns)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setNumOfColumns(response['NumOfColumns']);
      }
    });
  }

  /**
   * Called while the user is drawing a path in the grid, each time the path changes.
   */
  function onPathChange(newPath) {
    // No effect if waiting.
    if (waiting) {
      return;
    }

    
    if (newPath.length > 1) {
      //código de la consulta a sv prolog
      const gridS = JSON.stringify(grid);
      const pathS = JSON.stringify(newPath);
      const queryS = "preview(" + gridS + "," + numOfColumns + "," + pathS + ", Prev)";

      pengine.query(queryS, (success, response) => {
        if (success) {
          console.log(response);
          mostrarResultadoParcial(response.Prev);
        }
      });
    } else {
      mostrarResultadoParcial(0);
    }

    setPath(newPath);
    console.log(JSON.stringify(newPath));
  }

  /**
   * Called when the user finished drawing a path in the grid.
   */
  function onPathDone() {
    /*
    Build Prolog query, which will be like:
    join([
          64,4,64,32,16,
          64,8,16,2,32,
          2,4,64,64,2,
          2,4,32,16,4,
          16,4,16,16,16,
          16,64,2,32,32,
          64,2,64,32,64,
          32,2,64,32,4
          ], 
          5, 
          [[2, 0], [3, 0], [4, 1], [3, 1], [2, 1], [1, 1], [1, 2], [0, 3]],
          RGrids
        ).
    */
    mostrarResultadoParcial(0);
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      
      console.log(response);
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setPath([]);
        animateEffect(response['RGrids']);
      } else {
        setWaiting(false);
      }
    });
  }

  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids) {
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {
      setTimeout(() => {
        animateEffect(restRGrids);
      }, 300);
    } else {
      setWaiting(false);
    }
  }

  function mostrarResultadoParcial(puntajeParcial) {
    let celdaPuntaje = document.querySelector(".previewBanner .square");
    let banner = document.querySelector(".previewBanner");

    if (puntajeParcial !== 0) {
      banner.style.backgroundColor = "white";
      celdaPuntaje.innerText = puntajeParcial;
      celdaPuntaje.style.backgroundColor = numberToColor(puntajeParcial);
    } else {
      banner.style.backgroundColor = "";
      celdaPuntaje.innerText = "";
      celdaPuntaje.style.backgroundColor = "";
    }
    
  }

  function colapsarIguales() {

    if(!waiting) {

      //Bugfix: previene jugadas ilegales al presionar el booster cuando se estaba haciendo un camino
      if (path !== []) {
        setPath([]);
        //deja de mostrar el resultado de terminar el camino en el momento
        mostrarResultadoParcial(0);
      }
      
      const gridS = JSON.stringify(grid);
      const queryS = "booster_iguales(" + gridS + "," + numOfColumns + ", GrillasBooster)";
      setWaiting(true);
      pengine.query(queryS, (success, response) => {
        if (success) {
          animateEffect(response['GrillasBooster']);
          //console.log(response.NuevaMatriz);
          //setWaiting(false);
        } else {
          setWaiting(false);
        }
      });

    }

  }

  if (grid === null) {
    return null;
  }
  return (
    <div className="game">
      <div className='previewBanner'>
        <Square
          value={0}
          onClick={() => {}}
          onMouseEnter={() => {}}
          className={""}
        />
      </div>
      <div className="header">
        <div className="score">{score}</div>
      </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      />
      <div className='footer'>
        <Booster
          value={"Colapsar Iguales"}
          onClick={colapsarIguales}
        />
      </div>
    </div>
  );
}

export default Game;