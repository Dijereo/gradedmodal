import React, { useState } from "react";
import './App.css'
import CytoscapeComponent from 'react-cytoscapejs';


function ModelGraph({ elements }) {
  return (
    <CytoscapeComponent elements={CytoscapeComponent.normalizeElements(elements)} style={{ width: '300px', height: '400px' }} />
  );
}

function DisplayFormula({ formula }) {
  return <span className="tableau-formula">{formula}</span>;
}

function FormulaList2({ formulae }) {
  const rows = [];

  formulae.forEach(formula => {
    rows.push(
      <DisplayFormula formula={formula} key={formula} />
    );
  });

  return (
    <>
      <div className="formulae-list">
        {rows}
      </div>
    </>
  );
}

function FormulaList({ formulae }) {
  // const rows = [];

  // formulae.forEach(formula => {
  //   rows.push(
  //     <DisplayFormula formula={formula} key={formula} />
  //   );
  // });

  return (
    <>
      <textarea
        value={formulae}
        rows="23" cols="100"></textarea>
    </>
  );
}

function SearchBar({ setResponseData, setEnd2EndTime }) {
  const [formula, setFormula] = useState("_|_");
  const [frameClass, setFrameClass] = useState("K");

  const handleSubmit = async (e) => {
    const startTime = performance.now();
    e.preventDefault();

    const payload = {
      formula: formula,
      frames: frameClass,
    };

    try {
      const res = await fetch("http://localhost:3000/api", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(payload),
      });
      console.log("Request Sent");

      if (!res.ok) throw new Error("API error");

      const responseJsonData = await res.json();
      console.log("Success!");
      console.log(responseJsonData);
      setResponseData(responseJsonData);

      const elapsedMs = performance.now() - startTime;
      setEnd2EndTime(`${elapsedMs.toFixed(3)} ms`)

    } catch (err) {
      console.error("Failed to submit:", err);
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      <div className="formula-input">
        <textarea
          value={formula}
          onChange={(e) => setFormula(e.target.value)} rows="3" cols="100" placeholder="Enter formula here..."></textarea>
        <button type="submit">Satisfy</button>
      </div>
      <div className="frame-options">
        <label><input type="radio" name="frames" value="K" checked={frameClass === "K"} onChange={() => setFrameClass("K")} />K</label>
        <label><input type="radio" name="frames" value="D" checked={frameClass === "D"} onChange={() => setFrameClass("D")} />D</label>
        <label><input type="radio" name="frames" value="T" checked={frameClass === "T"} onChange={() => setFrameClass("T")} />T</label>
        <label><input type="radio" name="frames" value="KB" checked={frameClass === "KB"} onChange={() => setFrameClass("KB")} />KB</label>
        <label><input type="radio" name="frames" value="DB" checked={frameClass === "DB"} onChange={() => setFrameClass("DB")} />DB</label>
        <label><input type="radio" name="frames" value="TB" checked={frameClass === "TB"} onChange={() => setFrameClass("TB")} />TB</label>
        <label><input type="radio" name="frames" value="K4" checked={frameClass === "K4"} onChange={() => setFrameClass("K4")} />K4</label>
        <label><input type="radio" name="frames" value="D4" checked={frameClass === "D4"} onChange={() => setFrameClass("D4")} />D4</label>
        <label><input type="radio" name="frames" value="S4" checked={frameClass === "S4"} onChange={() => setFrameClass("S4")} />S4</label>
        <label><input type="radio" name="frames" value="K5" checked={frameClass === "K5"} onChange={() => setFrameClass("K5")} />K5</label>
        <label><input type="radio" name="frames" value="D5" checked={frameClass === "D5"} onChange={() => setFrameClass("D5")} />D5</label>
        <label><input type="radio" name="frames" value="K45" checked={frameClass === "K45"} onChange={() => setFrameClass("K45")} />K45</label>
        <label><input type="radio" name="frames" value="D45" checked={frameClass === "D45"} onChange={() => setFrameClass("D45")} />D45</label>
        <label><input type="radio" name="frames" value="KB5" checked={frameClass === "KB5"} onChange={() => setFrameClass("KB5")} />KB5</label>
        <label><input type="radio" name="frames" value="S5" checked={frameClass === "S5"} onChange={() => setFrameClass("S5")} />S5</label>
      </div>
    </form>
  );
}

function App() {
  const [computeTime, setComputeTime] = useState("");
  const [end2EndTime, setEnd2EndTime] = useState("");
  const [formulaList, setFormulaList] = useState(["∅⨉✓⊥⊤¬□≥◇≤∧∨→↔"]);
  const [graphData, setGraphData] = useState(
    {
      nodes: [
        { data: { id: 'one', label: 'Node 1' }, position: { x: 100, y: 100 } },
        { data: { id: 'two', label: 'Node 2' }, position: { x: 200, y: 100 } },],
      edges: [{ data: { source: 'one', target: 'two', label: 'Edge from Node1 to Node2' } }],
    }
  );

  function setResponseData(responseJsonData) {
    if (responseJsonData.compute_time !== undefined) {
      setComputeTime(responseJsonData.compute_time);
    }

    if (responseJsonData.graph_data !== undefined) {
      setGraphData(responseJsonData.graph_data);
    }

    if (responseJsonData.extra !== undefined) {
      setFormulaList(responseJsonData.extra);
    }
  }

  return (
    <>
      <div className="page">
        <SearchBar setResponseData={setResponseData} setEnd2EndTime={setEnd2EndTime} className="formula-query" />
        <div className="main-grid">
          {/* <div className="left-column">
            <div className="top-left">
              <ModelGraph elements={graphData} />
            </div>
            <div className="bottom-left">
              <ModelGraph elements={graphData} />
            </div>
          </div> */}
          <div className="middle-column">
            <ModelGraph elements={graphData} />
          </div>
          <div className="right-column">
            <FormulaList formulae={formulaList} />
          </div>
        </div>
        <div className="compute-times">
          <span>Compute Time: {computeTime}</span>
          <span>End-To-End Time: {end2EndTime}</span>
        </div>
      </div>
    </>
  );
}

export default App
