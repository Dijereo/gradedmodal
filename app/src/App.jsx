import React, { useState } from "react";
import './App.css'
import TabButtons from "./Tabs";
import TabDisplay from "./Display";

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

function SearchBar({ setResponseData, searchFormula, setSearchFormula }) {
  const [submissionType, setSubmissionType] = useState("sat");
  const [frameClass, setFrameClass] = useState("K");

  const handleSubmit = async (e) => {
    const startTime = performance.now();
    e.preventDefault();

    const payload = {
      formula: searchFormula,
      frames: frameClass,
      action: submissionType,
    };

    try {
      const res = await fetch("/api", {
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
      const elapsedMs = performance.now() - startTime;
      const end2EndTime = `${elapsedMs.toFixed(3)} ms`;
      console.log(end2EndTime);
      setResponseData(responseJsonData, end2EndTime);
    } catch (err) {
      console.error("Failed to submit:", err);
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      <div className="formula-input">
        <textarea
          value={searchFormula}
          onChange={(e) => setSearchFormula(e.target.value)} rows="3" cols="100" placeholder="Enter formula here..."></textarea>
        <button type="submit" onClick={() => setSubmissionType("sat")}>Satisfy</button>
        <button type="submit" onClick={() => setSubmissionType("val")}>Validate</button>
      </div>
      <div className="frame-options">
        <label><input type="radio" name="frames" value="K" checked={frameClass === "K"} onChange={() => setFrameClass("K")} />K</label>
        <label><input type="radio" name="frames" value="D" checked={frameClass === "D"} onChange={() => setFrameClass("D")} />D</label>
        <label><input type="radio" name="frames" value="T" checked={frameClass === "T"} onChange={() => setFrameClass("T")} />T</label>
        <label><input type="radio" name="frames" value="KB" checked={frameClass === "KB"} onChange={() => setFrameClass("KB")} />KB</label>
        <label><input type="radio" name="frames" value="DB" checked={frameClass === "DB"} onChange={() => setFrameClass("DB")} />DB</label>
        <label><input type="radio" name="frames" value="TB" checked={frameClass === "TB"} onChange={() => setFrameClass("TB")} />TB</label>
        <label hidden><input hidden type="radio" name="frames" value="K4" checked={frameClass === "K4"} onChange={() => setFrameClass("K4")} />K4</label>
        <label hidden><input hidden type="radio" name="frames" value="D4" checked={frameClass === "D4"} onChange={() => setFrameClass("D4")} />D4</label>
        <label hidden><input hidden type="radio" name="frames" value="S4" checked={frameClass === "S4"} onChange={() => setFrameClass("S4")} />S4</label>
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
  const defaultTimes = {
    solveTime: "N/A",
    serverTime: "N/A",
    end2EndTime: "N/A",
    parseTime: "N/A",
    tabWrTime: "N/A",
    modelTime: "N/A"
  };
  const [searchFormula, setSearchFormula] = useState("_|_");
  const [computeTimes, setComputeTimes] = useState(defaultTimes);
  const [formulaList, setFormulaList] = useState(["Placeholder ∅⨉✓⊥⊤¬□≥◇≤∧∨→↔"]);
  const [symmetric, setSymmetric] = useState(true);
  const [graphData, setGraphData] = useState(
    {
      nodes: [
        { data: { id: '1', label: 'Node 1', bg: 'red' }, position: { x: 100, y: 100 } },
        { data: { id: '2', label: 'Node 2', bg: 'blue' }, position: { x: 200, y: 100 } },
        { data: { id: '3', label: 'Node 3', bg: 'green' }, position: { x: 300, y: 100 } },
      ],
      edges: [
        { data: { source: '1', target: '2', label: 'Edge from Node1 to Node2' } },
        { data: { source: '2', target: '3', label: 'Edge from Node2 to Node3' } }
      ],
    }
  );

  function setResponseData(responseJsonData, end2EndTime) {
    if (responseJsonData.times !== undefined) {
      const compute_times = responseJsonData.times;
      const computeTimes = {
        solveTime: compute_times.solve_time !== undefined ? compute_times.solve_time : "N/A",
        serverTime: compute_times.server_time !== undefined ? compute_times.server_time : "N/A",
        end2EndTime: end2EndTime,
        parseTime: compute_times.parse_time !== undefined ? compute_times.parse_time : "N/A",
        tabWrTime: compute_times.tabwrite_time !== undefined ? compute_times.tabwrite_time : "N/A",
        modelTime: compute_times.graph_time !== undefined ? compute_times.graph_time : "N/A",
      };
      setComputeTimes(computeTimes);
    } else {
      setComputeTimes(defaultTimes);
    }
    if (responseJsonData.graph !== undefined) {
      setGraphData(responseJsonData.graph);
    }
    if (responseJsonData.tableau !== undefined) {
      setFormulaList(responseJsonData.tableau);
    }
    if (responseJsonData.formula !== undefined) {
      setSearchFormula(responseJsonData.formula)
    }
    if (responseJsonData.symmetric !== undefined) {
      setSymmetric(responseJsonData.symmetric)
    }
  }

  return (
    <>
      <div className="page">
        <SearchBar searchFormula={searchFormula} setSearchFormula={setSearchFormula} setResponseData={setResponseData} className="formula-query" />
        <TabDisplay proofdata={{ model: graphData, tableau: formulaList, symmetric: symmetric, computeTimes: computeTimes }}></TabDisplay>
      </div>
    </>
  );
}

export default App
