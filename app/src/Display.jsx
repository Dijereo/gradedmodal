import React, { useState } from "react";
import TabButtons from "./Tabs";
import CytoscapeComponent from 'react-cytoscapejs';

function Times({ computeTimes }) {
    return (
        <>
            <div className="times">
                <span>Solve Time: {computeTimes.solveTime}</span>
                <span>Server Time: {computeTimes.serverTime}</span>
                <span>End-To-End Time: {computeTimes.end2EndTime}</span>
            </div>
        </>
    );
}

function ModelGraph({ elements, symmetric }) {
    return (
        <CytoscapeComponent
            elements={CytoscapeComponent.normalizeElements(elements)}
            layout={{ name: "breadthfirst" }}
            style={{ width: '1200px', height: '400px' }}
            stylesheet={[
                {
                    selector: 'node',
                    style: {
                        'background-color': 'data(bg)'
                    }
                },
                {
                    selector: 'edge',
                    style: {
                        'width': 3,
                        'line-color': '#000',
                        'target-arrow-color': '#000',
                        'target-arrow-shape': 'triangle',
                        'source-arrow-color': '#000',
                        'source-arrow-shape': symmetric ? 'triangle' : 'none',
                        'curve-style': 'bezier',
                    }
                }
            ]}
        />
    );
}

function FormulaList({ formulae }) {
    return (
        <>
            <textarea
                value={formulae}
                rows="22" cols="120" readOnly wrap="off"></textarea>
        </>
    );
}


export default function TabDisplay({ proofdata }) {
    const [tab, setTab] = useState("instructions");
    const buttonStates = [
        tab === "instructions" ? "active" : "inactive",
        "model" in proofdata ? (tab === "model" ? "active" : "inactive") : "hidden",
        "tableau" in proofdata ? (tab === "tableau" ? "active" : "inactive") : "hidden",
    ];
    const buttonClicks = [
        () => setTab("instructions"),
        () => setTab("model"),
        () => setTab("tableau"),
    ];

    return (
        <>
            <TabButtons
                states={buttonStates}
                onClicks={buttonClicks}
            />
            <div className="main-grid">
                {
                    tab === "instructions" &&
                    <div>Here are the instructions...</div>
                }
                {
                    tab === "model" &&
                    <div className="middle-column">
                        <ModelGraph elements={proofdata.model} symmetric={proofdata.symmetric} />
                    </div>
                }
                {
                    tab === "tableau" &&
                    <div className="right-column">
                        <FormulaList formulae={proofdata.tableau} />
                    </div>
                }
            </div>
            {
                (tab === "tableau" || tab == "model") &&
                <Times computeTimes={proofdata.computeTimes} />
            }
        </>
    );
}
