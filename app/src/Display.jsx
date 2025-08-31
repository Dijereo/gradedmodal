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

function ModelGraph({ elements, setNodeFormulae }) {
    return (
        <CytoscapeComponent
            cy={(cy) => {
                cy.on('click', 'node', function (evt) {
                    var node = evt.target;
                    setNodeFormulae(cy.getElementById(node.id()).data().formulae);
                });
            }}
            elements={CytoscapeComponent.normalizeElements(elements)}
            layout={{ name: "breadthfirst" }}
            style={{ width: '1000px', height: '400px' }}
            stylesheet={[
                {
                    selector: 'node',
                    style: {
                        'background-color': 'data(bg)',
                        'label': 'data(label)',
                        'text-valign': 'center',
                        'text-halign': 'right',
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
                        'source-arrow-shape': 'data(sym)',
                        'curve-style': 'bezier',
                    }
                }
            ]}
        />
    );
}

function FormulaList({ formulae, rows, cols, placeholder }) {
    return (
        <>
            <textarea
                value={formulae}
                rows={rows} cols={cols} readOnly wrap="off"
                placeholder={placeholder}></textarea>
        </>
    );
}


export default function TabDisplay({ proofdata }) {
    const [tab, setTab] = useState("instructions");
    const [nodeFormulae, setNodeFormulae] = useState([""]);
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
                    <div className="upper-middle-column">
                        <div className="middle-column">
                            <ModelGraph elements={proofdata.model} setNodeFormulae={setNodeFormulae} />
                        </div>
                        <FormulaList formulae={nodeFormulae} rows={"22"} cols={"20"} placeholder={"Click on a node!"} />
                    </div>
                }
                {
                    tab === "tableau" &&
                    <div className="right-column">
                        <FormulaList formulae={proofdata.tableau} rows={"22"} cols={"120"} placeholder={"No tableaux to display!"} />
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
