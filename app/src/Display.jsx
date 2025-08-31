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
                    <div>
                        <h3>Welcome to Golem!</h3>
                        <p>
                            Golem is a tableau-based automated theorem prover for graded modal logic.
                            To use Golem, enter a formula in the input bar at the top of the page,
                            select a logic corresponding to the class of frames you want to work with,
                            and click satisfy to find and display a satisfying model,
                            or prove to prove its validity by tableau refutation.
                            If it is unsatisfiable, the refuting tableau will be shown.
                            If it is not valid, the counter-satisfying model will be shown.
                        </p>
                        <h3>Models</h3>
                        <p>
                            When a model is shown, click on a world to see the formulae that are true at that world.
                            A multiplier label on a world indicates that that number of copies of this world exist.
                            For Euclidean frames, to avoid clutter, not all of the transitions in the clique are shown; they are left implied.
                        </p>
                        <h3>Syntax Reference</h3>
                        <dl>
                            <dt>Propositional Letters</dt>
                            <dd><code>p</code> or <code>pi</code> where p is any lowercase letter, and i is an unsigned integer</dd>
                            <dt>Negation (not)</dt>
                            <dd><code>~</code> or <code>¬</code></dd>
                            <dt>Bottom (constant false)</dt>
                            <dd><code>_|_</code> or <code>⊥</code></dd>
                            <dt>Top (constant true)</dt>
                            <dd><code>T</code> or <code>⊤</code></dd>
                            <dt>Negation (not)</dt>
                            <dd><code>~</code> or <code>¬</code></dd>
                            <dt>Conjunction (and)</dt>
                            <dd><code>&amp;</code> or <code>∧</code></dd>
                            <dt>Disjunction (or)</dt>
                            <dd><code>|</code> or <code>∨</code></dd>
                            <dt>Implication (if, then)</dt>
                            <dd><code>-&gt;</code> or <code>→</code></dd>
                            <dt>Biconditional (iff)</dt>
                            <dd><code>&lt;-&gt;</code> or <code>↔</code></dd>
                            <dt>Parentheses</dt>
                            <dd><code>(  )</code></dd>
                            <dt>Possibility</dt>
                            <dd><code>&lt;&gt;</code> or <code>◇</code></dd>
                            <dt>Necessity</dt>
                            <dd><code>[]</code> or <code>□</code></dd>
                            <dt>Graded Modalities</dt>
                            <dd><code>&lt;&gt;&gt;=c</code> or <code>◇≥c</code> or <code>&lt;&gt;&lt;=c</code> or <code>◇≤c</code> where c is an unsigned integer</dd>
                        </dl>
                    </div>
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
