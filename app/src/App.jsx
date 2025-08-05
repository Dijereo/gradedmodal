// import reactLogo from './assets/react.svg'
// import viteLogo from '/vite.svg'
import './App.css'
import CytoscapeComponent from 'react-cytoscapejs';


function MyGraph() {

  const elements = [
    { data: { id: 'one', label: 'Node 1' }, position: { x: 100, y: 100 } },
    { data: { id: 'two', label: 'Node 2' }, position: { x: 200, y: 100 } },
    { data: { source: 'one', target: 'two', label: 'Edge from Node1 to Node2' } }
  ];

  return <CytoscapeComponent elements={elements} style={{ width: '300px', height: '150px' }} />;
}

function FormulaList() {
  return (
    <>
      <div className="formulae-list">
        <p>This is the right side with some text.</p>
        <p>You can put anything here: instructions, notes, etc.</p>
      </div>
    </>
  );
}

function SearchBar() {
  return (
    <form>
      <div className="formula-input">
        <textarea rows="3" cols="100" placeholder="Enter formula here..."></textarea>
        <button>Submit</button>
      </div>
      <div className="frame-options">
        <label><input type="radio" name="frames" value="K" />K</label>
        <label><input type="radio" name="frames" value="D" />D</label>
        <label><input type="radio" name="frames" value="T" />T</label>
        {/* <label><input type="radio" name="frames" value="KB" />KB</label>
        <label><input type="radio" name="frames" value="DB" />DB</label>
        <label><input type="radio" name="frames" value="TB" />TB</label>
        <label><input type="radio" name="frames" value="K4" />K4</label>
        <label><input type="radio" name="frames" value="D4" />D4</label>
        <label><input type="radio" name="frames" value="S4" />S4</label> */}
        <label><input type="radio" name="frames" value="K5" />K5</label>
        <label><input type="radio" name="frames" value="D5" />D5</label>
        <label><input type="radio" name="frames" value="K45" />K45</label>
        <label><input type="radio" name="frames" value="D45" />D45</label>
        <label><input type="radio" name="frames" value="KB5" />KB5</label>
        <label><input type="radio" name="frames" value="S5" />S5</label>
      </div>
    </form>
  );
}

function App() {
  return (
    <>
      <div className="page">
        <SearchBar className="formula-query"/>
        <div className="main-grid">
          <div className="left-column">
            <div className="top-left">
              <MyGraph />
            </div>
            <div className="bottom-left">
              <MyGraph />
            </div>
          </div>
          <div className="middle-column">
            <MyGraph />
          </div>
          <div className="right-column">
            <FormulaList />
          </div>
        </div>
      </div>
    </>
  );
}

export default App
