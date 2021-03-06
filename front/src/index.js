import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

const key = 'savedstate';

app.ports.loadSavedState.send(window.localStorage.getItem(key) || "");

app.ports.storeSavedState.subscribe(function(message) {
	window.localStorage.setItem(key, message);
});


// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
