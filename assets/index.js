import '@webcomponents/webcomponentsjs/webcomponents-bundle.js'
require('./elm-resize.js');
require('./elmEditor.js');

const {
  Elm
} = require('../src/elm/Main.elm');

const app = Elm.Main.init({
  node: document.getElementById('application')
});
