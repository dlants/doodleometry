var Main = require('../src/Main.purs')
require('../static/css/app.css')
var debug = process.env.NODE_ENV === 'development'
var initialState = require('../src/Model.purs').init;

overrideWithWindowDims = function(state) {
  state.windowWidth = window.innerWidth;
  state.windowHeight = window.innerHeight;
  return state;
}

if (module.hot) {
  var state = overrideWithWindowDims(window.puxLastState || initialState);
  var app = Main[debug ? 'debug' : 'main'](state)();
  app.state.subscribe(function (state) {
    window.puxLastState = state;
  });
  module.hot.accept();
} else {
  var state = overrideWithWindowDims(initialState);
  Main[debug ? 'debug' : 'main'](state)();
}
