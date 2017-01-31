var Main = require('../src/Main.purs')
var initialState = require('../src/Model.purs').init;
require('../static/css/app.css')
var debug = process.env.NODE_ENV === 'development'

if (module.hot) {
  var app = Main[debug ? 'debug' : 'main'](window.puxLastState || initialState)();
  app.state.subscribe(function (state) {
    window.puxLastState = state;
  });
  module.hot.accept();
} else {
  Main[debug ? 'debug' : 'main'](initialState)();
}
