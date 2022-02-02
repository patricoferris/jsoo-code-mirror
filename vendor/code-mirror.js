var cm = {
    View: require('@codemirror/basic-setup').EditorView,
    State: require('@codemirror/basic-setup').EditorState,
    lint: require('@codemirror/lint'),
    autocomplete: require('@codemirror/autocomplete'),
    basicSetup: require('@codemirror/basic-setup').basicSetup,
    dark: require('@codemirror/theme-one-dark'),
    streamParser: require('@codemirror/stream-parser'),
    OCaml: require('@codemirror/legacy-modes/mode/mllike.cjs').oCaml,
  };
  
  global.CodeMirror = cm;
  module.exports = cm;