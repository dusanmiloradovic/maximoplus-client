import resolve from "rollup-plugin-node-resolve";

export default {
  input: "js-foreign/EventSource.js",
  output: {
    file: "js-foreign/out/es.js",
      format: "iife",
      name:"es"
  },
  plugins: [
    resolve({
      jsnext: true,
      main: true
    })
  ]
};
