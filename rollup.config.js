import resolve from "rollup-plugin-node-resolve";
import commonjs from "rollup-plugin-commonjs";

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
