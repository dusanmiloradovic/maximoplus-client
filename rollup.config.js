import resolve from "rollup-plugin-node-resolve";
import commonjs from "rollup-plugin-commonjs";

export default {
  input: "repackage.js",
  output: {
    file: "rollup/index.js",
    format: "esm"
  },
  plugins: [
    resolve({
      jsnext: true,
      main: true
    }),
    commonjs({
      include: ["out/**"]
    })
  ]
};
