import RNEventSource from "react-native-event-source";

var gl = typeof global === "undefined" ? window : global;

gl.RNEventSource = RNEventSource;
