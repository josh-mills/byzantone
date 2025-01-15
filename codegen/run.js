const fs = require('fs');
const path = require('path');
const process = require('process');
const elmCodeGen = require("elm-codegen");

const neumesList = path.join(process.cwd(), 'codegen', 'component-list-neumes.md');

fs.readFile(neumesList, 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  } else {
    elmCodeGen.run("Generate.elm", {
      debug: "debug", // remove to run in optimize mode. Debug needed for debug statements.
      output: "generated",
      // flags: JSON.parse(data),
      flags: data,
      cwd: "./codegen",
    });
  }
});