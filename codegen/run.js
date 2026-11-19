import fs from "node:fs";
import path from "node:path";
import process from "node:process";
import elmCodeGen from "elm-codegen";

const codegenDir = path.join(process.cwd(), "codegen");

// Pinned to a specific byzhtml commit so codegen output is reproducible.
// To pick up upstream glyph changes, bump this SHA and re-run `npm run codegen`.
const BYZHTML_COMMIT = "31a8bf061225f252c6ebe568bb88f4ec09e97a16";
const GLYPHNAMES_URL = `https://raw.githubusercontent.com/neanes/byzhtml/${BYZHTML_COMMIT}/assets/fonts/sbmufl/glyphnames.json`;

const neumesList = fs.readFileSync(
  path.join(codegenDir, "component-list-neumes.md"),
  "utf8",
);

const response = await fetch(GLYPHNAMES_URL);
if (!response.ok) {
  throw new Error(
    `Failed to fetch glyphnames.json (${response.status} ${response.statusText}): ${GLYPHNAMES_URL}`,
  );
}
const glyphnames = await response.json();

elmCodeGen.run("Generate.elm", {
  debug: "debug", // remove to run in optimize mode. Debug needed for debug statements.
  output: "generated",
  flags: { neumesList, glyphnames },
  cwd: "./codegen",
});
