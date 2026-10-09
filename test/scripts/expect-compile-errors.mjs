import {spawnSync} from "node:child_process";
import {dirname, join} from "node:path";
import {fileURLToPath} from "node:url";

const scriptDir = dirname(fileURLToPath(import.meta.url));
const testRoot = dirname(scriptDir);
const binName = process.platform === "win32" ? "rescript.cmd" : "rescript";
const rescriptBin = join(testRoot, "node_modules", ".bin", binName);

// Each fixture must fail to compile with its message.
const fixtures = {
  "invalid-as-payload":
    "@spice.as is only supported on constructors without payload",
  "serde-tag-tuple-payload":
    "An internally tagged (@tag) @spice.serde constructor can't have several payload values",
};

let failed = false;

for (const [fixture, expected] of Object.entries(fixtures)) {
  const result = spawnSync(rescriptBin, [], {
    cwd: join(testRoot, "fixtures", fixture),
    encoding: "utf8",
  });
  const output = `${result.stdout ?? ""}${result.stderr ?? ""}`;

  if (result.status === 0) {
    console.error(`compile-fail: ${fixture} compiled, expected an error.`);
    failed = true;
  } else if (!output.includes(expected)) {
    console.error(`compile-fail: ${fixture} failed with unexpected output.`);
    console.error(output);
    failed = true;
  } else {
    console.log(`compile-fail: ${fixture} rejected`);
  }
}

process.exit(failed ? 1 : 0);
