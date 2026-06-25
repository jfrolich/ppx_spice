---
"@jfrolich/ppx-spice": patch
---

Ship a linux-arm64 prebuilt `ppx` binary. The `linux` install path serves the x86-64 `ppx-linux.exe` regardless of architecture, so arm64 Linux (CI runners, Docker on Apple Silicon) hits "Exec format error". `postInstall.js` now selects the arm64 binary when `process.arch === "arm64"`.
