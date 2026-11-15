# What is vendored here, and from where

`scan/` and `iocs/` are archivegenocide's own scanner, copied unmodified.

- Repository: https://github.com/interplains/Zero-Breach-Toolkit
- Commit: `2525377788feb8335052ba08981f37239091307a` (2026-11-14T20:15:48Z, "Zero Breach Toolkit 3.0.2")
- Licence: Apache-2.0 (see ../NOTICE and ../LICENSE)
- Taken: whole, with `--exclude __pycache__`, and two folders left out that a scan never reads:
  `scan/tests/` (the vendor's own pytest suite) and the repository's `web/`, `dist/`, `build/`,
  `locales/`, `third_party/` and `skills/` (the download build, the browser page, the Windows
  helper packaging and the desktop app).
- Size: 7.8 MB, mostly the indicator tables and the 9,014 browser-extension fingerprints.

Nothing here was edited. A fix belongs upstream first; re-vendoring is one `rsync` from the
commit above. The scan needs nothing but Python 3.8 or newer and its standard library; the
optional `--patterns` check is the one part that wants the pinned YARA-X wheel installed in
that same interpreter.

Indicators are facts published by researchers; the citations for each are in the files'
own headers under `iocs/`.
