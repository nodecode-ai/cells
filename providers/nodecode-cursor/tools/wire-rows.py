#!/usr/bin/env python3
"""Write nodecode-cursor/wire.json from an oh-my-pi checkout.

Usage: python3 -I nodecode-cursor/tools/wire-rows.py OMP_CHECKOUT OUT_FILE

Development tooling only: the cell ships the file this writes and never runs
this script. models.json (tools/omp-models.py) carries what the catalog
reads; this file carries what the Cursor wire reads of omp's bundled rows
(packages/catalog/src/models.json, provider "cursor"):

  id            the public model id
  request       requestModelId, the wire id a round sends when no effort
                routes it elsewhere
  routing       thinking.effortRouting, effort -> wire id ("off" included)
  max_mode      cursorMaxMode, when the row states it
  max_routes    cursorMaxModeRoutes, wire id -> max mode
  projection    requiresCursorToolSchemaProjection
  class/family  the model's identity (omp's taxonomy classification)
"""
import json
import os
import sys


def main(omp, out):
    bundled = json.load(open(os.path.join(omp, "packages/catalog/src/models.json")))
    rows = []
    for model_id, m in sorted(bundled.get("cursor", {}).items()):
        if m.get("api") != "cursor-agent":
            continue
        identity = m.get("identity") or {}
        row = {
            "id": model_id,
            "request": m.get("requestModelId"),
            "routing": (m.get("thinking") or {}).get("effortRouting"),
            "max_mode": m.get("cursorMaxMode"),
            "max_routes": m.get("cursorMaxModeRoutes"),
            "projection": True if m.get("requiresCursorToolSchemaProjection") else None,
            "class": identity.get("class"),
            "family": identity.get("family"),
        }
        rows.append({k: v for k, v in row.items() if v is not None})
    with open(out, "w") as f:
        json.dump(rows, f, ensure_ascii=False, indent=1)
        f.write("\n")
    print(f"cursor: {len(rows)} wire rows -> {out}")


if __name__ == "__main__":
    main(*sys.argv[1:3])
