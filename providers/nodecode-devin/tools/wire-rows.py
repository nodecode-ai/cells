#!/usr/bin/env python3
"""Write the devin cell's wire.json from an oh-my-pi checkout.

Usage: python3 -I tools/wire-rows.py OMP_CHECKOUT OUT_FILE

Development tooling only: the cell ships the file this writes and never runs
this script. models.json (tools/omp-models.py at the repository root) carries
what the catalog reads; this file carries what the wire reads and that tool
leaves out:

  models    per bundled model: whether it is a server-side router (AssignModel)
            and whether it takes parallel tool calls (packages/catalog/src/
            models.json, compat.modelRouter / supportsParallelToolCalls)
  families  omp's reviewed Devin variant families (compat/rules.json,
            taxonomy.collapse.variantFamilies, provider devin): one logical
            model over the per-effort wire uids Cascade serves
  aliases   omp's Devin selector aliases (taxonomy.collapse.providerAliases)
"""
import json
import os
import sys


def main(omp, out):
    catalog = os.path.join(omp, "packages/catalog/src")
    bundled = json.load(open(os.path.join(catalog, "models.json"))).get("devin", {})
    collapse = json.load(open(os.path.join(catalog, "compat/rules.json")))["taxonomy"]["collapse"]
    models = {}
    for model_id, row in sorted(bundled.items()):
        compat = row.get("compat") or {}
        models[model_id] = {
            "model_router": bool(compat.get("modelRouter")),
            "parallel_tool_calls": bool(compat.get("supportsParallelToolCalls")),
        }
    families = []
    for family in collapse["variantFamilies"]:
        if family.get("provider") != "devin":
            continue
        entry = {"id": family["id"], "name": family["name"], "members": family["members"],
                 "routing": family.get("routing") or {}}
        if family.get("defaultMember"):
            entry["default_member"] = family["defaultMember"]
        if not family.get("noThinking") and family.get("mode"):
            entry["efforts"] = family.get("efforts") or []
            if family.get("requiresEffort"):
                entry["requires_effort"] = True
            if family.get("defaultLevel"):
                entry["default_level"] = family["defaultLevel"]
        families.append(entry)
    rows = {"models": models, "families": families,
            "aliases": collapse.get("providerAliases", {}).get("devin", {})}
    with open(out, "w") as f:
        json.dump(rows, f, ensure_ascii=False, indent=1)
        f.write("\n")
    print(f"devin: {len(models)} models, {len(families)} families -> {out}")


if __name__ == "__main__":
    main(*sys.argv[1:3])
