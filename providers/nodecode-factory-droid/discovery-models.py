#!/usr/bin/env python3
"""Write the factory-droid cell's models.json from an oh-my-pi checkout.

Usage: python3 -I discovery-models.py OMP_CHECKOUT OUT_FILE

Development tooling only: the cell ships the file this writes and never runs
this script. omp bundles no factory-droid rows (Factory has no model listing;
omp's discovery, packages/catalog/src/discovery/factory-droid.ts, builds the
roster per account from the registry in
packages/catalog/src/compat/rules/providers/factory-droid.kdl and narrows it
with live feature flags and org policy). These are the rows that discovery
yields with no live answer: no flags, no policy, the global region. So a
model behind a feature flag or an explicit opt-in is left out, as are models
the global region does not serve; a deprecation flag hides nothing offline.

Each row carries the facts the cell reads: the wire (from runtime/behavior.kdl
api-routes), the window and output ceiling, the effort ladder, the default
effort, whether thinking may be off, the upstreams that serve it in each
inference region in rotation order (the first is the default x-api-provider),
and the list price of the upstream provider's own bundled row.
"""
import json
import os
import re
import shlex
import sys

REGIONS = ("global", "us", "eu")


def words(line):
    """The KDL node on LINE as shell-like words, comments and braces dropped."""
    line = re.sub(r"(^|\s)//.*$", "", line).strip().rstrip("\\").strip().rstrip("{").strip()
    return shlex.split(line) if line else []


def seed_rows(lines):
    """id -> {name, input, context, output} from the seed block."""
    rows, current = {}, None
    for line in lines:
        w = words(line)
        if not w:
            continue
        if w[0] == "model":
            current = {"id": w[1], "name": w[2].split("=", 1)[1]}
            rows[w[1]] = current
        elif current and w[0] == "input":
            current["input"] = w[1:]
        elif current and w[0] == "limits":
            for word in w[1:]:
                key, value = word.split("=")
                current["context" if key == "context" else "output"] = int(value)
    return rows


def registry(lines):
    """id -> registry facts from the per-model `models "<id>" {` entries."""
    entries, current = {}, None
    for line in lines:
        w = words(line)
        if not w:
            continue
        if w[0] == "models" and len(w) == 2:
            current = entries.setdefault(w[1], {"regions": {}})
        elif w[0] == "on-api":
            break
        elif current is None:
            continue
        elif w[0] == "upstream-rotation":
            current["rotation"] = w[1:]
        elif w[0].startswith("region-upstreams-"):
            current["regions"][w[0][len("region-upstreams-"):]] = w[1:]
        elif w[0] == "list-price-from":
            current["price_from"] = w[1:]
        elif w[0] == "thinking-efforts":
            current["efforts"] = w[1:]
        elif w[0] == "thinking-requires-effort":
            current["requires_effort"] = w[1] == "#true"
        elif w[0] == "thinking-default-level":
            current["default_effort"] = w[1]
        elif w[0] == "default-reasoning-off":
            current["default_off"] = w[1] == "#true"
        elif w[0] == "entitlement":
            body = line[line.index("{") + 1:line.rindex("}")]
            for clause in body.split(";"):
                cw = shlex.split(clause.strip())
                if cw:
                    current[cw[0]] = cw[1] if len(cw) > 1 else True
        elif w[0] == "region-limits-eu":
            body = line[line.index("{") + 1:line.rindex("}")]
            limits = dict(shlex.split(c.strip()) for c in body.split(";") if c.strip())
            current["limits_eu"] = {"context": int(limits["context-window"]),
                                    "output": int(limits["max-tokens"])}
    return entries


def region_tables(text):
    """region -> the upstreams that region serves."""
    tables = {}
    for region in REGIONS:
        match = re.search(r"^\s*region-upstreams-%s((?:\s+\"[^\"]+\"|\s*\\\s*\n)+)" % region, text, re.M)
        tables[region] = re.findall(r'"([^"]+)"', match.group(1))
    return tables


def routes(behavior):
    """id -> omp wire name, from api-routes provider="factory-droid"."""
    block = behavior[behavior.index('api-routes provider="factory-droid"'):]
    block = block[:block.index("\n\t}")]
    wires = {}
    for wire, body in re.findall(r'route "([^"]+)"((?:[^\n]*\\\n)*[^\n]*)', block):
        for model in re.findall(r'exact="([^"]+)"', body):
            wires[model] = wire
    return wires


def main(omp, out):
    rules = os.path.join(omp, "packages/catalog/src/compat/rules")
    text = open(os.path.join(rules, "providers/factory-droid.kdl")).read()
    behavior = open(os.path.join(rules, "runtime/behavior.kdl")).read()
    bundled = json.load(open(os.path.join(omp, "packages/catalog/src/models.json")))
    lines = text.splitlines()
    seed_end = next(i for i, l in enumerate(lines) if l.strip().startswith("region-upstreams-global"))
    seeds = seed_rows(lines[:seed_end])
    entries = registry(lines[seed_end:])
    tables = region_tables(text)
    wires = routes(behavior)
    rows = []
    for model_id, seed in seeds.items():
        entry = entries.get(model_id, {"regions": {}})
        if entry.get("feature-flag") or entry.get("requires-explicit-opt-in"):
            continue
        rotation = entry.get("rotation", [])
        upstreams = {}
        for region in REGIONS:
            allowed = entry["regions"].get(region, tables[region])
            upstreams[region] = [u for u in rotation if u in allowed]
        if not upstreams["global"]:
            continue
        cost = None
        if entry.get("price_from"):
            source, *named = entry["price_from"]
            listed = bundled.get(source, {}).get(named[0] if named else model_id)
            if listed and listed.get("cost"):
                cost = {k: listed["cost"][k] for k in ("input", "output", "cacheRead", "cacheWrite")
                        if k in listed["cost"]}
        row = {
            "id": model_id, "name": seed["name"], "api": wires.get(model_id, "openai-completions"),
            "context": seed["context"], "output": seed["output"], "reasoning": True,
            "efforts": entry.get("efforts"), "default_effort": entry.get("default_effort"),
            "default_off": entry.get("default_off") or None,
            "requires_effort": entry.get("requires_effort"),
            "input": seed["input"], "cost": cost, "upstreams": upstreams,
            "limits_eu": entry.get("limits_eu"),
        }
        rows.append({k: v for k, v in row.items() if v is not None})
    with open(out, "w") as f:
        json.dump(rows, f, ensure_ascii=False, indent=1)
        f.write("\n")
    print(f"factory-droid: {len(rows)} models -> {out}")


if __name__ == "__main__":
    main(*sys.argv[1:3])
