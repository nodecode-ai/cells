#!/usr/bin/env python3
"""Write this cell's models.json from an oh-my-pi checkout.

Usage: python3 -I tools/bedrock-models.py OMP_CHECKOUT OUT_FILE

Development tooling only: the cell ships the file this writes and never runs
this script. The rows are omp's bundled amazon-bedrock ones
(packages/catalog/src/models.json), with the facts tools/omp-models.py at the
repository's root writes, and the ones the Converse request is shaped by,
which that tool does not carry: each model's thinking mode, whether it takes
a thinking display, whether its thinking is prefix-bound, its prompt-cache
policy, whether it takes a forced tool choice and sampling controls, and
whether its tool results carry images.
"""
import json
import os
import sys


def main(omp, out):
    bundled = json.load(open(os.path.join(omp, "packages/catalog/src/models.json")))
    rows = []
    for model_id, m in sorted(bundled.get("amazon-bedrock", {}).items()):
        if m.get("api") != "bedrock-converse-stream":
            continue
        cost = m.get("cost") or {}
        thinking = m.get("thinking") or {}
        compat = m.get("compat") or {}
        row = {
            "id": model_id, "name": m.get("name"), "api": m.get("api"), "base": m.get("baseUrl"),
            "context": m.get("contextWindow"), "output": m.get("maxTokens"),
            "reasoning": bool(m.get("reasoning")), "efforts": thinking.get("efforts"),
            "input": m.get("input"),
            "cost": {k: cost[k] for k in ("input", "output", "cacheRead", "cacheWrite") if k in cost} or None,
            "thinking_mode": thinking.get("mode"),
            "thinking_display": thinking.get("supportsDisplay"),
            "prefix_binding": thinking.get("prefixBinding"),
            "cache_mode": compat.get("promptCacheMode"),
            "cache_checkpoints": compat.get("promptCacheMaximumCheckpoints"),
            "long_cache": compat.get("supportsLongPromptCacheRetention"),
            "forced_tool_choice": compat.get("supportsForcedToolChoice"),
            "sampling": compat.get("supportsSamplingParams"),
            "hoist_images": m.get("requiresToolResultImageHoisting"),
        }
        rows.append({k: v for k, v in row.items() if v not in (None, [], {})})
    with open(out, "w") as f:
        json.dump(rows, f, ensure_ascii=False, indent=1)
        f.write("\n")
    print(f"amazon-bedrock: {len(rows)} Converse models -> {out}")


if __name__ == "__main__":
    main(*sys.argv[1:3])
