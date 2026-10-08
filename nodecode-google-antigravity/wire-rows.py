#!/usr/bin/env python3
"""Write a Cloud Code Assist cell's wire.json from an oh-my-pi checkout.

Usage: python3 -I wire-rows.py OMP_CHECKOUT PROVIDER_ID OUT_FILE

Development tooling only: the cell ships the file this writes and never runs
this script. models.json (tools/omp-models.py) carries what the catalog reads;
this carries what the wire reads and omp-models.py leaves out: the id the
request names, the model's lineage, how it thinks (level or budget, the
per-effort wire ids and budgets, whether off must be said), and the compat
flags omp's Cloud Code Assist request builder branches on.
"""
import json
import os
import sys

COMPAT = {
    "supportsFunctionPartId": "function_part_id",
    "requiresSkipThoughtSignature": "skip_signature",
    "requiresSkipThoughtSignatureOnFirstFunctionCall": "skip_signature_first_call",
    "dropUnsignedThinking": "drop_unsigned_thinking",
    "ccaLegacyParametersSchema": "legacy_parameters",
    "multimodalFunctionResponse": "multimodal_function_response",
    "flashStreamLeakWorkaround": "flash_leak",
    "claudeThinkingBetaHeader": "claude_thinking_beta",
    "antigravityClaudeToolMode": "claude_tool_mode",
    "antigravityUsageLabel": "usage_label",
    "supportsSamplingParams": "sampling",
}


def main(omp, provider, out):
    bundled = json.load(open(os.path.join(omp, "packages/catalog/src/models.json")))
    rows = []
    for model_id, m in sorted(bundled.get(provider, {}).items()):
        if m.get("api") != "google-gemini-cli":
            continue
        thinking = m.get("thinking") or {}
        compat = m.get("compat") or {}
        row = {
            "id": model_id,
            "request_model_id": m.get("requestModelId"),
            "class": (m.get("identity") or {}).get("class"),
            "reasoning": bool(m.get("reasoning")),
            "output": m.get("maxTokens"),
            "images": "image" in (m.get("input") or []),
            "tools": m.get("supportsTools", True) is not False and m.get("kind") != "image",
            "thinking": {k: v for k, v in {
                "mode": thinking.get("mode"),
                "efforts": thinking.get("efforts"),
                "routing": thinking.get("effortRouting"),
                "budgets": thinking.get("effortBudgets"),
                "suppress_when_off": thinking.get("suppressWhenOff"),
                "requires_effort": thinking.get("requiresEffort"),
            }.items() if v not in (None, [], {})} or None,
            "compat": {ours: compat[theirs] for theirs, ours in COMPAT.items() if theirs in compat},
        }
        rows.append({k: v for k, v in row.items() if v not in (None, [], {})})
    with open(out, "w") as f:
        json.dump(rows, f, ensure_ascii=False, indent=1)
        f.write("\n")
    print(f"{provider}: {len(rows)} wire rows -> {out}")


if __name__ == "__main__":
    main(*sys.argv[1:4])
