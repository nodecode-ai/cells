# drive.py --- one turn of the home's organism through `nodecode rpc`. SPDX-License-Identifier: MIT
#
# usage: drive.py NODECODE PROMPT [SECONDS]. Prints one JSON line: how the turn
# ended, its failure (condition-type, detail) and every tool result it carried.
import json, subprocess, sys, threading

nc, prompt = sys.argv[1], sys.argv[2]
limit = int(sys.argv[3]) if len(sys.argv) > 3 else 300
child = subprocess.Popen([nc, 'rpc'], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                         stderr=subprocess.DEVNULL, text=True)
child.stdin.write(json.dumps({'type': 'start_turn', 'request': {'prompt': prompt}}) + '\n'); child.stdin.flush()
timer = threading.Timer(limit, child.kill); timer.start()
out = {'end': None, 'condition_type': None, 'detail': None, 'tool_results': []}

def walk(obj):
    if isinstance(obj, dict):
        kind = obj.get('kind') or obj.get('type')
        payload = obj.get('payload') if isinstance(obj.get('payload'), dict) else None
        if kind == 'turn.tool_result' and payload is not None:
            out['tool_results'].append(str(payload.get('result')))
        if kind in ('turn.completed', 'turn.failed', 'turn.cancelled'):
            out['end'] = kind
            if payload is not None:
                out['condition_type'] = payload.get('condition-type')
                out['detail'] = payload.get('detail')
            return True
        return any(walk(v) for v in obj.values())
    if isinstance(obj, list):
        return any(walk(v) for v in obj)
    return False

for line in child.stdout:
    try:
        if walk(json.loads(line)): break
    except ValueError:
        pass
timer.cancel()
child.stdin.close()
try: child.wait(timeout=60)
except Exception: child.kill()
print(json.dumps(out))
