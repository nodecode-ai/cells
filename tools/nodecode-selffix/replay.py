# replay.py --- the inside of a selffix test, copied into each test file.
#
# SPDX-License-Identifier: MIT
#
# It runs in the sandbox. Serves the recorded assistant messages
# from a loopback provider, drives one turn of the organism through `nodecode rpc`,
# and judges whether the recorded failure happened again.
# exit 0: it did not (the test passes), 1: it did (the test fails), 2: could not run.
import json, os, re, socket, subprocess, sys, threading, time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

replay = json.load(open(sys.argv[1]))
NC, HOME, WORK = sys.argv[2], os.environ['NODECODE_HOME'], sys.argv[3]
LIMIT = int(os.environ.get('SELFFIX_TIMEOUT', '240'))
messages, served, side = list(replay.get('messages') or []), [0], [0]

NORMALIZATIONS = [(r'#<[^>]*>', '#<..>'),
                  (r'(?<![\w.])(?:~|\.\.?)?/(?:[\w.@%+-]+/?)+', 'PATH'),
                  (r'\b[a-z][a-z0-9]*(?:-[0-9A-Z]{6,})+\b', 'ID'),
                  (r'(?i)\b[0-9a-f]{8,}\b', 'N'),
                  (r'\d+(?:\.\d+)?', 'N'),
                  (r'\s+', ' ')]

def normalize(text, cap=160):
    s = text or ''
    for pattern, repl in NORMALIZATIONS:
        s = re.sub(pattern, repl, s)
    return s.strip(' ')[:cap]

def say(line):
    print(line, flush=True)

class Provider(BaseHTTPRequestHandler):
    protocol_version = 'HTTP/1.1'
    def log_message(self, *a): pass
    def _json(self, code, obj):
        body = json.dumps(obj).encode()
        self.send_response(code); self.send_header('content-type', 'application/json')
        self.send_header('content-length', str(len(body))); self.end_headers(); self.wfile.write(body)
    def do_GET(self):
        if self.path.rstrip('/').endswith('/models'):
            return self._json(200, {'object': 'list', 'data': [{'id': 'replay', 'object': 'model'}]})
        self._json(404, {'error': {'message': 'no such path'}})
    def do_POST(self):
        n = int(self.headers.get('content-length') or 0)
        req = json.loads(self.rfile.read(n) or b'{}')
        if req.get('tools') and served[0] < len(messages):
            message = messages[served[0]]; served[0] += 1
        else:
            if not req.get('tools'): side[0] += 1
            message = {'role': 'assistant', 'content': 'replay: no more recorded messages'}
        self._answer(req, message)
    def _answer(self, req, message):
        content = message.get('content') or ''
        calls = message.get('tool_calls') or []
        finish = 'tool_calls' if calls else 'stop'
        usage = {'prompt_tokens': 10, 'completion_tokens': 5, 'total_tokens': 15}
        if not req.get('stream'):
            out = {'role': 'assistant', 'content': content or None}
            if calls: out['tool_calls'] = calls
            return self._json(200, {'id': 'replay', 'object': 'chat.completion', 'model': 'replay',
                                    'choices': [{'index': 0, 'message': out, 'finish_reason': finish}], 'usage': usage})
        self.send_response(200); self.send_header('content-type', 'text/event-stream')
        self.send_header('cache-control', 'no-cache'); self.send_header('connection', 'close'); self.end_headers()
        base = {'id': 'replay', 'object': 'chat.completion.chunk', 'model': 'replay'}
        def ev(delta, **more):
            obj = dict(base, choices=[dict({'index': 0, 'delta': delta}, **more)])
            self.wfile.write(b'data: ' + json.dumps(obj).encode() + b'\n\n'); self.wfile.flush()
        ev({'role': 'assistant', 'content': content})
        for i, call in enumerate(calls):
            ev({'tool_calls': [{'index': i, 'id': call.get('id') or 'call_%d' % i, 'type': 'function',
                                'function': {'name': call['function']['name'],
                                             'arguments': call['function'].get('arguments') or '{}'}}]})
        self.wfile.write(b'data: ' + json.dumps(dict(base, choices=[{'index': 0, 'delta': {}, 'finish_reason': finish}],
                                                     usage=usage)).encode() + b'\n\n')
        self.wfile.write(b'data: [DONE]\n\n'); self.wfile.flush()
        self.close_connection = True

def sealed():
    keys = [k for k in os.environ if re.search(r'KEY|TOKEN|SECRET|PASSW|CREDENTIAL', k, re.I)]
    try:
        socket.create_connection(('1.1.1.1', 443), timeout=2).close(); net = 'reachable'
    except OSError:
        net = 'unreachable'
    probe = os.environ.get('SELFFIX_PROBE', '')
    home = 'visible' if probe and os.path.exists(probe) else 'hidden'
    say('sealed: %d environment names, keys among them: %s; network: %s; the real home: %s'
        % (len(os.environ), ','.join(keys) or 'none', net, home))
    return not keys and net == 'unreachable' and home == 'hidden'

def find_end(obj):
    """The turn-ending fact inside a gateway frame, as (kind, payload), or None."""
    if isinstance(obj, dict):
        kind = obj.get('kind') or obj.get('type')
        if kind in ('turn.completed', 'turn.failed', 'turn.cancelled'):
            return kind, obj.get('payload') if isinstance(obj.get('payload'), dict) else obj
        for v in obj.values():
            hit = find_end(v)
            if hit: return hit
    elif isinstance(obj, list):
        for v in obj:
            hit = find_end(v)
            if hit: return hit
    return None

def main():
    if not sealed():
        say('not sealed: refusing to replay'); return 2
    server = ThreadingHTTPServer(('127.0.0.1', 0), Provider)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    port = server.server_address[1]
    with open(os.path.join(HOME, 'config.jsonc'), 'w') as f:
        json.dump({'providers': {'replay': {'sdk': 'openai-completions', 'base_url': 'http://127.0.0.1:%d/v1' % port,
                                            'models': ['replay']}},
                   'general': {'default_model': {'provider': 'replay', 'model': 'replay'}}}, f)
    expect = replay['expect']
    say('replaying turn %s of session %s: %d recorded message(s); expecting %s: %s'
        % (replay.get('turn'), replay.get('session'), len(messages), expect['condition_type'], expect['detail']))
    child = subprocess.Popen([NC, '--ephemeral', 'rpc'], cwd=WORK, stdin=subprocess.PIPE,
                             stdout=subprocess.PIPE, stderr=open(os.path.join(HOME, 'organism.log'), 'w'), text=True)
    child.stdin.write(json.dumps({'type': 'start_turn', 'request': {'prompt': replay['input']}}) + '\n'); child.stdin.flush()
    end, deadline = None, time.time() + LIMIT
    timer = threading.Timer(LIMIT, child.kill); timer.start()
    frames = open(os.path.join(HOME, 'frames.jsonl'), 'w')
    for line in child.stdout:
        frames.write(line)
        try:
            end = find_end(json.loads(line))
        except ValueError:
            continue
        if end: break
    timer.cancel()
    try:
        child.stdin.close(); child.wait(timeout=30)
    except Exception:
        child.kill()
    if not end:
        say('could not run: the organism ended no turn within %ds (exit %s)' % (LIMIT, child.returncode))
        say(open(os.path.join(HOME, 'organism.log')).read()[-1500:]); return 2
    kind, payload = end
    say('the turn ended with %s after %d of %d recorded message(s)' % (kind, served[0], len(messages)))
    if kind != 'turn.failed':
        say('NOT REPRODUCED: the recorded failure did not happen'); return 0
    ctype = str(payload.get('condition-type') or payload.get('condition_type') or '')
    detail = str(payload.get('detail') or '')
    say('failed with %s: %s' % (ctype, detail[:300]))
    if ctype == expect['condition_type'] and normalize(detail) == expect['detail_normal']:
        say('REPRODUCED: the turn fails as recorded'); return 1
    say('NOT REPRODUCED: a different failure (%s: %s)' % (ctype, normalize(detail))); return 0

sys.exit(main())
