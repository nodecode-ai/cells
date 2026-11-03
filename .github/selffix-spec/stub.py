# stub.py --- the selffix spec's loopback provider. SPDX-License-Identifier: MIT
#
# usage: stub.py PORT SCRIPT. Each request that offers tools takes the next
# message of SCRIPT (a JSON list, re-read per request, so the spec can swap it
# between runs); past the script, and for side requests, it answers "done".
import json, sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

PORT, SCRIPT = int(sys.argv[1]), sys.argv[2]
state = {'script': None, 'served': 0}

class H(BaseHTTPRequestHandler):
    protocol_version = 'HTTP/1.1'
    def log_message(self, *a): pass
    def _json(self, code, obj):
        body = json.dumps(obj).encode()
        self.send_response(code); self.send_header('content-type', 'application/json')
        self.send_header('content-length', str(len(body))); self.end_headers(); self.wfile.write(body)
    def do_GET(self):
        if self.path.rstrip('/').endswith('/models'):
            return self._json(200, {'object': 'list', 'data': [{'id': 'stub', 'object': 'model'}]})
        self._json(404, {'error': {'message': 'no'}})
    def do_POST(self):
        n = int(self.headers.get('content-length') or 0)
        req = json.loads(self.rfile.read(n) or b'{}')
        script = json.load(open(SCRIPT))
        if script != state['script']:
            state['script'], state['served'] = script, 0
        message = {'role': 'assistant', 'content': 'done'}
        if req.get('tools') and state['served'] < len(script):
            message = script[state['served']]; state['served'] += 1
        sys.stderr.write('served %d: %s\n' % (state['served'], json.dumps(message)[:200])); sys.stderr.flush()
        if message.get('stub_status'):
            return self._json(message['stub_status'], {'error': {'message': 'Insufficient account balance'}})
        calls = message.get('tool_calls') or []
        finish = 'tool_calls' if calls else 'stop'
        self.send_response(200); self.send_header('content-type', 'text/event-stream')
        self.send_header('cache-control', 'no-cache'); self.send_header('connection', 'close'); self.end_headers()
        base = {'id': 'stub', 'object': 'chat.completion.chunk', 'model': 'stub'}
        def ev(obj):
            self.wfile.write(b'data: ' + json.dumps(dict(base, **obj)).encode() + b'\n\n'); self.wfile.flush()
        ev({'choices': [{'index': 0, 'delta': {'role': 'assistant', 'content': message.get('content') or ''}}]})
        for i, c in enumerate(calls):
            ev({'choices': [{'index': 0, 'delta': {'tool_calls': [{'index': i, 'id': c['id'], 'type': 'function',
                                                                     'function': c['function']}]}}]})
        ev({'choices': [{'index': 0, 'delta': {}, 'finish_reason': finish}],
            'usage': {'prompt_tokens': 10, 'completion_tokens': 5, 'total_tokens': 15}})
        self.wfile.write(b'data: [DONE]\n\n'); self.wfile.flush()
        self.close_connection = True

ThreadingHTTPServer(('127.0.0.1', PORT), H).serve_forever()
