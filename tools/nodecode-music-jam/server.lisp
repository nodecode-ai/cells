;;;; server.lisp --- the band's own page, on 127.0.0.1 and nowhere else.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original opened a loopback HTTP service with a random token, served
;;;; its page and answered the browser's requests from it. This does the same
;;;; with no dependency beyond sb-bsd-sockets, which the Nodecode image
;;;; already loads: one listener on 127.0.0.1, one thread a connection, one
;;;; token in the URL's fragment that every API call must carry.

(in-package #:nodecode-music-jam)

(defstruct (band-server (:constructor %make-server))
  socket port token jam thread (stopping nil))

(defparameter *max-body* 8000000
  "The largest request body the service reads, as the original's ceiling.")

;;; --- the page --------------------------------------------------------------------

(defparameter *page* (concatenate 'string
  "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  "<title>Music Jam</title><style>"
  "body{margin:0;background:#12100e;color:#efe6d9;font:16px/1.5 ui-sans-serif,system-ui,sans-serif}"
  "header{display:flex;gap:1rem;align-items:center;padding:1rem 1.5rem;border-bottom:1px solid #332c25}"
  "h1{font-size:1rem;font-weight:600;margin:0;letter-spacing:.08em;text-transform:uppercase}"
  "button{background:#1d1915;color:#efe6d9;border:1px solid #4a3f34;border-radius:.5rem;padding:.5rem 1rem;cursor:pointer}"
  "button:hover{border-color:#c8a06a}main{padding:1.5rem;display:grid;gap:1rem;grid-template-columns:repeat(auto-fit,minmax(14rem,1fr))}"
  "section{background:#1a1613;border:1px solid #2c251f;border-radius:.75rem;padding:1rem}"
  "h2{font-size:.75rem;letter-spacing:.12em;text-transform:uppercase;color:#c8a06a;margin:0 0 .5rem}"
  ".bar{color:#8a7d6d;font-size:.8rem}.keys{color:#8a7d6d;font-size:.8rem;margin-top:1rem}"
  "</style></head><body><header><h1>Music Jam</h1>"
  "<button id=\"play\">Play</button><button id=\"stop\">Stop</button>"
  "<span class=\"bar\" id=\"clock\">stopped</span></header>"
  "<main id=\"roles\"></main><p class=\"keys\" style=\"padding:0 1.5rem 1.5rem\">"
  "A S D F G H J K play the lead voice: the phrase is kept for the MIDI export.</p>"
  "<script type=\"module\" src=\"/app.js\"></script></body></html>")
  "The band's page: four musicians, a transport, and the keyboard's line.")

(defparameter *script* (concatenate 'string
  "const token = location.hash.slice(1);\n"
  "const clockline = document.getElementById('clock');\n"
  "let ctx = null, nextBarTime = 0, nextBar = 0, playing = false, buffer = [], owner = Math.random().toString(36).slice(2);\n"
  "const rolesBox = document.getElementById('roles');\n"
  "async function api(action, body = {}) {\n"
  "  const r = await fetch('/api/' + action, {method:'POST',headers:{'Content-Type':'application/json','Authorization':'Bearer ' + token},body:JSON.stringify(body)});\n"
  "  return await r.json();\n"
  "}\n"
  "function paint(status) {\n"
  "  if (!status || !status.session) return;\n"
  "  rolesBox.innerHTML = '';\n"
  "  for (const [role, state] of Object.entries(status.session.roles)) {\n"
  "    const box = document.createElement('section');\n"
  "    box.innerHTML = '<h2>' + role + '</h2><p>' + (state.intention || '') + '</p>' +\n"
  "      '<p class=\"bar\">density ' + (state.controls.density ?? 0).toFixed(2) + '  energy ' + (state.controls.energy ?? 0).toFixed(2) + '</p>';\n"
  "    rolesBox.appendChild(box);\n"
  "  }\n"
  "}\n"
  "function freq(pitch) { return 440 * Math.pow(2, (pitch - 69) / 12); }\n"
  "function noise(dur) {\n"
  "  const b = ctx.createBuffer(1, Math.max(1, ctx.sampleRate * dur), ctx.sampleRate);\n"
  "  const d = b.getChannelData(0);\n"
  "  for (let i = 0; i < d.length; i++) d[i] = Math.random() * 2 - 1;\n"
  "  return b;\n"
  "}\n"
  "function drum(when, pitch) {\n"
  "  const g = ctx.createGain(); g.connect(ctx.destination);\n"
  "  if (pitch === 36) { const o = ctx.createOscillator(); o.frequency.setValueAtTime(120, when);\n"
  "    o.frequency.exponentialRampToValueAtTime(45, when + 0.16); o.connect(g);\n"
  "    g.gain.setValueAtTime(0.9, when); g.gain.exponentialRampToValueAtTime(0.001, when + 0.18); o.start(when); o.stop(when + 0.2); return; }\n"
  "  const src = ctx.createBufferSource(); src.buffer = noise(pitch === 38 ? 0.18 : 0.06); src.connect(g);\n"
  "  g.gain.setValueAtTime(pitch === 38 ? 0.5 : 0.22, when);\n"
  "  g.gain.exponentialRampToValueAtTime(0.001, when + (pitch === 38 ? 0.18 : 0.05));\n"
  "  src.start(when);\n"
  "}\n"
  "function tone(when, pitch, seconds, velocity) {\n"
  "  const o = ctx.createOscillator(), g = ctx.createGain();\n"
  "  o.type = 'triangle'; o.frequency.value = freq(pitch); o.connect(g); g.connect(ctx.destination);\n"
  "  g.gain.setValueAtTime(0.0001, when); g.gain.exponentialRampToValueAtTime(0.18 * velocity + 0.02, when + 0.01);\n"
  "  g.gain.exponentialRampToValueAtTime(0.0001, when + seconds);\n"
  "  o.start(when); o.stop(when + seconds + 0.05);\n"
  "}\n"
  "function scheduleBar(record, when) {\n"
  "  const beat = 60 / record.tempo;\n"
  "  for (const [role, state] of Object.entries(record.roles)) {\n"
  "    for (const e of state.pattern) {\n"
  "      const at = when + e.beat * beat;\n"
  "      if (role === 'drums') drum(at, e.pitch); else tone(at, e.pitch, e.duration * beat, e.velocity);\n"
  "    }\n"
  "  }\n"
  "  return when + 4 * beat;\n"
  "}\n"
  "async function pull() {\n"
  "  const answer = await api('bars', {owner, start: nextBar, count: 2});\n"
  "  if (answer.error) { clockline.textContent = answer.error; playing = false; return; }\n"
  "  buffer.push(...answer.bars); nextBar += 2;\n"
  "}\n"
  "async function tick() {\n"
  "  if (!playing) return;\n"
  "  while (nextBarTime < ctx.currentTime + 0.6) {\n"
  "    if (!buffer.length) { await pull(); if (!buffer.length) break; }\n"
  "    const record = buffer.shift();\n"
  "    nextBarTime = scheduleBar(record, Math.max(nextBarTime, ctx.currentTime + 0.05));\n"
  "    api('heartbeat', {owner, clock:{bar: record.bar, beat: 0, 'audio-time': ctx.currentTime, 'late-events': 0}}).then(paint).catch(() => {});\n"
  "    clockline.textContent = 'bar ' + (record.bar + 1) + ' of ' + record.tempo + ' bpm';\n"
  "  }\n"
  "  setTimeout(tick, 50);\n"
  "}\n"
  "async function play() {\n"
  "  if (!ctx) ctx = new (window.AudioContext || window.webkitAudioContext)();\n"
  "  const started = await api('start', {owner});\n"
  "  if (started.error) { clockline.textContent = started.error; return; }\n"
  "  nextBar = started['next-bar']; buffer = []; nextBarTime = ctx.currentTime + 0.1;\n"
  "  playing = true; tick(); paint(await api('status', {}));\n"
  "}\n"
  "async function stop() { playing = false; buffer = []; await api('stop', {}); clockline.textContent = 'stopped'; }\n"
  "document.getElementById('play').onclick = play;\n"
  "document.getElementById('stop').onclick = stop;\n"
  "const scale = [62, 64, 65, 67, 69, 72, 74, 76];\n"
  "'asdfghjk'.split('').forEach(function (key, i) {\n"
  "  addEventListener('keydown', function (event) {\n"
  "    if (event.key !== key || event.repeat) return;\n"
  "    if (ctx) tone(ctx.currentTime, scale[i], 0.4, 0.5);\n"
  "    const bar = Math.max(0, nextBar - buffer.length);\n"
  "    api('human', {events: [{bar: bar, beat: 0, pitch: scale[i], duration: 0.5, velocity: 0.6}]}).catch(() => {});\n"
  "  });\n"
  "});\n"
  "api('status', {}).then(paint).catch(() => { clockline.textContent = 'open this page from a jam_start link'; });\n")
  "The band's page script: Web Audio playback in bar order, the transport, and the keyboard.")

;;; --- one request -----------------------------------------------------------------

(defun body-json (octets)
  "OCTETS, a request body, as a JSON value."
  (when (plusp (length octets))
    (nlk:decode-json (sb-ext:octets-to-string octets :external-format :utf-8) :whole t)))

(defun jsonable (value)
  "VALUE as what the JSON writer makes of it: a plist or an alist an object,
a list an array. The band keeps its state as plists, and the page reads
objects."
  (cond
    ((null value) nil)
    ((hash-table-p value) value)
    ((and (consp value) (keywordp (first value)))
     (let ((object (nlk:json-object)))
       (loop for (key item) on value by #'cddr
             do (setf (gethash (string-downcase (subst #\_ #\- (symbol-name key))) object)
                      (jsonable item)))
       object))
    ((and (consp value) (every (lambda (item) (and (consp item) (stringp (car item)))) value))
     (let ((object (nlk:json-object)))
       (dolist (pair value)
         (setf (gethash (string-downcase (subst #\_ #\- (car pair))) object)
               (jsonable (cdr pair))))
       object))
    ((consp value) (coerce (mapcar #'jsonable value) 'vector))
    (t value)))

(defun reply (status mime text)
  "STATUS, MIME and TEXT as the octets a browser reads."
  (values status mime (sb-ext:string-to-octets text :external-format :utf-8)))

(defun header (headers name)
  "HEADERS, an alist of lowercase names, at NAME."
  (cdr (assoc name headers :test #'equal)))

(defun json-reply (status value)
  "STATUS carrying VALUE as JSON, or the refusal it becomes."
  (reply status "application/json; charset=utf-8"
         (sb-ext:octets-to-string (nlk:encode-json-octets (jsonable value))
                                  :external-format :utf-8)))

(defun action-of (path)
  "PATH's action: /api/bars answers \"bars\"."
  (if (uiop:string-prefix-p "/api/" path) (subseq path 5) ""))

(defun handle (server method path headers body)
  "One request against SERVER => (values STATUS MIME OCTETS)."
  (let ((jam (band-server-jam server))
        (port (band-server-port server)))
    (cond
      ((and (equal method "GET") (equal path "/"))
       (reply 200 "text/html; charset=utf-8" *page*))
      ((and (equal method "GET") (equal path "/app.js"))
       (reply 200 "text/javascript; charset=utf-8" *script*))
      ((and (equal method "GET") (equal path "/style.css"))
       (reply 200 "text/css; charset=utf-8" ""))
      ((not (equal method "POST"))
       (reply 404 "application/json; charset=utf-8" "{\"error\":\"not found\"}"))
      ((not (equal (header headers "host")
                   (format nil "127.0.0.1:~d" port)))
       (json-reply 403 (list :error "invalid host")))
      ((not (equal (header headers "authorization")
                   (format nil "Bearer ~a" (band-server-token server))))
       (json-reply 401 (list :error "unauthorized")))
      (t
       (handler-case
           (let ((action (action-of path))
                 (data (body-json body)))
             (cond
               ((equal action "status") (json-reply 200 (status jam)))
               ((equal action "start")
                (json-reply 200 (start-band jam (nlk:json-value data :text "owner"))))
               ((equal action "stop") (json-reply 200 (stop-band jam)))
               ((equal action "bars")
                (json-reply 200 (bars jam (nlk:json-value data :text "owner")
                                      (nlk:json-value data :number "start")
                                      (or (nlk:json-value data :number "count") 1))))
               ((equal action "heartbeat")
                (json-reply 200 (heartbeat jam (nlk:json-value data :text "owner")
                                           (nlk:json-value data :any "clock"))))
               ((equal action "human")
                (json-reply 200 (human jam (nlk:json-value data :any "events"))))
               ((equal action "direction")
                (json-reply 200 (direct jam (nlk:json-value data :text "text"))))
               ((equal action "configure")
                (json-reply 200 (restate jam (nlk:json-value data :text "role")
                                         (nlk:json-value data :any "controls")
                                         :after (or (nlk:json-value data :number "after_bars") 0))))
               ((equal action "undo")
                (json-reply 200 (undo jam (nlk:json-value data :text "role"))))
               ((equal action "save") (json-reply 200 (save jam)))
               ((equal action "export") (json-reply 200 (export-midi jam)))
               (t (json-reply 404 (list :error "unknown action")))))
         (music-jam-error (condition)
           (json-reply 400 (list :error (music-jam-error-detail condition))))
         (error () (json-reply 500 (list :error "operation failed; the session is as it was"))))))))

;;; --- the listener ------------------------------------------------------------------

(defun read-line-octets (stream)
  "One line from STREAM without its line ending, or NIL at end of file."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for octet = (read-byte stream nil nil)
          while octet
          do (if (= octet 10) (return) (vector-push-extend octet octets)))
    (when (plusp (length octets))
      (coerce octets '(vector (unsigned-byte 8))))))

(defun latin (octets)
  "OCTETS as a string, one byte a character."
  (sb-ext:octets-to-string octets :external-format :latin-1))

(defun answer-connection (server client)
  "Answer one HTTP request on CLIENT, a usocket connection, then close it."
  (let ((stream (usocket:socket-stream client)))
    (unwind-protect
         (let ((line (read-line-octets stream)) (headers '()))
           (when line
             (let* ((text (latin line))
                    (space (or (position #\Space text) (length text)))
                    (method (subseq text 0 space))
                    (rest (if (< space (length text)) (subseq text (1+ space)) ""))
                    (path (subseq rest 0 (or (position #\Space rest) (length rest)))))
               (loop for header = (read-line-octets stream)
                     while (and header (plusp (length header)))
                     do (let* ((text (latin header))
                               (colon (position #\: text)))
                          (when colon
                            (push (cons (string-downcase (string-trim '(#\Space) (subseq text 0 colon)))
                                        (string-trim '(#\Space) (subseq text (1+ colon))))
                                  headers))))
               (let* ((length (ignore-errors (parse-integer (or (header headers "content-length") "0"))))
                      (body (if (and length (plusp length) (<= length *max-body*))
                                (let ((octets (make-array length :element-type '(unsigned-byte 8))))
                                  (read-sequence octets stream)
                                  octets)
                                (make-array 0 :element-type '(unsigned-byte 8)))))
                 (multiple-value-bind (status mime answer)
                     (handle server method path headers body)
                   (let ((head (format nil "HTTP/1.1 ~d ~a~%Content-Type: ~a~%Content-Length: ~d~%~
Cache-Control: no-store~%Referrer-Policy: no-referrer~%X-Content-Type-Options: nosniff~%~
Connection: close~%~%"
                                        status (if (= status 200) "OK" "Error") mime (length answer))))
                     (write-sequence (sb-ext:string-to-octets head :external-format :latin-1) stream)
                     (write-sequence answer stream)
                     (finish-output stream)))))))
      (ignore-errors (close stream))
      (ignore-errors (usocket:socket-close client)))))

(defun accept-loop (server)
  "Accept connections on SERVER until its socket closes."
  (handler-case
      (loop
        (let ((client (usocket:socket-accept (band-server-socket server)
                                              :element-type '(unsigned-byte 8))))
          (sb-thread:make-thread (lambda () (answer-connection server client))
                                 :name "music-jam-request")))
    (error () nil)))

(defun band-token ()
  "A bearer minted from the operating system's entropy: 24 bytes, in hex."
  (string-downcase (format nil "~{~2,'0x~}" (coerce (nlk:random-bytes 24) 'list))))

(defun open-server (jam)
  "A listener on 127.0.0.1, an available port, and the page on it."
  (let* ((wanted (if (and (integerp *port*) (plusp *port*)) *port* 0))
         (socket (usocket:socket-listen "127.0.0.1" wanted
                                        :element-type '(unsigned-byte 8)
                                        :reuse-address t)))
    (let ((server (%make-server :socket socket
                                :port (usocket:get-local-port socket)
                                :token (band-token)
                                :jam jam)))
      (setf (band-server-thread server)
            (sb-thread:make-thread (lambda () (accept-loop server)) :name "music-jam"))
      server)))

(defun close-server (server)
  "Close SERVER's socket so its accept loop ends."
  (when server
    (ignore-errors (usocket:socket-close (band-server-socket server)))
    (setf (band-server-socket server) nil)))

(defun page-url (server)
  "SERVER's page, token and all."
  (format nil "http://127.0.0.1:~d/#~a" (band-server-port server) (band-server-token server)))
