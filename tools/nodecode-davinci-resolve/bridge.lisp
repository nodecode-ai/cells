;;;; bridge.lisp --- the live half: Resolve's own scripting API, through Python.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; DaVinci Resolve publishes its scripting API as a Python module,
;;;; DaVinciResolveScript, that talks to the running app over a local socket.
;;;; Every live verb is one call of that module: this file carries the one
;;;; program that speaks it, a request of (OP, PARAMS) and an answer of one
;;;; JSON object, and the Lisp that runs it and decodes it. The interpreter is
;;;; the operator's own, named by the section's `python'; nothing is bundled.

(in-package #:nodecode-davinci-resolve)

;;; --- the one failure the model sees -----------------------------------------

(define-condition resolve-error (error)
  ((detail :initarg :detail :reader resolve-error-detail))
  (:report (lambda (condition stream)
             (write-string (resolve-error-detail condition) stream))))

(defun resolve-error (format &rest args)
  "Signal RESOLVE-ERROR with FORMAT's text: the model reads it in the tool
 result, so it says what to do, never a traceback."
  (error 'resolve-error :detail (apply #'format nil format args)))

;;; --- the program -------------------------------------------------------------

(defparameter +bridge-source+
  "import sys, os, json

def connect():
    api = (sys.argv[3] if len(sys.argv) > 3 else '') or os.environ.get('RESOLVE_SCRIPT_API') or '/Library/Application Support/Blackmagic Design/DaVinci Resolve/Developer/Scripting'
    mods = os.path.join(api, 'Modules')
    if os.path.isdir(mods) and mods not in sys.path:
        sys.path.insert(0, mods)
    try:
        import DaVinciResolveScript as dvr
    except Exception as exc:
        raise RuntimeError('DaVinciResolveScript could not be imported; set davinci-resolve.script_api to the Resolve Scripting folder (' + type(exc).__name__ + ')')
    resolve = dvr.scriptapp('Resolve')
    if resolve is None:
        raise RuntimeError('DaVinci Resolve answered no scripting session; open Resolve and enable scripting')
    return resolve

def project(resolve):
    manager = resolve.GetProjectManager()
    if manager is None:
        raise RuntimeError('Resolve answered no project manager')
    current = manager.GetCurrentProject()
    if current is None:
        raise RuntimeError('no project is open in DaVinci Resolve')
    return current

def status(resolve):
    manager = resolve.GetProjectManager()
    current = manager.GetCurrentProject() if manager is not None else None
    out = {'product': str(resolve.GetProductName()), 'version': str(resolve.GetVersionString()), 'project': None, 'timelines': [], 'timeline': None, 'rendering': False}
    try:
        out['page'] = str(resolve.GetCurrentPage())
    except Exception:
        out['page'] = ''
    if current is not None:
        out['project'] = str(current.GetName())
        count = current.GetTimelineCount() or 0
        for index in range(count):
            timeline = current.GetTimelineByIndex(index + 1)
            if timeline is not None:
                out['timelines'].append(str(timeline.GetName()))
        timeline = current.GetCurrentTimeline()
        if timeline is not None:
            out['timeline'] = {'name': str(timeline.GetName()), 'frame_rate': str(timeline.GetSetting('timelineFrameRate')), 'start': timeline.GetStartFrame(), 'end': timeline.GetEndFrame()}
        out['rendering'] = bool(current.IsRenderingInProgress())
    return out

def do_import(resolve, req):
    current = project(resolve)
    pool = current.GetMediaPool()
    if pool is None:
        raise RuntimeError('the current project has no media pool')
    wanted = req.get('paths') or []
    have = [path for path in wanted if os.path.exists(path)]
    missing = [path for path in wanted if not os.path.exists(path)]
    if not have:
        raise RuntimeError('none of the paths exist')
    if req.get('bin'):
        folder = pool.AddSubFolder(pool.GetRootFolder(), req['bin'])
        if folder is not None:
            pool.SetCurrentFolder(folder)
    items = pool.ImportMedia(have) or []
    return {'imported': len(items), 'names': [str(item.GetName()) for item in items if item is not None], 'missing': missing}

def do_marker(resolve, req):
    current = project(resolve)
    timeline = current.GetCurrentTimeline()
    if timeline is None:
        raise RuntimeError('no current timeline is active')
    name = req.get('name') or 'Marker'
    color = req.get('color') or 'Blue'
    added = timeline.AddMarker(int(req.get('frame') or 0), color, name, req.get('note') or '', int(req.get('duration') or 1), '')
    if not added:
        raise RuntimeError('Resolve refused the marker')
    return {'added': True, 'frame': int(req.get('frame') or 0), 'name': name, 'color': color}

def do_render(resolve, req):
    current = project(resolve)
    if current.GetCurrentTimeline() is None:
        raise RuntimeError('no current timeline is active')
    target = req.get('target_dir') or ''
    if not target:
        raise RuntimeError('a render needs a target directory')
    if not os.path.isdir(target):
        os.makedirs(target)
    settings = dict(req.get('render_settings') or {})
    settings['TargetDir'] = target
    settings['CustomName'] = req.get('custom_name') or 'resolve-render'
    preset = None
    if req.get('preset_name'):
        preset = bool(current.LoadRenderPreset(req['preset_name']))
    codec = None
    if req.get('render_format') and req.get('render_codec'):
        codec = bool(current.SetCurrentRenderFormatAndCodec(req['render_format'], req['render_codec']))
    applied = bool(current.SetRenderSettings(settings))
    job = current.AddRenderJob()
    if not job:
        raise RuntimeError('Resolve created no render job')
    started = bool(current.StartRendering(job)) if req.get('start', True) else False
    return {'job_id': str(job), 'started': started, 'preset_loaded': preset, 'format_and_codec': codec, 'settings_applied': applied}

def main():
    op = sys.argv[1]
    req = json.loads(sys.argv[2] or '{}')
    try:
        resolve = connect()
        if op == 'status':
            out = status(resolve)
        elif op == 'import':
            out = do_import(resolve, req)
        elif op == 'marker':
            out = do_marker(resolve, req)
        elif op == 'render':
            out = do_render(resolve, req)
        else:
            raise RuntimeError('unknown op ' + op)
        out['ok'] = True
    except Exception as exc:
        out = {'ok': False, 'error': type(exc).__name__ + ': ' + str(exc)}
    print(json.dumps(out))

main()
"
  "The program the bridge runs: one op, one JSON request, one JSON answer.
 It connects to the running Resolve, so every failure is a sentence.")

;;; --- running it --------------------------------------------------------------

(defvar *bridge* nil
  "The function a live verb reaches Resolve through, or NIL for BRIDGE-CALL.
 A test binds it to a stub, so no test needs Resolve, Python or a socket.")

(defun bridge-call (op &optional params)
  "Run OP against the running DaVinci Resolve with PARAMS, a JSON object.
 => the decoded answer object. Signalling RESOLVE-ERROR when no interpreter,
 no module, no Resolve or no project answers."
  (check-running)
  (let* ((interpreter (or (setting :python) "python3"))
         (request (shasht:write-json (or params (nlk:json-object)) nil))
         (program (list interpreter "-c" +bridge-source+ op
                        request (or (setting :script-api) "")))
         (outputs (handler-case
                      (multiple-value-list
                       (uiop:run-program program :output :string :error-output :string
                                                  :ignore-error-status t))
                    (error (condition)
                      (resolve-error "~a could not be run (~a); set davinci-resolve.python to an interpreter that can load DaVinciResolveScript"
                                     interpreter (type-of condition)))))
         (output (first outputs))
         (answer (handler-case (nlk:decode-json output) (error () nil))))
    (unless (hash-table-p answer)
      (resolve-error "~a answered nothing usable (~a)~@[; it said ~s~]; set davinci-resolve.python to an interpreter that can load DaVinciResolveScript"
                     interpreter
                     (or (third outputs) 0)
                     (text-snippet (or (second outputs) output))))
    (unless (nlk:json-value answer :boolean "ok")
      (resolve-error "~a" (or (nlk:json-value answer :string "error")
                                "the Resolve bridge failed with no reason")))
    answer))

(defun text-snippet (text)
  "TEXT's first 200 characters, its edges trimmed, or NIL when it says nothing."
  (let ((text (string-trim '(#\Space #\Newline #\Return #\Tab) (or text ""))))
    (and (plusp (length text)) (subseq text 0 (min 200 (length text))))))

(defun running-p ()
  "Whether this cell has started: its settings are set, so a verb may read them."
  (and (boundp '*davinci-resolve*) (not (null (symbol-value '*davinci-resolve*)))))

(defun check-running ()
  "Refuse a verb called on a folder that never started, in words that say so."
  (unless (running-p)
    (resolve-error "the resolve cell is not running; turn it on in /cells, or add a davinci-resolve section to the config")))

(defun call-resolve (op &optional params)
  "OP through the bridge in force: *BRIDGE*'s stub in a test, else the app."
  (if *bridge*
      (funcall *bridge* op params)
      (bridge-call op params)))
