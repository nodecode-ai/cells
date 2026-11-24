;;;; jxa.lisp --- the app scripts, and the one seam that runs them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Numbers, Keynote and Pages are scriptable: each script below is
;;;; JavaScript for Automation, run by the osascript macOS ships, with its
;;;; parameters as one JSON object in argv[0] and one JSON object back on
;;;; stdout. Every script wraps its body in try/catch and answers
;;;; {ok: false, error: ...} rather than exiting non-zero, so a refusal
;;;; arrives as data and the cell can word it.
;;;;
;;;; Running through *OSASCRIPT* is the cell's one outside seam: the tests
;;;; rebind it and answer canned objects, so the suite needs no Mac, no app
;;;; and no network. On a machine that is not a Mac the runner refuses before
;;;; it starts a process, which is the whole portability story.
;;;;
;;;; Rewritten from Arkanji's Hermes Agent plugin iwork-studio; see NOTICE.

(in-package #:nodecode-iwork)

(defparameter *osascript-fallback* "/usr/bin/osascript"
  "Where macOS keeps osascript; the cell's osascript setting overrides it.")

(defparameter *read-numbers-script*
  "function run(argv) {
  var p = JSON.parse(argv[0]);
  function pathOf(d) { var f = String(d.file()); if (f.length && f.charAt(f.length - 1) === '/') f = f.slice(0, -1); return f; }
  try {
    var app = Application('Numbers');
    var doc = null, opened = false, docs = app.documents();
    for (var i = 0; i < docs.length; i++) { try { if (pathOf(docs[i]) === p.path) doc = docs[i]; } catch (e) {} }
    if (!doc) { doc = app.open(Path(p.path)); opened = true; }
    var sheets = doc.sheets(), names = [], tables = [];
    for (var s = 0; s < sheets.length; s++) {
      var sheetName = String(sheets[s].name()); names.push(sheetName);
      var own = sheets[s].tables();
      for (var t = 0; t < own.length; t++) tables.push(sheetName + '/' + String(own[t].name()));
    }
    var si = 0, ti = 0, chosen;
    if (p.sheet) { for (var s = 0; s < names.length; s++) if (names[s] === p.sheet) si = s; }
    chosen = sheets[si].tables();
    if (p.table) { for (var t = 0; t < chosen.length; t++) if (String(chosen[t].name()) === p.table) ti = t; }
    var table = chosen[ti];
    var values = table.cells[p.range ? p.range : 'A1:J20'].value(), rows = [];
    var limit = p.max_rows ? p.max_rows : 20;
    for (var r = 0; r < values.length && r < limit; r++) {
      var row = [];
      for (var c = 0; c < values[r].length; c++) row.push(values[r][c] === null ? '' : String(values[r][c]));
      rows.push(row);
    }
    var answer = {ok: true, kind: 'numbers', sheets: names, tables: tables,
                  sheet: names[si], table: String(table.name()), rows: rows};
    if (opened) app.close(doc, {saving: 'no'});
    return JSON.stringify(answer);
  } catch (e) { return JSON.stringify({ok: false, error: String(e)}); }
}")

(defparameter *read-keynote-script*
  "function run(argv) {
  var p = JSON.parse(argv[0]);
  function pathOf(d) { var f = String(d.file()); if (f.length && f.charAt(f.length - 1) === '/') f = f.slice(0, -1); return f; }
  function text(item) { try { return item ? String(item.objectText()) : ''; } catch (e) { return ''; } }
  try {
    var app = Application('Keynote');
    var doc = null, opened = false, docs = app.documents();
    for (var i = 0; i < docs.length; i++) { try { if (pathOf(docs[i]) === p.path) doc = docs[i]; } catch (e) {} }
    if (!doc) { doc = app.open(Path(p.path)); opened = true; }
    var slides = doc.slides(), out = [];
    for (var i = 0; i < slides.length; i++) {
      out.push({slide: i + 1,
                title: text(slides[i].defaultTitleItem()),
                body: text(slides[i].defaultBodyItem())});
    }
    if (opened) app.close(doc, {saving: 'no'});
    return JSON.stringify({ok: true, kind: 'keynote', slides: out});
  } catch (e) { return JSON.stringify({ok: false, error: String(e)}); }
}")

(defparameter *read-pages-script*
  "function run(argv) {
  var p = JSON.parse(argv[0]);
  function pathOf(d) { var f = String(d.file()); if (f.length && f.charAt(f.length - 1) === '/') f = f.slice(0, -1); return f; }
  try {
    var app = Application('Pages');
    var doc = null, opened = false, docs = app.documents();
    for (var i = 0; i < docs.length; i++) { try { if (pathOf(docs[i]) === p.path) doc = docs[i]; } catch (e) {} }
    if (!doc) { doc = app.open(Path(p.path)); opened = true; }
    var body = '';
    try { body = String(doc.bodyText()); } catch (e) { body = ''; }
    if (opened) app.close(doc, {saving: 'no'});
    return JSON.stringify({ok: true, kind: 'pages', text: body});
  } catch (e) { return JSON.stringify({ok: false, error: String(e)}); }
}")

(defparameter *set-cell-script*
  "function run(argv) {
  var p = JSON.parse(argv[0]);
  function pathOf(d) { var f = String(d.file()); if (f.length && f.charAt(f.length - 1) === '/') f = f.slice(0, -1); return f; }
  try {
    var app = Application('Numbers');
    var doc = null, opened = false, docs = app.documents();
    for (var i = 0; i < docs.length; i++) { try { if (pathOf(docs[i]) === p.path) doc = docs[i]; } catch (e) {} }
    if (!doc) { doc = app.open(Path(p.path)); opened = true; }
    var sheets = doc.sheets(), si = 0, ti = 0;
    if (p.sheet) { for (var s = 0; s < sheets.length; s++) if (String(sheets[s].name()) === p.sheet) si = s; }
    var tables = sheets[si].tables();
    if (p.table) { for (var t = 0; t < tables.length; t++) if (String(tables[t].name()) === p.table) ti = t; }
    var table = tables[ti];
    table.cells[p.ref].value = p.value;
    doc.save();
    var wrote = String(table.cells[p.ref].value());
    if (opened) app.close(doc);
    return JSON.stringify({ok: true, kind: 'numbers', ref: p.ref, wrote: wrote});
  } catch (e) { return JSON.stringify({ok: false, error: String(e)}); }
}")

(defparameter *slide-script*
  "function run(argv) {
  var p = JSON.parse(argv[0]);
  function pathOf(d) { var f = String(d.file()); if (f.length && f.charAt(f.length - 1) === '/') f = f.slice(0, -1); return f; }
  function text(item) { try { return item ? String(item.objectText()) : ''; } catch (e) { return ''; } }
  function setText(item, value) { if (!item) throw new Error('the slide has no such text item'); item.objectText = value; }
  try {
    var app = Application('Keynote');
    var doc = null, opened = false, docs = app.documents();
    for (var i = 0; i < docs.length; i++) { try { if (pathOf(docs[i]) === p.path) doc = docs[i]; } catch (e) {} }
    if (!doc) { doc = app.open(Path(p.path)); opened = true; }
    var index = p.slide ? p.slide : 1;
    if (p.action === 'add') {
      var master = doc.masterSlides()[0];
      doc.slides.push(app.Slide({baseSlide: master}));
      index = doc.slides.length;
      if (p.title) setText(doc.slides[index - 1].defaultTitleItem(), p.title);
      if (p.body) setText(doc.slides[index - 1].defaultBodyItem(), p.body);
    } else if (p.action === 'set_text') {
      if (p.title) setText(doc.slides[index - 1].defaultTitleItem(), p.title);
      if (p.body) setText(doc.slides[index - 1].defaultBodyItem(), p.body);
    } else if (p.action === 'delete') {
      if (doc.slides.length < 2) throw new Error('the last slide cannot be deleted');
      doc.slides[index - 1].delete();
    }
    doc.save();
    var slides = doc.slides(), out = [];
    for (var i = 0; i < slides.length; i++) {
      out.push({slide: i + 1, title: text(slides[i].defaultTitleItem()), body: text(slides[i].defaultBodyItem())});
    }
    if (opened) app.close(doc);
    return JSON.stringify({ok: true, kind: 'keynote', action: p.action, slide: index, slides: out});
  } catch (e) { return JSON.stringify({ok: false, error: String(e)}); }
}")

(defparameter *export-script*
  "function run(argv) {
  var p = JSON.parse(argv[0]);
  function pathOf(d) { var f = String(d.file()); if (f.length && f.charAt(f.length - 1) === '/') f = f.slice(0, -1); return f; }
  try {
    var app = Application(p.app);
    var doc = null, opened = false, docs = app.documents();
    for (var i = 0; i < docs.length; i++) { try { if (pathOf(docs[i]) === p.path) doc = docs[i]; } catch (e) {} }
    if (!doc) { doc = app.open(Path(p.path)); opened = true; }
    doc.export({to: Path(p.output), as: p.as});
    if (opened) app.close(doc, {saving: 'no'});
    return JSON.stringify({ok: true, kind: 'export', output: p.output, as: p.as});
  } catch (e) { return JSON.stringify({ok: false, error: String(e)}); }
}")

(defparameter *scripts*
  `((:read-numbers . ,*read-numbers-script*)
    (:read-keynote . ,*read-keynote-script*)
    (:read-pages . ,*read-pages-script*)
    (:set-cell . ,*set-cell-script*)
    (:slide . ,*slide-script*)
    (:export . ,*export-script*))
  "What each operation runs: a keyword to the JavaScript above.")

;;; --- the seam -----------------------------------------------------------------

(defun macos-p ()
  "Whether this is a Mac, which is where Numbers, Keynote and Pages live."
  (uiop:os-macosx-p))

(defun osascript-pathname ()
  "The osascript this cell runs, as a pathname."
  (let ((named (ignore-errors (setting :osascript))))
    (uiop:ensure-pathname (if (and named (plusp (length (princ-to-string named))))
                               (princ-to-string named)
                               *osascript-fallback*))))

(defun default-osascript (op script payload)
  "Run SCRIPT with PAYLOAD in argv[0] through osascript, and answer the JSON
object it printed. OP is the operation, for a caller that wants it."
  (declare (ignore op))
  (unless (macos-p)
    (fail "iWork editing drives Apple's Numbers, Keynote and Pages, so it runs on macOS only, and this machine is not a Mac"))
  (let ((program (osascript-pathname)))
    (unless (probe-file program)
      (fail "no osascript at ~a: set the cell's osascript to the one macOS ships (~a)"
            (namestring program) *osascript-fallback*))
    (let ((out (handler-case
                    (uiop:run-program (list (namestring program) "-l" "JavaScript"
                                             "-e" script "--" payload)
                                      :output '(:string :stripped t)
                                      :error-output '(:string :stripped t)
                                      :ignore-error-status t
                                      :timeout (ignore-errors (setting :timeout)))
                  (error (condition)
                    (fail "the app's script could not run: ~a" (nlk:one-line (princ-to-string condition)))))))
      (or (ignore-errors (nlk:decode-json out))
          (fail "the app's script answered something that is not JSON: ~a"
                (nlk:clip (nlk:one-line (or out "")) 240))))))

(defvar *osascript* #'default-osascript
  "The one seam: a function of (operation script payload) answering the JSON
object a script printed. The tests rebind it and answer canned objects.")

(defun run-op (op &rest parameters)
  "Run OPERATION with PARAMETERS (a plist of JSON values) and answer its
object; a script that answers ok=false is a refusal."
  (let* ((script (or (cdr (assoc op *scripts*))
                     (fail "no script for ~a" op)))
         (payload (apply #'nlk:make-json-object
                          (loop for (key value) on parameters by #'cddr
                                collect (string-downcase
                                         (if (stringp key) key (symbol-name key)))
                                collect value)))
         (answer (funcall *osascript* op script (nlk:encode-json-object payload))))
    (unless (nlk:json-value answer :boolean "ok")
      (fail "~a" (or (nlk:json-value answer :string "error")
                     "the app answered nothing")))
    answer))
