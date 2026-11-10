;;;; support.lisp --- the test slice: a stubbed Clef MCP endpoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; CLEF-CELL- name prefix; RUN-CLEF-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-CLEF::HTTP, the cell's only
;;;; way to the network. WITH-WIRE replaces it with a canned JSON-RPC answer
;;;; chosen by the tool the request names, and records every call, so a test
;;;; asserts on the request and nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "clef" "CLEF-CELL-" :start nodecode-clef:start-cell)

;;; --- the canned server ------------------------------------------------------------

(defun mcp-answer (structured)
  "The JSON-RPC reply a tools/call gets for STRUCTURED: its text block and
its structured content, as the server sends both."
  (nlk:encode-json-object
   (nlk:json-object
    "jsonrpc" "2.0" "id" 1
    "result" (nlk:json-object
              "content" (vector (nlk:json-object "type" "text"
                                                  "text" (nlk:encode-json-object structured)))
              "structuredContent" structured))))

(defun mcp-refusal (text)
  "The reply a refused tools/call gets: one text block, ISERROR true."
  (nlk:encode-json-object
   (nlk:json-object
    "jsonrpc" "2.0" "id" 1
    "result" (nlk:json-object
              "content" (vector (nlk:json-object "type" "text" "text" text))
              "isError" t))))

(defparameter +transposed+
  (nlk:json-object "chords" "D A Bm G#m7b5/C"
                   "browserTool" "https://clefdrills.com/tools/chord-transposer/")
  "What the stubbed transpose_chords answers for C G Am F#m7b5/Bb, up two.")

(defparameter +spelled+
  (nlk:json-object
   "notes" (vector (nlk:json-object "midi" 60 "name" "C4" "letter" "C" "octave" 4)
                   (nlk:json-object "midi" 64 "name" "E4" "letter" "E" "octave" 4)
                   (nlk:json-object "midi" 67 "name" "G4" "letter" "G" "octave" 4)
                   (nlk:json-object "midi" 71 "name" "B4" "letter" "B" "octave" 4))
   "browserTool" "https://clefdrills.com/tools/piano-chord-finder/")
  "What the stubbed spell_piano_chord answers for Cmaj7.")

(defparameter +tabbed+
  (nlk:json-object
   "result" (nlk:json-object
             "events" (vector (nlk:json-object "index" 0 "block" 0 "column" 2
                                                "midis" (vector 48 52 55 60 64))
                               (nlk:json-object "index" 1 "block" 0 "column" 5
                                                "midis" (vector 47 50 55 59 67)))
             "warnings" (vector "Columns show order, not rhythm; no note durations are encoded."))
   "warnings" (vector "Rhythm and note durations are not inferred from spacing. Standard six-string guitar tuning only.")
   "browserTool" "https://clefdrills.com/tools/guitar-tab-to-piano/")
  "What the stubbed tab_to_piano_notes answers for two columns of a C chord
shape and a G shape.")

(defparameter +found+
  (nlk:json-object
   "guides" (vector
             (nlk:json-object "title" "Tabs to Piano Sheet Music"
                               "description" "A practical guide to converting ASCII tablature."
                               "uri" "https://clefdrills.com/blog/tab-files-to-piano-sheet-music.html")
             (nlk:json-object "title" "Guitar, Bass and Ukulele Tabs to Piano"
                               "description" "Tuning, capo and octaves."
                               "uri" "https://clefdrills.com/blog/alternate-tuning-tabs-piano.html"))
   "directory" "https://clefdrills.com/blog/music-conversion-workflow-finder.html")
  "What the stubbed find_music_guides answers for guitar.")

;;; --- the stub -------------------------------------------------------------------

(defvar *calls* '()
  "Every request the stub took, newest first: (:url :headers :content).")

(defvar *answer* nil
  "A function of one call answering (values BODY STATUS), or NIL for the
canned answer the tool the request names has.")

(defun canned-answer (call)
  "The body the stub answers CALL with, chosen by the tool it names."
  (let ((tool (call-tool-name call)))
    (cond ((equal tool "transpose_chords") (mcp-answer +transposed+))
          ((equal tool "spell_piano_chord") (mcp-answer +spelled+))
          ((equal tool "tab_to_piano_notes") (mcp-answer +tabbed+))
          ((equal tool "find_music_guides") (mcp-answer +found+))
          (t (mcp-refusal (format nil "unknown tool ~a" tool))))))

(defun stub-http (url &key headers content timeout)
  (declare (ignore timeout))
  (let ((call (list :url url :headers headers :content content)))
    (push call *calls*)
    (if *answer* (funcall *answer* call) (values (canned-answer call) 200))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-clef::http (url &key headers content timeout)
                               (stub-http url :headers headers :content content :timeout timeout))
       ,@body)))

;;; --- starting it, calling it -----------------------------------------------------

(defmacro with-clef ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((clef-start ,@pairs))
       ,@body)))

(defun clef-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the request a call made ---------------------------------------------

(defun call-json (call)
  "What the stubbed seam was sent, decoded."
  (nlk:decode-json (getf call :content)))

(defun call-tool-name (call)
  "The MCP tool CALL asks for."
  (nlk:json-value (call-json call) :string "params" "name"))

(defun call-argument (call name)
  "The argument NAME CALL carries, whatever its type."
  (nlk:json-value (call-json call) :any "params" "arguments" name))

(defun call-header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))
