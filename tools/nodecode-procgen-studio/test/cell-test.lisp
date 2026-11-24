;;;; cell-test.lisp --- lifecycle, the four tools, the contracts, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches the network, a model, or an image program the
;;;; machine may not have: a pixel-grid source is drawn by render.lisp alone,
;;;; and the one SVG test stubs *RASTERIZE* with a function that writes a
;;;; real PNG and keeps the text it was handed, so what a seed expanded to is
;;;; asserted rather than assumed. Every render lands in the scratch home the
;;;; slice runs in.

(in-package #:nodecode.test)

;;; --- the slice ---------------------------------------------------------------

(define-test-slice "procgen-studio" "PROCGEN-STUDIO-CELL-"
    :start nodecode-procgen-studio:start-cell
  :defaults (("preset" "flat-illustration") ("aspect" "1:1") ("render_timeout" 90)))

(defparameter +tool-names+
  '("procgen_draw" "procgen_style" "procgen_reroll" "procgen_renders")
  "The four tools this cell registers, by the names the model calls.")

(defvar *drawn* '()
  "The SVG text the stubbed rasteriser was handed, newest first.")

(defmacro with-procgen ((&rest pairs) &body body)
  "BODY with the cell started on the section PAIRS (its defaults when none),
stopped on unwind: every verb reads the settings of a running cell."
  `(with-temp-store ()
     (with-cell-stop ((procgen-studio-start ,@pairs))
       ,@body)))

(defun procgen-tool (name &rest pairs)
  "The tool called NAME as the engine calls it: the arguments built from
PAIRS and decoded through the contract the tool registered with."
  (let ((entry (nle::find-tool name)))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun procgen-json (name &rest pairs)
  "The tool called NAME's answer, decoded; it must be one object."
  (let ((answer (nlk:decode-json (apply #'procgen-tool name pairs))))
    (is (hash-table-p answer) (format nil "~a answers one JSON object" name))
    answer))

(defun pixel-source (&optional (rows '("KKKK" "K..K" "K..K" "KKKK")))
  "A pixel-grid source: one palette letter and one frame of ROWS."
  (nlk:encode-json-object
   (nlk:json-object
    "palette" (nlk:json-object "K" "ink")
    "frames" (vector (map 'vector #'identity rows)))))

(defun png-file-p (path)
  "Whether PATH starts with the eight octets every PNG starts with."
  (when (uiop:file-exists-p path)
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let ((head (make-array 8 :element-type '(unsigned-byte 8))))
        (and (= 8 (read-sequence head in))
             (equalp head (coerce (list #x89 80 78 71 13 10 26 10)
                                   '(vector (unsigned-byte 8)))))))))

(defun refusal-of (thunk)
  "THUNK's own refusal, or :NO-REFUSAL when it did not refuse: a tool call's
arguments are refused at the boundary before the body sees them."
  (handler-case (progn (funcall thunk) :no-refusal)
    (nodecode-procgen-studio:procgen-studio-error () :refused)
    (nlk:contract-refusal () :refused)))

(defun drawn-png (png width height)
  "A stub rasteriser's PNG: black, WIDTH by HEIGHT."
  (nodecode-procgen-studio::write-png
   png width height
   (lambda (x y) (declare (ignore x y)) (values 0 0 0))))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "procgen-studio"
  (:help :procgen-studio)
  (:command "procgen-studio")
  (:running
   (is (= 4 (count-if #'nle::find-tool +tool-names+)) "all four tools are registered")
   (is (equal "flat-illustration" (nodecode-procgen-studio::setting :preset)) "the section's preset is bound")
   (is (= 90 (nodecode-procgen-studio::setting :render-timeout)) "and its render timeout"))
  (:stopped
   (is (null (nle::find-tool "procgen_draw")) "stop takes the first tool out")
   (is (null (nle::find-tool "procgen_renders")) "and the last"))
  (:refused ("preset" "no-such-preset") ("aspect" "7:3") ("render_timeout" 0)))

;;; --- drawing ---------------------------------------------------------------------

(deftest procgen-studio-cell-a-pixel-draw-writes-a-png-a-source-and-its-metadata ()
  (with-procgen ()
    (let* ((answer (procgen-json "procgen_draw" "prompt" "a 4x4 test tile" "source" (pixel-source)))
           (record (nodecode-procgen-studio::load-record (gethash "render_id" answer))))
      (is (equal "r0001" (getf record :id)) "the first render of a clean home is r0001")
      (is (integerp (gethash "seed" answer)) "the answer carries the seed that made it")
      (is (png-file-p (getf record :image)) "the image is a PNG, written here")
      (is (= 1024 (getf record :width)) "a 4x4 grid at the square aspect fills 1024 pixels")
      (is (= 1024 (getf record :height)) "in both directions")
      (is (equal "flat-illustration" (getf record :style)) "the section's preset is the style it ran on")
      (is (uiop:file-exists-p (getf record :source)) "the source is kept beside the image")
      (is (search "\"frames\"" (or (nlk:read-text (getf record :source)) "")) "and it is the source as the model wrote it")
      (let ((meta (nlk:decode-json (nlk:read-text (gethash "metadata" answer)))))
        (is (equal (gethash "seed" answer) (nlk:json-value meta :integer "seed")) "the seed in the answer is the seed on disk")
        (is (equal "a 4x4 test tile" (nlk:json-value meta :string "prompt")) "and so is the prompt")))))

(deftest procgen-studio-cell-a-seed-expands-the-source-and-a-re-roll-changes-it ()
  (with-procgen ()
    (let ((*drawn* '())
          (nodecode-procgen-studio::*rasterize*
            (lambda (svg png width height)
              (push (nlk:read-text svg) *drawn*)
              (drawn-png png width height))))
      (let* ((answer (procgen-json "procgen_draw" "prompt" "a gem in a frame" "seed" 1234
                                    "source" "<svg data-seed=\"{{seed}}\" width=\"64\" height=\"64\"><rect fill=\"{{gem}}\" width=\"{{rand:1:9}}\" height=\"64\"/></svg>"))
             (first-drawn (first *drawn*)))
        (is (search "fill=\"#ffcf6e\"" first-drawn) "the gem role became the preset's own colour")
        (is (search "data-seed=\"1234\"" first-drawn) "the seed token became the seed the call named")
        (is (not (search "{{" first-drawn)) "no token is left in what was drawn")
        (is (equal 1234 (gethash "seed" answer)) "and the answer says so")
        (let* ((again (procgen-json "procgen_reroll" "ref" "r0001" "seed" 7))
               (second-drawn (first *drawn*)))
          (is (equal "r0002" (gethash "render_id" again)) "a re-roll is a render of its own")
          (is (search "data-seed=\"7\"" second-drawn) "the new seed reached the same source")
          (is (not (equal first-drawn second-drawn)) "so the drawing really changed")
          (is (not (search "{{" second-drawn)) "and every token expanded"))))))

;;; --- the contract ------------------------------------------------------------------

(deftest procgen-studio-cell-the-contract-carries-a-preset-and-what-it-extends ()
  (with-procgen ()
    (let ((text (procgen-tool "procgen_style" "style" "frost")))
      (is (stringp text) "procgen_style answers text")
      (is (search "STYLE CONTRACT: frost" text) "the preset it was asked for")
      (is (search "soft icy rim" text) "frost's own lighting rule")
      (is (search "10px ink" text) "and the outline rule it inherits from ember")
      (is (search "#56e0ff" text) "frost's own gem colour")
      (is (search "palette (frost)" text) "the palette names where it came from")))
  (with-procgen ()
    (let* ((dir (merge-pathnames "project/" (nlk:home)))
           (file (merge-pathnames "STYLE.md" dir)))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string (format nil "preset: ember~%palette.gem: #123456~%keep outlines heavy~%") out))
      (let ((style (nodecode-procgen-studio::resolve-style :cwd dir)))
        (is (equal "#123456" (cdr (assoc "gem" (nodecode-procgen-studio::style-palette style) :test #'string=))) "one role overridden by the project's STYLE.md")
        (is (equal "ember" (nodecode-procgen-studio::style-palette-name style)) "the palette it extends")
        (is (search "keep outlines heavy" (nodecode-procgen-studio::style-contract style)) "and the prose it adds reaches the model")))))

(deftest procgen-studio-cell-a-source-with-no-prompt-is-refused ()
  (with-procgen ()
    (is (eq :refused (refusal-of (lambda () (procgen-tool "procgen_draw" "source" (pixel-source))))) "a drawing with no prompt refuses")
    (is (eq :refused (refusal-of (lambda () (procgen-tool "procgen_draw" "prompt" "x" "source" "<html></html>")))) "a source that is neither SVG nor a pixel grid refuses")))

;;; --- the command ------------------------------------------------------------------

(deftest procgen-studio-cell-the-command-answers-every-verb-it-given ()
  (with-procgen ()
    (let ((styles (nodecode-procgen-studio::run-slash "styles" nil))
          (help (nodecode-procgen-studio::run-slash "" nil))
          (set (nodecode-procgen-studio::run-slash "set dungeon icons" nil)))
      (is (search "pixel-arcade" styles) "styles names the six presets")
      (is (search "flat-illustration" styles) "including the one in effect")
      (is (search "reroll" help) "the bare command teaches the verbs")
      (is (search "dungeon-icons" set) "set groups the next draws")
      (is (equal "dungeon-icons" (nodecode-procgen-studio::active-set)) "and remembers the group"))
    (is (eq :refused (refusal-of (lambda () (nodecode-procgen-studio::run-slash "show" nil)))) "showing a render in an empty home refuses")
    (let ((record (nodecode-procgen-studio::draw :prompt "a test tile" :source (pixel-source))))
      (is (search "r0001" (nodecode-procgen-studio::run-slash "list" nil)) "list shows what was drawn")
      (is (search "seed=" (nodecode-procgen-studio::run-slash "show last" nil)) "show finds the newest render")
      (is (search "ink" (nodecode-procgen-studio::run-slash "source last" nil)) "source prints the drawing itself")
      (is (equal "dungeon-icons" (getf record :set)) "a new render joins the active set")
      (is (search (namestring (nodecode-procgen-studio::renders-dir)) (nodecode-procgen-studio::run-slash "status" nil)) "status names where the renders are kept"))))
