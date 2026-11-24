;;;; cell.lisp --- the drawing verbs, the four tools, /procgen-studio, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin is an image and video *backend*: Hermes's own
;;;; image_generate tool asks the configured model for a drawing program and
;;;; renders it. Nodecode has no image backend to hang itself on, so the seam
;;;; moves up one step and the session's own model writes the source: the
;;;; four tools here render what it wrote, hand back the contract to draw
;;;; against, list and show what was drawn, and re-roll a stored source with
;;;; a new seed at no model cost. The presets, the palette roles, the
;;;; STYLE.md contract and the seed that made a render reproducible are the
;;;; original's; the critic loop and the Python sandbox are not (see NOTICE).

(in-package #:nodecode-procgen-studio)

(nlk:define-peripheral procgen-studio :not-running t)

(defparameter *usage*
  (format nil "Procgen Studio: image assets drawn as code by your own model, rendered locally.~%~
~%  /procgen-studio status                 what is in effect (style, set, rasteriser, renders)~%~
  /procgen-studio styles                 the six built-in presets~%~
  /procgen-studio style [name|show]      one preset's contract, or the one in effect~%~
  /procgen-studio init [preset] [--force]  write a STYLE.md into this directory~%~
  /procgen-studio set [name|off]         group the next draws into an asset set~%~
  /procgen-studio list [n]               recent renders~%~
  /procgen-studio show <id|last>         paths, seed and style of one render~%~
  /procgen-studio source <id|last>       print the drawing's own source~%~
  /procgen-studio reroll <id|last> [--seed n] [--style p]  re-render the same source~%~
  /procgen-studio render <file> [--size WxH] [--style p]   render a local .svg or .pixel.json~%~
~%In a source, {{seed}}, {{rand:0:100}}, {{pick:a,b,c}} and {{gem}} (any palette role) are~%~
replaced before drawing, so a re-roll with a new seed is a real variation.")
  "What /procgen-studio help answers, and what a typo answers with.")

(defparameter *manual*
  (format nil "Procgen Studio draws image assets with code and renders them locally: no image API ~
key, an editable source and a reproducible seed beside every PNG.~%~
~%Draw one: call procgen_draw with a PROMPT (one sentence naming the subject, the count and the ~
layout: \"a set of 4 game icons -- sword, potion, shield, key -- on rounded tiles in a 2x2 ~
grid\") and a SOURCE the model writes: an <svg> document, or a pixel-grid JSON object ~
({\"palette\": {\"K\": \"ink\", \"h\": \"base\"}, \"frames\": [[\"KKK\", \"KhK\", \"KKK\"]]}) whose ~
letters map to palette roles or #hex colours, '.' being background. The answer carries the ~
render id, the seed, the image path, the source path and next steps.~%~
~%Draw against a contract: procgen_style answers the style in effect -- its prose rules and ~
every palette role with its colour -- which comes from the preset section setting, from a ~
STYLE.md in the session's directory (preset:/palette: lines; palette.gem: #56e0ff overrides ~
one role), or from the style argument. Name the asset kind (icon, logo, wordmark, scene, ~
pattern, sprite, diagram): the rules say what each preset looks like. This is a drawing tool, ~
not a photo model.~%~
~%Re-roll and show: procgen_reroll re-renders a stored source with a new seed -- no model ~
call -- and procgen_renders lists the recent renders or shows one, its source included. A ~
source's {{seed}}, {{rand:MIN:MAX}}, {{pick:a,b,c}} and {{role}} tokens are expanded with ~
the seed before drawing, which is what makes a re-roll a variation.~%~
~%The six presets: flat-illustration (layered vector scenes), ember (warm game icons), frost ~
(ember's rules, cold palette), dark-fantasy-lowpoly (faceted creatures), brand-minimal (logos ~
and wordmarks), pixel-arcade (palette-grid sprites, which need no rasteriser).~%~
~%Rendering: a pixel-grid source is drawn here, byte for byte; an SVG needs one of ~
rsvg-convert, resvg, magick, convert, cairosvg or inkscape on PATH. Renders live under the ~
Nodecode home in plugin-data/procgen-studio/renders/<id>/ and are copied into the section's ~
output_dir when one is set. The operator has /procgen-studio for the same verbs, and ~
(help :procgen-studio) for this text.")
  "What (help :procgen-studio) answers while the cell runs.")

;;; --- the boundaries ---------------------------------------------------------------

(nlk:define-contract draw-call
  (:doc "Draw one image asset from a source the model wrote and render it to PNG locally.
The source is an <svg> document, or a pixel-grid JSON object whose letters map to palette roles
or #hex colours and whose '.' cells are background. Its {{seed}}, {{rand:MIN:MAX}},
{{pick:a,b,c}} and {{role}} tokens are expanded with the seed before drawing. The answer
carries the render id, the seed, the image, the source and the metadata paths.")
  ("prompt" :text :required t :doc "one sentence naming the subject, the count and the layout")
  ("source" :text :required t :doc "the drawing itself: an <svg> document or a pixel-grid JSON object")
  ("seed" :integer :doc "fixes the random tokens; omitted, it is derived from the prompt so the same prompt draws the same picture")
  ("style" :text :doc "a preset name (frost, ember, pixel-arcade...) or a path to a STYLE.md, for this drawing only")
  ("set" :text :slot group :doc "group this render with others, so the set can be listed and re-rolled together")
  ("size" :text :doc "WIDTHxHEIGHT, like 1024x1024 or 512x768; omitted, the section's aspect decides"))

(nlk:define-contract style-call
  (:doc "The art contract the next drawing should be written against: the style's prose rules,
its preferred renderer, and every palette role with its colour and what it is for.")
  ("style" :text :doc "a preset name or a STYLE.md path; omitted, the style in effect"))

(nlk:define-contract reroll-call
  (:doc "Re-render a stored source with a new seed, or under another style, without a model
call: the same drawing program, new numbers in it. Answers the new render id and its paths.")
  ("ref" :text :doc "the render to re-roll: its id, or last for the newest; omitted, last")
  ("seed" :integer :doc "the seed to draw with; omitted, one past the original's")
  ("style" :text :doc "a preset name or a STYLE.md path to re-render under; omitted, the stored source's own style"))

(nlk:define-contract renders-call
  (:doc "What has been drawn: the recent renders, or one render's paths, seed, style and
source. Use it before re-rolling, and to find the source of an earlier drawing.")
  ("limit" :integer :default 10 :doc "how many recent renders to list")
  ("ref" :text :doc "one render's id, or last; omitted, the list")
  ("source" :boolean :default nil :doc "with a ref, include the drawing's own source text"))

;;; --- one drawing ------------------------------------------------------------------

(defparameter *aspect-sizes*
  '(("1:1" 1024 1024) ("3:2" 1536 1024) ("2:3" 1024 1536))
  "The section's aspect, as the WIDTH and HEIGHT one render is drawn at.")

(defun drawing-size (size)
  "SIZE -- WIDTHxHEIGHT -- or the section's aspect, as (values WIDTH HEIGHT)."
  (let ((size (and (stringp size) (nlk:trimmed size))))
    (if (and size (plusp (length size)))
        (let* ((x (position #\x size :test #'char-equal))
               (width (and x (parse-integer size :end x :junk-allowed t)))
               (height (and x (parse-integer size :start (1+ x) :junk-allowed t))))
          (unless (and width height (<= 16 width 4096) (<= 16 height 4096))
            (fail "size ~s is no WIDTHxHEIGHT between 16 and 4096 (1024x1024, 512x768)" size))
          (values width height))
        (destructuring-bind (name width height)
            (or (find (setting :aspect) *aspect-sizes* :test #'string= :key #'first)
                (first *aspect-sizes*))
          (declare (ignore name))
          (values width height)))))

(defun derive-seed (prompt palette-name)
  "The seed PROMPT draws with when none is given: a hash of the words and the
palette, so the same prompt draws the same picture again."
  (let* ((text (format nil "~a|~a" (string-downcase (nlk:trimmed prompt)) palette-name))
         (digest (nlk:sha256-text text))
         (hex (subseq digest 7 (min (length digest) 15))))
    (mod (or (ignore-errors (parse-integer hex :radix 16)) 0) 1000000)))

(defun draw (&key prompt source seed style set size)
  "Draw SOURCE -- an SVG document or a pixel grid -- as PROMPT says, render it
to PNG locally, and keep the image, the source, the seed and a meta.json in
one folder under the home. => the record."
  (let* ((prompt (nlk:trimmed (or prompt "")))
         (source (and source (string-trim '(#\Space #\Tab #\Newline #\Return) source)))
         (set (and (stringp set) (nlk:trimmed set)))
         (style (and (stringp style) (nlk:trimmed style)))
         (chosen (resolve-style :override (and style (plusp (length style)) style))))
    (when (zerop (length prompt))
      (fail "a drawing needs a prompt: one sentence naming the subject, the count and the layout"))
    (when (or (null source) (zerop (length source)))
      (fail "a drawing needs a source: an <svg> document, or a pixel-grid JSON object"))
    (let ((seed (or seed (derive-seed prompt (style-palette-name chosen))))
          (set-name (cond ((and set (plusp (length set))) (slugify set))
                          ((plusp (length (active-set))) (active-set))
                          (t ""))))
      (multiple-value-bind (width height) (drawing-size size)
        (let* ((id (next-render-id))
               (dir (record-dir id)))
          (multiple-value-bind (source-path png-path real-width real-height renderer frames)
              (draw-source source chosen seed width height dir)
            (let ((record (list :id id :prompt prompt :seed seed
                                :width real-width :height real-height
                                :renderer renderer :style (style-name chosen)
                                :palette (style-palette-name chosen) :set set-name
                                :frames frames :created (nlk:iso-now)
                                :source source-path :image png-path :dir dir)))
              (save-record record)
              (setf (getf record :published) (publish record))
              (values record))))))))

(defun reroll (&key ref seed style)
  "Re-render the source of REF with a new SEED, or under another STYLE: no
model call, the same drawing program with new numbers in it. => the record."
  (let* ((base (resolve-render ref))
         (source (nlk:read-text (getf base :source)))
         (style (and (stringp style) (nlk:trimmed style))))
    (unless (and source (plusp (length source)))
      (fail "the source of ~a is gone: ~a" (getf base :id) (namestring (getf base :source))))
    (draw :source source
          :prompt (getf base :prompt)
          :seed (or seed (1+ (or (getf base :seed) 0)))
          :style (and style (plusp (length style)) style)
          :set (getf base :set))))

(defun contract-text (&key style)
  "The contract a drawing is held to: the style in effect, or one named by STYLE."
  (let ((style (and (stringp style) (nlk:trimmed style))))
    (style-contract (resolve-style :override (and style (plusp (length style)) style)))))

(defun describe-render (record)
  "RECORD as the lines a person reads: what it is, where it is, what it was drawn with."
  (format nil "~a  ~ax~a  seed=~d  style=~a/~a  renderer=~a~@[  set=~a~]~%  prompt:  ~a~%  image:   ~a~%  source:  ~a~%  record:  ~a"
          (getf record :id) (getf record :width) (getf record :height) (getf record :seed)
          (getf record :style) (getf record :palette) (getf record :renderer)
          (let ((set (getf record :set))) (and (plusp (length set)) set))
          (getf record :prompt)
          (or (getf record :published) (namestring (getf record :image)))
          (namestring (getf record :source))
          (namestring (merge-pathnames "meta.json" (getf record :dir)))))

(defun renders (&key limit ref source)
  "The recent renders, or the one REF names -- its details, and its source
when SOURCE is true. => the text a model or a person reads."
  (let ((ref (and (stringp ref) (nlk:trimmed ref))))
    (if (and ref (plusp (length ref)))
        (let ((record (resolve-render ref)))
          (if source
              (format nil "# ~a~%~a" (namestring (getf record :source))
                      (or (nlk:read-text (getf record :source)) "(the source file is gone)"))
              (describe-render record)))
        (let* ((count (max 1 (min 50 (or limit 10))))
               (records (latest-renders count)))
          (if (null records)
              "no renders yet: procgen_draw one"
              (with-output-to-string (out)
                (format out "~d render~:p, newest first:~%" (length records))
                (dolist (record records)
                  (format out "~a  seed=~d  ~ax~a  ~a/~a~@[  set=~a~]  ~a~%"
                          (getf record :id) (getf record :seed) (getf record :width)
                          (getf record :height) (getf record :style) (getf record :palette)
                          (let ((set (getf record :set))) (and (plusp (length set)) set))
                          (nlk:clip (getf record :prompt) 70)))))))))

;;; --- the tools ---------------------------------------------------------------------

(defun draw-answer (record)
  "RECORD as the JSON a tool call answers."
  (nlk:encode-json-object
   (nlk:json-object
    "render_id" (getf record :id)
    "seed" (getf record :seed)
    "size" (format nil "~dx~d" (getf record :width) (getf record :height))
    "renderer" (getf record :renderer)
    "style" (getf record :style)
    "palette" (getf record :palette)
    "set" (getf record :set)
    "image" (or (getf record :published) (namestring (getf record :image)))
    "source" (namestring (getf record :source))
    "metadata" (namestring (merge-pathnames "meta.json" (getf record :dir)))
    "next_steps" (format nil "Re-roll it locally with no new drawing: procgen_reroll ref ~a -- or edit the source file and re-roll. procgen_style answers the contract for the next one."
                          (getf record :id)))))

(defun tool-draw (call)
  "The procgen_draw tool: one drawing in, its render's paths out."
  (draw-answer (draw :prompt (draw-call-prompt call)
                      :source (draw-call-source call)
                      :seed (draw-call-seed call)
                      :style (draw-call-style call)
                      :set (draw-call-group call)
                      :size (draw-call-size call))))

(defun tool-style (call)
  "The procgen_style tool: the contract to draw against."
  (contract-text :style (style-call-style call)))

(defun tool-reroll (call)
  "The procgen_reroll tool: a stored source drawn again."
  (draw-answer (reroll :ref (reroll-call-ref call)
                        :seed (reroll-call-seed call)
                        :style (reroll-call-style call))))

(defun tool-renders (call)
  "The procgen_renders tool: what has been drawn."
  (renders :limit (renders-call-limit call)
           :ref (renders-call-ref call)
           :source (renders-call-source call)))

;;; --- /procgen-studio ----------------------------------------------------------------

(defun option (words name)
  "The value of --NAME in WORDS, written --NAME VALUE or --NAME=VALUE, or NIL."
  (or (let ((at (position name words :test #'string=)))
        (and at (second (nthcdr at words))))
      (let ((prefix (concatenate 'string name "=")))
        (some (lambda (word)
                (and (uiop:string-prefix-p prefix word) (subseq word (length prefix))))
              words))))

(defun operands (words)
  "WORDS without the --NAME VALUE pairs: what the verb is asked about."
  (let ((out '()) (skip nil))
    (dolist (word words (nreverse out))
      (cond (skip (setf skip nil))
            ((uiop:string-prefix-p "--" word) (setf skip (not (find #\= word))))
            (t (push word out))))))

(defun integer-option (words name)
  "--NAME as an integer, or NIL; a value that is not one is a refusal."
  (let ((value (option words name)))
    (and value
         (or (ignore-errors (parse-integer value :junk-allowed t))
             (fail "--~a wants a number, got ~s" (subseq name 2) value)))))

(defun store-line ()
  "Where the renders are kept, said the way a person can act on."
  (namestring (renders-dir)))

(defun status-text ()
  "What is in effect right now."
  (let* ((chosen (handler-case
                      (resolve-style)
                    (procgen-studio-error (error) (princ-to-string error))))
         (set (active-set)))
    (format nil "Procgen Studio~%  style in effect: ~a~%  working dir:     ~a~%  active set:      ~a~%  rasteriser:      ~a~%  renders stored:  ~d in ~a~%  section:         the procgen-studio section in /settings (preset, style_file, output_dir, aspect, render_timeout)"
            (if (stringp chosen)
                (format nil "error: ~a" chosen)
                (format nil "~a (palette ~a, from ~a)" (style-name chosen) (style-palette-name chosen)
                        (style-source chosen)))
            (namestring (default-directory))
            (if (plusp (length set)) set "-")
            (or (rasterizer-name) "none: pixel-grid sources only")
            (length (render-ids)) (store-line))))

(defun styles-text ()
  "The six presets, the one in effect marked."
  (let ((active (ignore-errors (setting :preset))))
    (with-output-to-string (out)
      (format out "Built-in style presets:~%")
      (dolist (name (preset-names))
        (let ((entry (preset-entry name)))
          (format out "  ~c ~a  ~a~%"
                  (if (string= name active) #\* #\Space) name
                  (or (getf entry :description) ""))))
      (format out "Draw against one with the style argument, or write a STYLE.md: /procgen-studio init <preset>"))
    ))

(defun style-command (words)
  "STYLE SHOW prints the contract in effect, STYLE NAME one preset's."
  (let ((name (first (operands words))))
    (if (or (null name) (string= name "show"))
        (style-contract (resolve-style))
        (let ((style (resolve-style :override (if (member name '("clear" "reset" "default") :test #'string=)
                                                  (setting :preset) name))))
          (format nil "~a~%~%[this look only: write a STYLE.md (/procgen-studio init ~a) or set the preset in /settings to keep it]"
                  (style-contract style) (style-palette-name style))))))

(defun init-command (words)
  "INIT [preset] writes a STYLE.md into the working directory."
  (let* ((name (or (first (operands words)) "ember"))
         (preset (load-preset name))
         (target (merge-pathnames "STYLE.md" (default-directory))))
    (when (and (uiop:file-exists-p target) (null (option words "--force")))
      (fail "~a already exists (pass --force to overwrite it)" (namestring target)))
    (with-open-file (out target :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
      (write-string (starter-style name) out))
    (format nil "Wrote ~a from preset ~a (palette ~a). Edit any line -- try changing `palette: ~a` to another preset -- and draw again."
            (namestring target) (style-name preset) (style-palette-name preset) (style-palette-name preset))))

(defun set-command (words)
  "SET NAME groups the next draws; SET off clears it."
  (let ((name (string-downcase (format nil "~{~a~^ ~}" (operands words)))))
    (cond ((or (zerop (length name)) (member name '("off" "none" "clear") :test #'string=))
           (setf (active-set) nil)
           "Asset set cleared: new renders join no set.")
          (t (setf (active-set) (slugify name))
             (format nil "New renders go into set '~a'; /procgen-studio list and a re-roll keep it."
                     (active-set))))))

(defun list-command (words)
  "LIST [n] shows the recent renders."
  (let* ((first (first (operands words)))
         (count (or (and first (ignore-errors (parse-integer first :junk-allowed t))) 10)))
    (renders :limit count :ref "")))

(defun show-command (words &key source)
  "SHOW REF shows one render; with SOURCE, its drawing source too."
  (renders :ref (or (first (operands words)) "last") :source source))

(defun reroll-command (words)
  "REROLL REF [--seed n] [--style p] draws a stored source again."
  (let ((record (reroll :ref (or (first (operands words)) "last")
                        :seed (integer-option words "--seed")
                        :style (option words "--style"))))
    (format nil "Re-rolled with no new drawing:~%~a" (describe-render record))))

(defun render-command (words)
  "RENDER FILE [--size WxH] [--style p] renders a local .svg or .pixel.json."
  (let* ((name (first (operands words)))
         (path (and name (uiop:ensure-pathname name))))
    (unless (and path (uiop:file-exists-p path))
      (fail "no such file: ~a" (or name "(none named)")))
    (let ((record (draw :source (nlk:read-text path)
                        :prompt (pathname-name path)
                        :seed (integer-option words "--seed")
                        :style (option words "--style")
                        :size (option words "--size"))))
      (format nil "Rendered ~a:~%~a" (namestring path) (describe-render record)))))

(defun run-slash (args session-id)
  "/procgen-studio typed alone shows what is in effect; a verb and its
argument do the rest. => the answer's text."
  (declare (ignore session-id))
  (let* ((words (remove "" (uiop:split-string (nlk:trimmed (or args ""))
                                               :separator '(#\Space #\Tab))
                        :test #'string=))
         (verb (string-downcase (or (first words) "help")))
         (rest (rest words)))
    (cond ((member verb '("help" "?") :test #'string=) *usage*)
          ((string= verb "status") (status-text))
          ((member verb '("styles" "presets") :test #'string=) (styles-text))
          ((string= verb "style") (style-command rest))
          ((string= verb "init") (init-command rest))
          ((string= verb "set") (set-command rest))
          ((member verb '("list" "ls") :test #'string=) (list-command rest))
          ((string= verb "show") (show-command rest))
          ((string= verb "source") (show-command rest :source t))
          ((string= verb "reroll") (reroll-command rest))
          ((string= verb "render") (render-command rest))
          (t (format nil "procgen-studio: no verb ~s~%~%~a" verb *usage*)))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell procgen-studio
  (:section ("procgen-studio")
    (:guide "nothing here is required; preset is the style a drawing runs on when no STYLE.md and no style argument says otherwise, output_dir also receives each render, aspect is the shape a render with no size gets")
    ("preset" :choice :options '("flat-illustration" "ember" "frost" "dark-fantasy-lowpoly"
                                  "brand-minimal" "pixel-arcade")
     :default "flat-illustration"
     :doc "The built-in style preset a drawing runs on when no STYLE.md in the session's directory and no style argument applies.")
    ("style_file" :path
     :doc "A STYLE.md that applies everywhere, relative to the session's directory; it wins over the project STYLE.md and the preset.")
    ("output_dir" :path
     :doc "A folder that also receives each render, its source and its metadata -- for example ./assets/procgen.")
    ("aspect" :choice :options '("1:1" "3:2" "2:3") :default "1:1"
     :doc "The shape a render asked for without a size gets: 1:1 is 1024x1024, 3:2 is 1536x1024, 2:3 is 1024x1536.")
    ("render_timeout" :integer :default 90 :min 1
     :doc "How long the SVG rasteriser may take, in seconds, before its process tree is killed."))
  (:tool "procgen_draw" (nlk:contract 'draw-call) #'tool-draw)
  (:tool "procgen_style" (nlk:contract 'style-call) #'tool-style)
  (:tool "procgen_reroll" (nlk:contract 'reroll-call) #'tool-reroll)
  (:tool "procgen_renders" (nlk:contract 'renders-call) #'tool-renders)
  (:command "procgen-studio" #'run-slash
            :catches 'procgen-studio-error
            :description "Draw, re-roll, restyle and show locally rendered image assets"
            :argument-hint "VERB [ARGUMENT]")
  (:help :procgen-studio
         "procgen-studio: image assets drawn as code -- procgen_draw, procgen_style, procgen_reroll, procgen_renders"
         *manual*))
