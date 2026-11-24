;;;; style.lisp --- STYLE.md and the six presets: one resolved contract per render.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's procgen_studio/style.py, read once and kept as data. A
;;;; style is a preset (frost extends ember), a palette pulled whole from
;;;; another preset, single roles overridden (palette.gem: #56e0ff) and
;;;; free prose rules -- everything but the structural keys goes to the
;;;; drawing model verbatim, so a one-line change restyles every asset on the
;;;; next draw. A project STYLE.md -- ./.procgen/STYLE.md or ./STYLE.md in
;;;; the session's own directory, when it carries a preset: or palette: line
;;;; -- is the art contract for every render there.

(in-package #:nodecode-procgen-studio)

(defparameter *palette-roles*
  '("bg" "bg2" "ink" "base" "base2" "metal" "metal2" "accent" "accent2"
    "wood" "wood2" "liquid" "liquid2" "gem" "glow" "highlight" "shadow"
    "sky" "sky2" "ground" "foliage" "light")
  "Every palette defines the same roles, so a drawing written against one
role -- bg, ink, gem, glow -- keeps its meaning when the palette is swapped.")

(defparameter *role-hints*
  '(("bg" . "background, light end of the gradient")
    ("bg2" . "background, dark end / vignette")
    ("ink" . "outlines and darkest line work")
    ("base" . "main subject material, lit side")
    ("base2" . "main subject material, shadow side")
    ("metal" . "steel / silver, lit")
    ("metal2" . "steel / silver, shadow")
    ("accent" . "trim, gold, brand accent, lit")
    ("accent2" . "trim, gold, brand accent, shadow")
    ("wood" . "wood / leather, lit")
    ("wood2" . "wood / leather, shadow")
    ("liquid" . "potions, water, energy, lit")
    ("liquid2" . "potions, water, energy, deep")
    ("gem" . "gems, eyes, small hot details")
    ("glow" . "glows, halos, rim light")
    ("highlight" . "specular highlights")
    ("shadow" . "cast shadows, ambient occlusion")
    ("sky" . "sky, top")
    ("sky2" . "sky, horizon")
    ("ground" . "ground plane, far terrain")
    ("foliage" . "trees, grass, vegetation")
    ("light" . "lamps, moons, windows, emissive"))
  "What each role is for, printed beside it in the contract.")

(defparameter *max-style-bytes* (* 64 1024)
  "A STYLE.md larger than this is refused: it rides every drawing's context.")

(defstruct (style (:constructor make-style))
  "One resolved art contract: what the drawing is held to, and the colours
it may use. SOURCE says where it came from -- preset, file or user."
  (name "")
  (palette-name "")
  (palette '())
  (rules '())
  (notes "")
  (renderer "")
  (description "")
  (source "preset")
  (path nil))

;;; --- the six presets, as one entry each ---------------------------------------------

(defun preset-entry (name)
  "The `*presets*' entry called NAME, or a refusal naming the six."
  (or (cdr (assoc name *presets* :test #'string=))
      (fail "unknown preset ~s (available: ~{~a~^, ~})"
            name (mapcar #'first *presets*))))

(defun preset-names ()
  "The six built-in preset names, sorted."
  (sort (mapcar #'first *presets*) #'string<))

(defun preset-parsed (name)
  "The preset NAME as a parsed style: its rules, its palette as overrides,
and the preset it extends."
  (let ((entry (preset-entry name)))
    (list :preset (getf entry :extends)
          :palette nil
          :overrides (copy-tree (getf entry :palette))
          :rules (copy-tree (getf entry :rules))
          :notes ""
          :name nil
          :renderer (getf entry :renderer)
          :description (getf entry :description))))

;;; --- reading a STYLE.md ------------------------------------------------------------

(defun hex-colour-p (value)
  "Whether VALUE is a #rgb or #rrggbb colour."
  (and (stringp value)
       (> (length value) 1)
       (char= #\# (char value 0))
       (member (length value) '(4 7))
       (every (lambda (c) (digit-char-p c 16)) (subseq value 1))))

(defun expand-hex (value)
  "VALUE lowercased, with a #abc shorthand written #aabbcc."
  (let ((value (string-downcase value)))
    (if (= 4 (length value))
        (format nil "#~{~c~}" (mapcan (lambda (c) (list c c)) (coerce (subseq value 1) 'list)))
        value)))

(defun upcase-role (role)
  "ROLE as the palette's own spelling of it, or a refusal."
  (or (find role *palette-roles* :test #'string-equal)
      (fail "unknown palette role ~s (roles: ~{~a~^, ~})" role *palette-roles*)))

(defun strip-inline-comment (value)
  "VALUE up to an inline ` # comment', its ends and quotes trimmed."
  (let* ((cut (search " # " value))
         (head (if cut (subseq value 0 cut) value)))
    (string-trim '(#\Space #\Tab) (string-trim '(#\" #\' #\Space #\Tab) head))))

(defun style-key-p (key)
  "Whether KEY is a `key:' letter, digits and . _ -."
  (and (plusp (length key))
       (alpha-char-p (char key 0))
       (every (lambda (c) (or (alphanumericp c) (find c "._-"))) key)))

(defun split-style-line (line)
  "LINE as (values KEY VALUE), or NIL when it is not a `key: value' line."
  (let ((colon (position #\: line)))
    (when (and colon (plusp colon))
      (let ((key (string-downcase (substitute #\_ #\- (string-trim '(#\Space) (subseq line 0 colon))))))
        (when (style-key-p key)
          (values key (strip-inline-comment (subseq line (1+ colon)))))))))

(defun style-pair (parsed key value)
  "A known KEY sets its slot, palette.<role> an override, anything else a rule."
  (cond ((uiop:string-prefix-p "palette." key)
         (let ((role (upcase-role (subseq key 8))))
           (unless (hex-colour-p value)
             (fail "palette.~a must be a hex colour like #1a1020, got ~s" role value))
           (push (cons role (expand-hex value)) (getf parsed :overrides))))
        ((member key '("preset" "palette" "renderer" "name" "description") :test #'string=)
         (when (plusp (length value))
           (setf (getf parsed (intern (string-upcase key) :keyword))
                 (if (string= key "name") value (string-downcase value)))))
        ((plusp (length value)) (push (cons key value) (getf parsed :rules)))
        (t nil)))

(defun style-line (parsed line reading)
  "Read one LINE of a STYLE.md into PARSED, when READING is true. => the line
as a note when it is prose, else NIL."
  (let ((stripped (string-trim '(#\Space #\Tab) (string-left-trim "-* " line))))
    (cond ((zerop (length stripped)) nil)
          ((and reading (char= #\# (char stripped 0))) nil)
          (t (multiple-value-bind (key value) (and reading (split-style-line stripped))
               (if (null key) stripped (progn (style-pair parsed key value) nil)))))))

(defun parse-style-text (text)
  "TEXT -- a STYLE.md -- as a parsed style plist. Unknown keys are kept as
free-form rules; a heading, a comment or prose is a note."
  (when (> (length text) *max-style-bytes*)
    (fail "the STYLE.md is larger than ~d KiB" (round *max-style-bytes* 1024)))
  (let ((parsed (list :preset nil :palette nil :overrides '() :rules '() :notes ""
                       :name nil :renderer nil :description nil))
        (notes '())
        (fence nil))
    (dolist (line (uiop:split-string text :separator (list #\Newline)))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
        (if (uiop:string-prefix-p "```" trimmed)
            (setf fence (not fence))
            (let ((note (style-line parsed trimmed (not fence))))
              (when note (push note notes))))))
    (setf (getf parsed :notes) (format nil "~{~a~^~%~}" (nreverse notes))
          (getf parsed :overrides) (nreverse (getf parsed :overrides))
          (getf parsed :rules) (nreverse (getf parsed :rules)))
    parsed))

;;; --- resolving one, with preset inheritance -------------------------------------------

(defun resolve-parsed (parsed &key (depth 0) name)
  "PARSED with its preset chain resolved.
 => (values PALETTE PALETTE-NAME RULES NOTES RENDERER DESCRIPTION)."
  (when (> depth 4)
    (fail "preset inheritance is nested too deeply"))
  (let ((palette '()) (palette-name "") (rules '()) (notes "") (renderer "") (description ""))
    (let ((base-name (getf parsed :preset)))
      (when base-name
        (multiple-value-bind (p pn r n re d) (resolve-parsed (preset-parsed base-name) :depth (1+ depth))
          (setf palette p palette-name pn rules r notes n renderer re description d))
        (when (zerop (length palette-name)) (setf palette-name base-name))))
    (let ((pull (getf parsed :palette)))
      (when (and pull (not (string= pull palette-name)))
        (multiple-value-bind (p) (resolve-parsed (preset-parsed pull) :depth (1+ depth))
          (setf palette (copy-alist p))))
      (when pull (setf palette-name pull)))
    (dolist (pair (getf parsed :overrides))
      (let ((cell (assoc (car pair) palette :test #'string=)))
        (if cell (setf (cdr cell) (cdr pair)) (push (cons (car pair) (cdr pair)) palette))))
    (when (and name (getf parsed :overrides) (null (getf parsed :palette)))
      (setf palette-name name))
    (setf rules (append rules (getf parsed :rules)))
    (let ((more (getf parsed :notes)))
      (when (and more (plusp (length more)))
        (setf notes (if (plusp (length notes)) (format nil "~a~%~a" notes more) more))))
    (when (getf parsed :renderer) (setf renderer (getf parsed :renderer)))
    (when (getf parsed :description) (setf description (getf parsed :description)))
    (values palette palette-name rules notes renderer description)))

(defparameter *role-fallback*
  '(("bg2" . "bg") ("base2" . "base") ("metal2" . "metal") ("accent2" . "accent")
    ("wood2" . "wood") ("liquid2" . "liquid") ("sky2" . "sky") ("glow" . "accent")
    ("gem" . "accent") ("light" . "glow") ("highlight" . "light") ("shadow" . "ink")
    ("ground" . "bg2") ("foliage" . "base2") ("sky" . "bg") ("metal" . "highlight")
    ("wood" . "base2") ("liquid" . "accent") ("base" . "accent") ("accent" . "base"))
  "Where a role a custom palette left out takes its colour from.")

(defun finalize-palette (palette)
  "PALETTE with every role present: a missing one from its nearest sibling,
three passes deep, then the dark defaults."
  (let ((table (copy-alist palette)))
    (dolist (pair '(("bg" . "#14121c") ("ink" . "#0d0b12")
                    ("highlight" . "#ffffff") ("base" . "#8a7cff")))
      (unless (assoc (car pair) table :test #'string=) (push pair table)))
    (dotimes (pass 3)
      (declare (ignore pass))
      (dolist (role *palette-roles*)
        (unless (assoc role table :test #'string=)
          (let ((from (assoc role *role-fallback* :test #'string=)))
            (when (and from (assoc (cdr from) table :test #'string=))
              (push (cons role (cdr (assoc (cdr from) table :test #'string=))) table))))))
    (loop for role in *palette-roles*
          collect (cons role (or (cdr (assoc role table :test #'string=))
                                 (cdr (assoc "base" table :test #'string=)))))))

;;; --- loading a style ----------------------------------------------------------------

(defun load-preset (name)
  "The built-in preset NAME as a resolved style."
  (let ((name (string-downcase name)))
    (multiple-value-bind (palette palette-name rules notes renderer description)
        (resolve-parsed (preset-parsed name) :name name)
      (make-style :name (or (getf (preset-parsed name) :name) name)
                  :palette-name (if (plusp (length palette-name)) palette-name name)
                  :palette (finalize-palette palette)
                  :rules rules :notes notes :renderer renderer
                  :description description :source "preset"))))

(defun load-style-file (path)
  "The STYLE.md at PATH as a resolved style; it needs a preset: or palette: line."
  (let ((path (uiop:ensure-pathname path)))
    (unless (uiop:file-exists-p path)
      (fail "no STYLE.md at ~a" (namestring path)))
    (let ((parsed (parse-style-text (nlk:read-text path))))
      (multiple-value-bind (palette palette-name rules notes renderer description)
          (resolve-parsed parsed)
        (when (and (null palette) (null (getf parsed :overrides)))
          (fail "~a needs a 'preset:' or 'palette:' line (or palette.<role> colours)" (namestring path)))
        (make-style :name (or (getf parsed :name) (getf parsed :preset) "project")
                    :palette-name (if (plusp (length palette-name)) palette-name "custom")
                    :palette (finalize-palette palette)
                    :rules rules :notes notes :renderer renderer :description description
                    :source "file" :path (namestring path))))))

(defun procgen-style-p (line)
  "Whether LINE is a preset:, palette: or palette.<role>: line of ours."
  (let* ((line (string-left-trim "-* " (string-trim '(#\Space #\Tab #\Return) line)))
         (colon (position #\: line)))
    (and colon
         (let ((key (string-downcase (subseq line 0 colon))))
           (or (string= key "preset")
               (string= key "palette")
               (uiop:string-prefix-p "palette." key))))))

(defun looks-like-procgen-style (path)
  "Whether PATH is a STYLE.md of ours -- a repository may keep its own for prose."
  (let ((text (nlk:read-text path 8192)))
    (and text (some #'procgen-style-p (uiop:split-string text :separator (list #\Newline))))))

(defun find-project-style (cwd)
  "The STYLE.md of CWD -- .procgen/STYLE.md then STYLE.md -- when it is ours."
  (when cwd
    (loop for relative in '(".procgen/STYLE.md" "STYLE.md")
          for path = (merge-pathnames relative (uiop:ensure-directory-pathname cwd))
          when (and (uiop:file-exists-p path) (looks-like-procgen-style path))
            return path)))

(defun default-directory ()
  "The directory a render belongs to: the asking session's own, else this process's."
  (or (ignore-errors
        (and (stringp nlk:*scribe-session-id*)
             (nlk:find-session-cwd nlk:*scribe-session-id*)))
      (uiop:getcwd)))

(defun resolve-style (&key override style-file cwd)
  "The style one drawing runs on. Precedence: an explicit OVERRIDE (a preset
name, or a .md path) > the section's STYLE-FILE > a project STYLE.md in CWD
> the preset the section names."
  (let ((cwd (uiop:ensure-directory-pathname (or cwd (default-directory)))))
    (cond
      (override
       (let ((override (string-trim '(#\Space) override)))
         (cond ((or (uiop:string-suffix-p override ".md") (find #\/ override))
                (let ((path (uiop:ensure-pathname override)))
                  (load-style-file (if (uiop:absolute-pathname-p path) path (merge-pathnames path cwd)))))
               (t (load-preset override)))))
      ((and (stringp style-file) (plusp (length style-file)))
       (let ((path (uiop:ensure-pathname style-file)))
         (load-style-file (if (uiop:absolute-pathname-p path) path (merge-pathnames path cwd)))))
      ((find-project-style cwd) (load-style-file (find-project-style cwd)))
      (t (load-preset (setting :preset))))))

;;; --- what the model reads -------------------------------------------------------------

(defun style-contract (style)
  "STYLE as the drawing model reads it: the rules, then every palette role
with its colour and what it is for."
  (with-output-to-string (out)
    (format out "STYLE CONTRACT: ~a~@[ (~a)~]" (style-name style) (style-description style))
    (loop for (key . value) in (style-rules style)
          do (format out "~%- ~a: ~a" (substitute #\Space #\_ key) value))
    (when (plusp (length (style-renderer style)))
      (format out "~%- preferred renderer: ~a" (style-renderer style)))
    (format out "~%- palette (~a), read colours from the palette, never hard-code them:" (style-palette-name style))
    (dolist (role *palette-roles*)
      (let ((colour (cdr (assoc role (style-palette style) :test #'string=))))
        (when colour
          (format out "~%    ~a = ~a   # ~a" role colour (cdr (assoc role *role-hints* :test #'string=))))))
    (when (plusp (length (style-notes style)))
      (format out "~%- notes:")
      (dolist (line (uiop:split-string (style-notes style) :separator (list #\Newline)))
        (format out "~%    ~a" line)))))

(defun starter-style (name)
  "A STYLE.md for the preset NAME: its rules as editable lines, its palette
as the line that restyles every render in the directory."
  (let ((style (load-preset name)))
    (with-output-to-string (out)
      (format out "# STYLE.md: the art contract for every Procgen Studio render in this directory.~%")
      (format out "# Change one line and draw again; palette: alone restyles every render with no new drawing.~%")
      (format out "# Presets: ~{~a~^, ~}. Override single colours with palette.<role>: #hex.~%~%"
              (preset-names))
      (format out "preset: ~a~%palette: ~a~%" name (style-palette-name style))
      (dolist (pair (style-rules style))
        (format out "~a: ~a~%" (car pair) (cdr pair)))
      (format out "~%# palette.gem: #56e0ff~%# Free-form notes below go to the drawing model as they are.~%"))))
