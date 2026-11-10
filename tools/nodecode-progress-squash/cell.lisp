;;;; cell.lisp --- progress bars, shown once.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A progress bar redraws its one terminal line by writing a carriage return
;;;; and the next frame. A terminal shows the last frame; a captured stream
;;;; keeps every one of them, so a 300-step tqdm loop reaches the model as 300
;;;; copies of a bar nobody reads twice. This cell advises NLE:SH -- the one
;;;; function behind (sh "...") -- and rewrites the answer it hands back:
;;;; within each line, the carriage-return-separated segments of one progress
;;;; line are reduced to the last, and a footer says how many were dropped.
;;;;
;;;; The original plugin registers a transform_terminal_output hook in Hermes;
;;;; Nodecode has no such point, so the advice sits on the function. It sees
;;;; every caller of SH, the organism's own included, and touches nothing
;;;; outside a line that carries two frames of the same progress line.
;;;;
;;;; The rule is the original's, ported: two segments are the same progress
;;;; line when their STEM matches -- the visible text before the first digit
;;;; or bar glyph ("epoch:", "Receiving objects:"). A different stem, such as
;;;; an error printed between two redraws, keeps a line of its own, and a
;;;; carriage return that ends a line (CR LF) is a line ending, not a redraw.

(defpackage #:nodecode-progress-squash
  (:use #:cl))

(in-package #:nodecode-progress-squash)

(nlk:define-peripheral progress-squash :not-running t)

(defparameter *calls* 0
  "SH answers the advice has seen since this start.")

(defparameter *rewritten* 0
  "Of those, the ones it rewrote.")

(defparameter *frames* 0
  "Progress frames dropped in total, since this start.")

(defparameter +footer-format* "[progress-squash: ~:d overwritten progress frame~:p removed]"
  "What a rewritten output ends with, when the `footer' setting is on.")

(defparameter +escape+ (code-char 27) "The escape character that opens an ANSI sequence.")
(defparameter +bell+ (code-char 7) "What ends an OSC sequence written the short way.")
(defparameter +blank+ '(#\Space #\Tab #\Newline #\Return #\Page)
  "What counts as nothing but spacing.")
(defparameter +stem-glyphs+ "#=>|["
  "Where a progress bar's changing part begins: a digit or one of these.")
(defparameter +bar-glyphs+
  (list (cons #x2500 #x257F)   ; box drawing, the bar ─━
        (cons #x2580 #x259F))  ; block elements, the bar ▏█
  "The Unicode blocks a bar is often drawn with.")

;;; --- ANSI, which is still inside the frames here ---------------------------
;; A captured stream keeps the erase-line codes a bar writes, so the stem is
;; read off the visible text, never off the raw segment.

(defun ansi-sequence-length (text start)
  "The length of the ANSI sequence that starts at TEXT's START, or NIL when the
escape there begins none. The three shapes a captured stream carries: CSI
(ESC [), OSC (ESC ]), and the two-character escapes."
  (let ((n (length text)))
    (when (< (1+ start) n)
      (let ((kind (char text (1+ start))))
        (cond
          ((char= kind #\[)
           (let ((end (+ start 2)))
             (loop while (and (< end n) (char<= #\0 (char text end) #\?)) do (incf end))
             (loop while (and (< end n) (char<= #\Space (char text end) #\/)) do (incf end))
             (when (and (< end n) (char<= #\@ (char text end) #\~))
               (- end start -1))))
          ((char= kind #\])
           (let ((end (+ start 2)))
             (loop while (and (< end n)
                              (not (char= (char text end) +bell+))
                              (not (char= (char text end) +escape+)))
                   do (incf end))
             (cond ((>= end n) nil)
                   ((char= (char text end) +bell+) (- end start -1))
                   ((and (< (1+ end) n) (char= (char text (1+ end)) #\\))
                    (- (1+ end) start -1)))))
          ((char<= #\@ kind #\_) 2))))))

(defun strip-ansi (text)
  "TEXT with every ANSI sequence removed, what the terminal would show."
  (with-output-to-string (out)
    (let ((index 0) (n (length text)))
      (loop while (< index n)
            do (let ((length (and (char= (char text index) +escape+)
                                  (ansi-sequence-length text index))))
                 (if length
                     (incf index length)
                     (progn (write-char (char text index) out) (incf index))))))))

;;; --- the transform ---------------------------------------------------------

(defun split-on (text separator)
  "TEXT's pieces between SEPARATOR, in order."
  (let ((pieces '()) (start 0) (n (length text)))
    (loop for index from 0 below n
          when (char= (char text index) separator)
            do (push (subseq text start index) pieces)
               (setf start (1+ index)))
    (push (subseq text start n) pieces)
    (nreverse pieces)))

(defun join (strings separator)
  "STRINGS joined with SEPARATOR."
  (with-output-to-string (out)
    (loop for (string . rest) on strings
          do (write-string string out)
             (when rest (write-char separator out)))))

(defun stem-end-p (character)
  "Whether CHARACTER is where a progress line's stem ends: a digit, a bar glyph."
  (or (digit-char-p character)
      (find character +stem-glyphs+)
      (loop for (first . last) in +bar-glyphs+
            thereis (<= first (char-code character) last))))

(defun stem (visible)
  "VISIBLE's stem: the text before its first digit or bar glyph, trimmed."
  (let ((end (or (position-if #'stem-end-p visible) (length visible))))
    (string-trim +blank+ (subseq visible 0 end))))

(defun squash-segments (line keep)
  "LINE's carriage-return-separated segments reduced to the last frame of each
progress line. KEEP names stems never merged. => (values RENDERED DROPPED):
RENDERED the frames joined with newlines, DROPPED how many were overwritten."
  (let ((kept '()) (dropped 0))
    (dolist (segment (split-on line #\Return))
      (let ((visible (strip-ansi segment)))
        (unless (string= "" (string-trim +blank+ visible))
          (let* ((stem (stem visible))
                 (same (and kept
                            (not (and keep (member stem keep :test #'string-equal)))
                            (equal stem (cdr (first kept))))))
            (if same
                (progn (setf (first kept) (cons segment stem)) (incf dropped))
                (push (cons segment stem) kept))))))
    (if (plusp dropped)
        (values (join (mapcar #'car (nreverse kept)) #\Newline) dropped)
        (values line 0))))

(defun squash (output &key footer (min-frames 1) keep)
  "OUTPUT with each line's progress frames collapsed to the one a terminal shows.
FOOTER appends a line saying how many were dropped. => (values TEXT DROPPED):
TEXT the rewrite, or NIL when nothing changed; DROPPED the frames removed."
  (when (find #\Return output)
    (let ((lines (split-on output #\Newline)) (total 0))
      (loop for line in lines
            for index from 0
            for ending = (and (plusp (length line))
                              (char= #\Return (char line (1- (length line)))))
            for body = (if ending (subseq line 0 (1- (length line))) line)
            when (find #\Return body)
              do (multiple-value-bind (rendered dropped) (squash-segments body keep)
                   (when (and (plusp dropped) (>= dropped min-frames))
                     (setf (nth index lines)
                           (if ending
                               (concatenate 'string rendered (string #\Return))
                               rendered))
                     (incf total dropped))))
      (when (plusp total)
        (let ((result (join lines #\Newline)))
          (values (if footer
                      (concatenate 'string result
                                   (if (and (plusp (length result))
                                            (char= #\Newline (char result (1- (length result)))))
                                       ""
                                       (string #\Newline))
                                   (format nil +footer-format* total))
                      result)
                  total))))))

;;; --- the advice on SH ------------------------------------------------------

(defun squash-advice (next &rest arguments)
  "Advice on NLE:SH: the answer's output with its progress frames collapsed,
and every other value the call answered untouched."
  (let ((answer (multiple-value-list (apply next arguments))))
    (let ((output (first answer)))
      (when (stringp output)
        (incf *calls*)
        (multiple-value-bind (rewritten dropped)
            (handler-case
                (squash output
                        :footer (setting :footer)
                        :min-frames (setting :min-frames)
                        :keep (setting :keep))
              (error () nil))
          (when rewritten
            (incf *rewritten*)
            (incf *frames* (or dropped 0))
            (setf (first answer) rewritten)))))
    (values-list answer)))

;;; --- /progress-squash ------------------------------------------------------

(defun run-slash (arguments session-id)
  "One /progress-squash: what this run has squashed so far, or `reset'."
  (declare (ignore session-id))
  (let ((argument (string-downcase (nlk:trimmed (or arguments "")))))
    (if (member argument '("reset" "clear" "zero") :test #'string=)
        (progn
          (setf *calls* 0 *rewritten* 0 *frames* 0)
          "progress-squash: counters cleared")
        (format nil "progress-squash: ~:d sh result~:p seen, ~:d rewritten, ~:d progress frame~:p dropped; footer ~:[off~;on~], min_frames ~:d, ~:d kept stem~:p.~%`reset` clears the counters; (help 'progress-squash) says what it leaves alone."
                *calls* *rewritten* *frames*
                (setting :footer) (setting :min-frames) (length (setting :keep))))))

(defparameter +manual+
  "progress-squash: a progress bar's redraws, shown once. A bar redraws its
one line by writing a carriage return and the next frame; this cell advises
the sh function, so a (sh \"...\") result that carries several frames of one
progress line keeps only the frame a terminal would show, and a last line
says how many were dropped. Two segments count as one progress line when
the text before their first digit or bar glyph matches (\"epoch:\",
\"Receiving objects:\"); anything else on the line, an error printed between
two redraws for instance, keeps a line of its own. Output with no carriage
return is unchanged, byte for byte; so is a carriage return that ends a
line (CR LF). The setting footer turns the last line off, min_frames decides
how many overwritten frames of a line are enough to collapse it, and keep
names stems to leave as they are. /progress-squash reports what this run has
squashed, and /progress-squash reset clears the counters. The advice sits on
sh itself, so the organism's own shell calls go through it too.")

(nle:define-cell progress-squash
  (:section ("progress-squash")
    (:guide "nothing is required: the folder is on as it stands; min_frames 2 keeps a single overwrite, keep names a stem left alone")
    ("footer" :boolean :default t
     :doc "end a rewritten output with a line saying how many progress frames were dropped")
    ("min_frames" :integer :default 1 :min 1
     :doc "how many overwritten frames of one line before that line is collapsed")
    ("keep" :list
     :doc "progress-line stems left as they are, such as epoch:"))
  (:hook 'nle:sh #'squash-advice)
  (:help :progress-squash
         "progress-squash: terminal progress bars shown once, not once per redraw"
         +manual+)
  (:command "progress-squash" 'run-slash
            :description "How much progress-bar output this run has squashed")
  (:start (lambda ()
            (setf *calls* 0 *rewritten* 0 *frames* 0))))
