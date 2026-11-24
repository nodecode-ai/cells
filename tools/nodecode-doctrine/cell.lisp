;;;; cell.lisp --- the standing section, the /doctrine command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin put the nine articles in front of every model turn and
;;;; answered /doctrine with a table of which article an enforcer covers. Here
;;;; the same text rides every request as a standing harness section, and the
;;;; same table answers which article a cell of this harness carries -- the
;;;; enforcers the doctrine names are separate plugins there and separate cells
;;;; here, and this cell enforces nothing itself.

(in-package #:nodecode-doctrine)

;;; --- the manual ---------------------------------------------------------------
;;; (help :doctrine) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter +primer+
  "The Axiom Doctrine is nine articles of agent conduct: one owner and one
operator (I), what counts as an order (II), the four consequential actions that
need a yes -- send, spend, spin, scrub (III), proof over claims (IV), earned
autonomy (V), a record of every refusal (VI), a handoff before a long session
loses its footing (VII), messages that follow the owner's rules (VIII), and
secrets never read aloud (IX). The text is the original plugin's own file,
shipped here unchanged as doctrine.md.
  /doctrine status   each article, the enforcer plugin it names, and whether a
      cell carrying that enforcer is installed in this home: ENFORCED (names),
      else ADVISED. ENFORCED is an install state, not proof that anything stops.
  /doctrine show     the whole doctrine and the owner's additions.
  /doctrine show 3   one article, by number or roman numeral.
  /doctrine plugins  every enforcer the doctrine names, the cell that carries
      it, and whether that cell is installed.
Eval answers the same: doctrine:status-text, doctrine:show-text, doctrine:show-text
with roman numeral III, and doctrine:plugins-text.
The section in the config decides how much rides each request: form compact
(the axiom, each article's opening sentence and its state) or full (the whole
text and one state line an article), inject false to leave the request alone and
keep /doctrine, and additions: a file of the owner's own rules appended after
the articles. The cell reads doctrine.md and that one additions file; it writes
nothing, dials nothing and enforces nothing."
  "What (help :doctrine) answers while the cell runs.")

;;; --- the standing section -------------------------------------------------------

(defun standing-section (next session-id)
  "Every request's harness sections, with the doctrine's own block when the
section injects it; the text is read here, so a restart is how a rewritten
file is picked up."
  (let ((sections (remove "doctrine" (funcall next session-id) :key #'car :test #'string=)))
    (if (setting :inject)
        (acons "doctrine" (context-text) sections)
        sections)))

;;; --- /doctrine -------------------------------------------------------------------

(defun split-words (text)
  "TEXT split on spaces and tabs, empty words dropped."
  (let ((words '())
        (start 0)
        (size (length text)))
    (dotimes (index size)
      (when (member (char text index) '(#\Space #\Tab))
        (when (< start index) (push (subseq text start index) words))
        (setf start (1+ index))))
    (when (< start size) (push (subseq text start) words))
    (nreverse words)))

(defun run-slash (args session-id)
  "/doctrine typed alone or with status: the table. show: the text, or one
article by number or roman numeral. plugins: the enforcer report."
  (declare (ignore session-id))
  (let* ((words (split-words (or args "")))
         (head (string-downcase (or (first words) ""))))
    (cond ((or (string= head "") (string= head "status")) (status-text))
          ((and (string= head "show") (null (rest words))) (show-text))
          ((and (string= head "show") (null (rest (rest words)))) (show-text (second words)))
          ((string= head "plugins") (plugins-text))
          (t +usage+))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell doctrine
  (:section ("doctrine")
    (:guide "nothing here is required: the nine articles ship with the folder. form picks how much rides each request (compact or full); inject false keeps /doctrine and leaves the request alone; additions names a file of the owner's own rules, empty reads doctrine.md in this home; cells_dir is where installed cells sit, empty reads this home's cells folder")
    ("form" :choice :options '("compact" "full") :default "compact"
            :doc "how much of the doctrine rides each request: compact is the axiom, each article's opening sentence and its state, full is the whole text")
    ("inject" :boolean :default t
              :doc "whether the doctrine rides each request as a standing section; false keeps the /doctrine command")
    ("additions" :string
                 :doc "a file of the owner's own rules appended after the articles; empty reads doctrine.md in this home")
    ("cells_dir" :string
                 :doc "where installed cells sit, for the enforcer report; empty reads this home's cells folder"))
  (:hook 'nle::read-harness-sections #'standing-section)
  (:command "doctrine" 'run-slash
            :description "The nine-article conduct doctrine, and which enforcer cells are installed"
            :argument-hint "[status | show [1-9 or I-IX] | plugins]")
  (:help :doctrine
   "doctrine: the nine-article Axiom Doctrine in every request - /doctrine show"
   +primer+)
  (:start (lambda () (ensure-doctrine))))
