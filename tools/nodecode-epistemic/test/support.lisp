;;;; support.lisp --- the epistemic test slice: a scratch store, a stubbed turn, the two hooks by hand.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing of the cell is stubbed but the two seams it reads: NLE:TURN,
;;;; which names the session a tool call belongs to outside a real turn, and
;;;; NLK:RECORD-TURN-CONTEXT-PROMPT, which is where a reminder would land.
;;;; The cases are real files under the run's own scratch home, one directory
;;;; a test, removed afterwards.

(in-package #:nodecode.test)

(define-test-slice "epistemic" "EPISTEMIC-CELL-" :start nodecode-epistemic:start-cell)

(defvar *dir-seq* 0
  "How many stores this run has made; the case directory's own name is this.")

(defmacro with-epistemic ((&rest pairs) &body body)
  "BODY with the cell started, its cases in a directory of this test's own
under the scratch home, and the section on PAIRS besides dir."
  `(let ((dir (format nil "epistemic-test/~d" (incf *dir-seq*))))
     (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree (nlk:home dir)
                                                                    :validate t)))
       (with-cell-stop ((epistemic-start "dir" dir ,@pairs))
         ,@body))))

(defmacro as-session ((session) &body body)
  "BODY with the running turn's session id SESSION."
  `(with-stubbed-fdefinition (nle:turn () (list :session-id ,session))
     ,@body))

(defun epistemic-tool (name &rest pairs)
  "The tool NAME as the engine calls it: the declared contract decoded, then
the body, the answer decoded."
  (let ((entry (nle::find-tool name)))
    (nlk:decode-json
     (funcall (third entry)
              (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs))))))

(defun case-answer (&rest pairs)
  "The epistemic_case tool's answer, decoded."
  (apply #'epistemic-tool "epistemic_case" pairs))

(defun probe-answer (&rest pairs)
  "The epistemic_probe tool's answer, decoded."
  (apply #'epistemic-tool "epistemic_probe" pairs))

(defun ok-p (answer) (and (gethash "ok" answer) t))

(defun answer-text (answer)
  "Every text ANSWER carries, for a search."
  (with-output-to-string (out)
    (format out "~a ~a"
            (or (gethash "text" answer) "")
            (or (gethash "error" answer) ""))))

(defmacro with-turn-prompts ((collected) &body body)
  "BODY with every reminder the cell would put in a request pushed onto
COLLECTED, newest first."
  `(let ((,collected '()))
     (with-stubbed-fdefinition (nlk:record-turn-context-prompt (turn &key text)
                                (declare (ignore turn))
                                (push text ,collected))
       ,@body)
     (reverse ,collected)))

(defun round-op (&key (session "s1") (turn "t1"))
  "A model-request (the :ROUND point) of SESSION in TURN."
  (list :session-id session :turn turn))

(defun open-a-case (&key (session "s1") (id "cfg-change"))
  "A case opened on SESSION as a test's first move."
  (as-session (session)
    (case-answer "operation" "open" "case_id" id
                 "decision" "Use the reload hypothesis only if the log supports it"
                 "top_unknown" "whether the config reload precedes the first error"
                 "stopping_condition" "one log window compared against the prediction"
                 "claims" (vector "the config reload caused the failure")
                 "plan" (vector "read the log around the reload"))))
