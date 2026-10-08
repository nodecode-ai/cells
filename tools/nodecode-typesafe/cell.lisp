;;;; cell.lisp --- the manual, the settings, /typesafe, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; TypeSafe answers typed questions, not conversations, so it is no lane
;;;; and no catalog row: a catalog row would put jev-latest in /models as a
;;;; model a turn could pick and never run. It is two functions in the
;;;; typesafe: package the model calls through eval, the way it reaches the
;;;; web through web:search; while the cell runs, (help :typesafe) answers
;;;; the manual, and every request's help section carries one line naming
;;;; the verbs. /typesafe says where the key comes from, and checks it.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "typesafe": {"base_url": "", "model": ""}
;;;; Empty means TYPESAFE_BASE_URL or https://api.typesafe.ai, and
;;;; TYPESAFE_DEFAULT_MODEL or jev-latest, as omp reads them. The key is
;;;; auth.json's api_keys.typesafe, else TYPESAFE_API_KEY.

(in-package #:nodecode-typesafe)

(defparameter +manual+
  "TypeSafe judgments are available through the nodecode-typesafe cell: typed questions about one
state, answered as probabilities by TypeSafe's System One (POST /v1/systemone), called through eval.
  (typesafe:judge state questions &key model)
      STATE is a string, or a JSON object or array (a hash table, a plist like (:diff \"...\" :tests 3),
      a list). QUESTIONS maps ids to questions, as a plist or a JSON object; every question sees the
      same state and is answered on its own, so put independent questions in one call:
        (:risky (:type \"bool\" :instructions \"Does this change risk breaking callers?\"
                 :criteria (:true \"an API, a default or a format changes\" :false \"internal only\"))
         :kind  (:type \"choice\" :instructions \"What kind of change is this?\"
                 :criteria (:fix nil :feature nil :refactor \"restructures without changing behaviour\"))
         :size  (:type \"score\" :instructions \"How large is the change?\"
                 :criteria (\"trivial\" \"small\" \"medium\" \"large\")))
      A choice needs two or more options (keyword labels read lowercase; a JSON object keeps exact
      labels), each a rubric or nil; a bool's criteria are optional; a score's are two or more levels,
      lowest first. => (values ANSWERS MODEL USAGE): ANSWERS a plist of each id's keyword and
        (:type :bool :bool P-YES)
        (:type :choice :choice LABEL :probabilities ((LABEL . P) ...) :confidence C)
        (:type :score :score LEVEL :probabilities ((\"0\" . P) ...) :confidence C)
      read them with getf: (getf (getf answers :risky) :bool). LEVEL is probability-weighted and may
      land between levels (0 is the lowest).
  (typesafe:models)   the judgment models this key may use, one per line.
Use it to decide, rank or classify from evidence you already hold; it reads only the state you pass.
ERROR: TYPESAFE-ERROR names the problem: a refused question, no key, or the API's own answer."
  "What (help :typesafe) answers while the cell runs.")

;;; --- /typesafe -----------------------------------------------------------------------------

(defun status ()
  "Where the key comes from, the base and the model, in a line; no network."
  (multiple-value-bind (key source) (api-key)
    (format nil "typesafe: ~a; ~a, model ~a"
            (cond ((null key)
                   (format nil "no key (make one at ~a, then set ~a or save api_keys.typesafe in auth.json)"
                           +key-page+ +env+))
                  ((eq source :auth) "key from auth.json")
                  (t (format nil "key from ~a" +env+)))
            (base-url) (model))))

(defun check ()
  "Ask TypeSafe's model listing with the key, on a thread of its own, and
say once how it answered: omp's models-endpoint validation."
  (let ((key (api-key)))
    (if (null key)
        (status)
        (let ((settings *typesafe*))
          (bt2:make-thread
           (lambda ()
             (let ((*typesafe* settings))
               (nle:notice (handler-case (format nil "typesafe: the key answered; ~a"
                                                 (substitute #\, #\Newline (models)))
                             (typesafe-error (condition) (format nil "typesafe: ~a" condition)))
                           :level :info)))
           :name "nodecode-typesafe check")
          "typesafe: checking the key against /v1/models"))))

(defun run-slash (args session-id)
  "/typesafe status | check"
  (declare (ignore session-id))
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond ((member verb '("" "status") :test #'equal) (status))
          ((equal verb "check") (check))
          (t "usage: /typesafe status | check"))))

(defun complete-slash (text session-id)
  "The verbs /typesafe takes, those that start with TEXT."
  (declare (ignore session-id))
  (loop for verb in '("status" "check")
        when (uiop:string-prefix-p (string-downcase (nlk:trimmed (or text ""))) verb)
          collect (list :name verb :value verb)))

(nle:define-cell typesafe
  (:section ("typesafe")
    (:guide "make a key at https://console.typesafe.ai/; set TYPESAFE_API_KEY or save it in auth.json under api_keys.typesafe; base_url and model are optional (empty reads TYPESAFE_BASE_URL and TYPESAFE_DEFAULT_MODEL, else the public API and jev-latest)")
    ("base_url" :string :default ""
     :doc "the TypeSafe API root; empty is TYPESAFE_BASE_URL, else https://api.typesafe.ai")
    ("model" :string :default ""
     :doc "the judgment model; empty is TYPESAFE_DEFAULT_MODEL, else jev-latest"))
  (:help :typesafe "typesafe:judge answers typed questions (choice, bool, score) about a state; typesafe:models lists the judges" +manual+)
  (:command "typesafe" 'run-slash
   :description "TypeSafe judgments: where the key comes from; check it"
   :argument-hint "status | check"
   :session nil
   :complete 'complete-slash))
