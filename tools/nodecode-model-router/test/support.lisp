;;;; support.lisp --- the model-router test slice: a stubbed judge, a canned catalog.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the same nodecode.test registry every cell's do
;;;; (the core DEFTEST, with its hermetic machine-state posture) under a
;;;; MODEL-ROUTER-CELL- name prefix, so RUN-MODEL-ROUTER-TESTS runs exactly
;;;; this slice.
;;;;
;;;; The cell has two ways out of the process: the environment a row's key is
;;;; looked up in, and the judge's HTTP call. Neither is touched here: the
;;;; judge is *JUDGE*, replaced below, and the environment of a test run holds
;;;; no vendor key at all, which is itself the case the catalog's has_key
;;;; column reports.

(in-package #:nodecode.test)

(define-test-slice "model-router" "MODEL-ROUTER-CELL-" :start nodecode-model-router:start-cell)

;;; --- the stubbed judge -------------------------------------------------------------

(defvar *judge-answer* nil
  "What the stubbed judge answers: an alist of (ID . SCORE), or :fail.")

(defvar *judge-calls* '()
  "Every (URL KEY-ENV TASK) the stubbed judge was handed, newest first.")

(defun stub-judge (url key-env task entries timeout)
  "The judge seam, answering what a test names instead of dialling anything."
  (declare (ignore entries timeout))
  (push (list url key-env task) *judge-calls*)
  (if (eq *judge-answer* :fail)
      (nodecode-model-router::fail "the judge is down")
      *judge-answer*))

(defmacro with-judge (&body body)
  "Run BODY with the judge stubbed and its answers reset."
  `(with-saved-globals (nodecode-model-router::*judge* *judge-answer* *judge-calls*)
     (setf *judge-answer* nil
           *judge-calls* '()
           nodecode-model-router::*judge* #'stub-judge)
     ,@body))

(defmacro with-model-router ((&rest pairs) &body body)
  "Run BODY with the cell started under PAIRS as its `model-router' section,
its judge stubbed, and stopped on unwind."
  `(with-judge
     (with-cell-stop ((model-router-start ,@pairs))
       ,@body)))

;;; --- a catalog file ----------------------------------------------------------------

(defparameter +catalog-json+
  "{\"providers\": {\"harbour\": {\"base_url\": \"https://models.test/v1\",
     \"api_key_env\": \"HARBOUR_API_KEY\",
     \"models\": [{\"id\": \"harbour-small\", \"quality\": 0.4, \"cost\": 0.01, \"latency\": 0.05,
                   \"capabilities\": [\"chat\", \"json\"], \"context_window\": 32000},
                  {\"id\": \"harbour-big\", \"quality\": 0.93, \"cost\": 0.6, \"latency\": 0.3,
                   \"capabilities\": [\"chat\", \"tools\", \"coding\"]}]}}}"
  "A catalog file a test writes: one vendor the presets do not know.")

(defun catalogue-file ()
  "A fresh file holding +CATALOG-JSON+, in the test's scratch home."
  (let ((path (nlk:home (format nil "mr-catalog-~36r.json" (random (expt 36 6)
                                                                  (make-random-state t))))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string +catalog-json+ out))
    (namestring path)))

;;; --- asking the cell what a model would ---------------------------------------------

(defun call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun call-routing (&rest arguments)
  "model_route with ARGUMENTS as its JSON body; => the decoded answer object."
  (nlk:decode-json (call-tool "model_route" (nlk:encode-json-object
                                               (apply #'nlk:make-json-object arguments)))))

(defun answer-decision (answer)
  (nlk:json-value answer :string "decision"))

(defun answer-selected (answer)
  "The chosen row's id, or its abstain reason."
  (or (nlk:json-value (nlk:json-value answer :object "selected") :string "id")
      (nlk:json-value answer :string "abstain_reason")))

(defun answer-rejections (answer)
  "Every rejection as \"id reason\", in the order the answer lists them."
  (let ((rows (nlk:json-value answer :array "rejected")))
    (when rows
      (loop for row across rows
            collect (format nil "~a ~a" (nlk:json-value row :string "id")
                            (nlk:json-value row :string "reason"))))))

(defun answer-reasons (answer)
  (let ((reasons (nlk:json-value answer :array "reasons")))
    (when reasons (coerce reasons 'list))))

(defun selected-reasons (answer)
  (let ((reasons (nlk:json-value (nlk:json-value answer :object "selected") :array "reasons")))
    (when reasons (coerce reasons 'list))))
