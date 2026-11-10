;;;; cell-test.lisp --- the skill router against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every judgment call is a stubbed DEX:REQUEST (what NLK:HTTP ends in),
;;;; the roster is a stubbed NLK:KNOWLEDGE-ENTRIES over three files in a
;;;; temp directory, and the key is a stubbed NLE::CREDENTIAL-ENV: nothing
;;;; touches the network, the environment or the operator's skills.

(in-package #:nodecode.test)

(define-test-slice "dogmatic-skill-router" "DOGMATIC-SKILL-ROUTER-CELL-" :start nodecode-dogmatic-skill-router:start-cell)

(define-cell-lifecycle-tests "dogmatic-skill-router"
  (:help :dogmatic-skill-router)
  (:command "dogmatic-skill-router")
  (:hooks 'nle:submit 'nle::read-harness-sections)
  (:refused ("mode" 5) ("shortlist" "three"))
  (:idle dogmatic-skill-router:dogmatic-skill-router-error
         (dogmatic-skill-router:route "anything")
         (dogmatic-skill-router:skills)))

;;; --- what the far side answers ----------------------------------------------------

(defparameter +rank-reply+
  "{\"answers\":{\"which\":{\"type\":\"choice\",\"choice\":\"xlsx\",\"probabilities\":{\"xlsx\":0.62,\"pdf-fill\":0.22,\"sheet-copy\":0.1,\"none_of_these\":0.06},\"confidence\":0.5},\"gate::acts_on_user_system\":{\"type\":\"noul\",\"noul\":0.92},\"gate::would_follow_documented_procedure\":{\"type\":\"noul\",\"noul\":0.81},\"gate::prose_suffices\":{\"type\":\"noul\",\"noul\":0.2}},\"usage\":{\"input_tokens\":212}}"
  "Request 1's answers: the roster ranked, the gate open (mean 0.84).")

(defparameter +rerank-reply+
  "{\"answers\":{\"which\":{\"type\":\"choice\",\"choice\":\"xlsx\",\"probabilities\":{\"xlsx\":0.74,\"pdf-fill\":0.2,\"none_of_these\":0.06},\"confidence\":0.62},\"fits::xlsx\":{\"type\":\"noul\",\"noul\":0.86},\"fits::pdf-fill\":{\"type\":\"noul\",\"noul\":0.1},\"fits::sheet-copy\":{\"type\":\"noul\",\"noul\":0.2}},\"usage\":{\"input_tokens\":320}}"
  "Request 2's answers: xlsx chosen, and it fits.")

(defparameter +gated-reply+
  "{\"answers\":{\"which\":{\"type\":\"choice\",\"choice\":\"xlsx\",\"probabilities\":{\"xlsx\":0.4,\"pdf-fill\":0.3,\"sheet-copy\":0.2,\"none_of_these\":0.1}},\"gate::acts_on_user_system\":{\"type\":\"noul\",\"noul\":0.1},\"gate::would_follow_documented_procedure\":{\"type\":\"noul\",\"noul\":0.1},\"gate::prose_suffices\":{\"type\":\"noul\",\"noul\":0.9}},\"usage\":{}}"
  "Request 1's answers for a turn prose answers: the gate mean is 0.10.")

(defparameter +xlsx-text+
  "Extend every =SUM range when a column is added, then re-check the totals row before saving."
  "The skill the ranking re-reads.")

;;; --- fixtures -------------------------------------------------------------------

(defvar *router-directory* nil)

(defun router-directory ()
  "A temp directory of this run's own."
  (or *router-directory*
      (setf *router-directory*
            (ensure-directories-exist (merge-pathnames "dogmatic-skill-router-test/" (uiop:temporary-directory))))))

(defun router-file (name text)
  "TEXT written as NAME's file under ROUTER-DIRECTORY: => (values PATH LENGTH)."
  (let ((path (merge-pathnames (format nil "~a.lisp" name) (router-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string text out))
    (values path (length text))))

(defun router-entry (name description text)
  "NAME's knowledge entry, its file holding TEXT."
  (multiple-value-bind (path length) (router-file name text)
    (nlk::make-knowledge-entry :name name :operator "DEFINE-SKILL"
                               :description description
                               :path path :start 0 :end length)))

(defun router-skills ()
  "The three skills these tests route over."
  (list (router-entry "pdf-fill" "Fill a PDF form from a spreadsheet row" "Fill each field with the row's value.")
        (router-entry "sheet-copy" "Copy one worksheet into another workbook" "Copy the sheet, then fix the links.")
        (router-entry "xlsx" "Keep a spreadsheet's formulas right when rows or columns move" +xlsx-text+)))

(defun router-empty-auth ()
  "An auth file holding no key, so no test reads the operator's."
  (let ((path (merge-pathnames "auth.json" (router-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string "{}" out))
    path))

(defmacro with-router ((requests &key (replies '(list +rank-reply+ +rerank-reply+))
                                    (config nil) (env '(("TYPESAFE_API_KEY" . "ts-key"))))
                       &body body)
  "FORMS with the cell started on CONFIG, the key env as ENV, the roster as
ROUTER-SKILLS, and the far side answering REPLIES in turn (the last
repeated): REQUESTS each (METHOD URL HEADERS CONTENT) it was sent, oldest first."
  `(let ((nle::*auth-file-path* (router-empty-auth))
         (,requests '())
         (replies ,replies))
     (with-stubbed-fdefinitions
         ((nle::credential-env (name) (cdr (assoc name ',env :test #'equal)))
          (nlk:knowledge-entries () (router-skills))
          (dex:request (url &rest args)
                       (setf ,requests (append ,requests
                                               (list (list (getf args :method) url
                                                           (getf args :headers)
                                                           (getf args :content)))))
                       (values (cond ((rest replies) (pop replies))
                                     (t (first replies)))
                               200
                               (make-hash-table :test #'equal))))
       (with-cell-stop ((dogmatic-skill-router-start ,@config))
         ,@body))))

(defun router-header (headers name)
  "HEADERS' value for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun router-body (request)
  "REQUEST's content as a decoded JSON object."
  (nlk:decode-json (fourth request)))

(defun router-sections (session)
  "What READ-HARNESS-SECTIONS answers for SESSION, as the cell advises it."
  (nle::read-harness-sections session))

(defun router-line (session)
  "SESSION's skill_relevance line, or NIL."
  (cdr (assoc "skill_relevance" (router-sections session) :test #'equal)))

;;; --- request 1 then request 2 -----------------------------------------------------

(deftest dogmatic-skill-router-cell-asks-two-stages-and-names-one ()
  (with-router (requests)
    (let ((answer (dogmatic-skill-router:route "which sheet adds the column to the totals?")))
      (is (equal 2 (length requests)) "request 1 ranks, request 2 re-reads the shortlist")
      (destructuring-bind (method url headers content) (first requests)
        (declare (ignore content))
        (is (eq :post method))
        (is (equal "https://api.typesafe.ai/v1/systemone" url))
        (is (equal "Bearer ts-key" (router-header headers "authorization")))
        (is (equal "application/json" (router-header headers "content-type"))))
      (let* ((body (router-body (first requests)))
             (questions (nlk:json-value body :object "questions"))
             (criteria (nlk:json-value questions :object "which" "criteria")))
        (is (equal "which sheet adds the column to the totals?"
                   (nlk:json-value body :string "state" "request")) "the turn is the state")
        (is (equal "choice" (nlk:json-value questions :string "which" "type")))
        (dolist (name '("xlsx" "pdf-fill" "sheet-copy"))
          (is (nth-value 1 (gethash name criteria)) (format nil "~a is an option" name)))
        (is (null (nth-value 1 (gethash "none_of_these" criteria))) "one chunk nominates from all of them")
        (let ((kind (nlk:json-value questions :string "gate::prose_suffices" "type")))
          (is (equal "noul" kind) "a yes/no question is the wire's noul")))
      (let* ((body (router-body (second requests)))
             (questions (nlk:json-value body :object "questions"))
             (criteria (nlk:json-value questions :object "which" "criteria")))
        (is (stringp (nlk:json-value questions :string "fits::xlsx" "instructions")))
        (is (search "=SUM" (gethash "xlsx" criteria)) "the re-read carries the skill's own text")
        (is (nth-value 1 (gethash "none_of_these" criteria)) "the re-read can decline all three"))
      (is (search "suggested: xlsx" answer) "the verb names the one skill"))))

(deftest dogmatic-skill-router-cell-goes-quiet-when-prose-would-do ()
  (with-router (requests :replies '(list +gated-reply+))
    (let ((answer (dogmatic-skill-router:route "what does a pivot table do?")))
      (is (equal 1 (length requests)) "the gate closes before the second request")
      (is (search "no suggestion" answer)))))

(deftest dogmatic-skill-router-cell-leaves-a-slash-command-alone ()
  (with-router (requests)
    (let ((answer (dogmatic-skill-router:route "/dogmatic-skill-router skills")))
      (is (null requests) "a slash command picks its own flow")
      (is (search "slash" answer)))))

;;; --- the turn's one line ----------------------------------------------------------

(deftest dogmatic-skill-router-cell-hints-the-turn-it-routed ()
  (with-router (requests)
    (funcall (cell-hook "nodecode-dogmatic-skill-router" 'nle:submit)
             (lambda (&rest args) args)
             "dogmatic-skill-router-s1" "which sheet adds the column to the totals?")
    (let ((line (router-line "dogmatic-skill-router-s1")))
      (is (stringp line) "the routed turn carries one line")
      (is (search "xlsx" line))
      (is (search "ignore this if it does not fit" line) "the model stays in charge"))
    (funcall (cell-hook "nodecode-dogmatic-skill-router" 'nle:submit)
             (lambda (&rest args) args)
             "dogmatic-skill-router-s1" "/dogmatic-skill-router skills")
    (is (null (router-line "dogmatic-skill-router-s1")) "a turn left alone clears the line")))

(deftest dogmatic-skill-router-cell-says-nothing-without-a-key ()
  (with-router (requests :env nil)
    (is (search "no suggestion" (dogmatic-skill-router:route "which sheet adds the column?")))
    (is (null requests) "no key, nothing sent")))

(deftest dogmatic-skill-router-cell-refuses-a-keyless-on ()
  (with-router (requests :config ("mode" "on") :env nil)
    (let ((raised (handler-case (progn (dogmatic-skill-router:route "which sheet?") nil)
                    (dogmatic-skill-router:dogmatic-skill-router-error () t))))
      (is raised "an on router with no key names the missing setting"))
    (is (null requests))))

(deftest dogmatic-skill-router-cell-lists-its-roster ()
  (with-router (requests)
    (let ((text (dogmatic-skill-router:skills)))
      (is (search "xlsx - " text) "one line each, name then its sentence")
      (is (search "pdf-fill - " text))
      (is (null requests) "listing spends no judgment"))))
