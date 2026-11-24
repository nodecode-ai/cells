;;;; cell-test.lisp --- the jev cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json bound as the auth file, every key variable
;;;; a stubbed NLE::CREDENTIAL-ENV, every exchange with Jev a stubbed
;;;; dex:request (NLK:HTTP's own call): nothing touches the network, the
;;;; environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "jev" "JEV-CELL-" :start nodecode-jev:start-cell)

(define-cell-lifecycle-tests "jev"
  (:config "backend" "typesafe")
  (:help :jev)
  (:command "jev")
  (:refused ("backend" 5)))

(deftest jev-cell-verbs-refuse-while-not-running ()
  (let ((condition (handler-case (jev:triage "x") (condition (it) it))))
    (is (search "not running" (format nil "~a" condition))
        (format nil "the refusal reads: ~a" condition))))

;;; --- fixtures -------------------------------------------------------------------

(defun jev-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defparameter +jev-triage+
  "{\"model\":\"jev-20260901\",\"answers\":{\"category\":{\"type\":\"choice\",\"choice\":\"bug\",\"probabilities\":{\"bug\":0.6,\"feature\":0.2,\"research\":0.1,\"docs\":0.05,\"operations\":0.03,\"unknown\":0.02},\"confidence\":0.55},\"missing-context\":{\"type\":\"noul\",\"noul\":0.2},\"requires-approval\":{\"type\":\"noul\",\"noul\":0.8}},\"usage\":{\"input_tokens\":210,\"output_tokens\":0}}"
  "Jev's answer to the task_triage preset.")

(defparameter +jev-plus-triage+
  "{\"model\":\"jev-latest\",\"answers\":{\"category\":{\"type\":\"choice\",\"choice\":\"feature\",\"confidence\":0.62},\"missing-context\":{\"type\":\"noul\",\"noul\":0.48},\"requires-approval\":{\"type\":\"noul\",\"noul\":0.55}},\"usage\":{}}"
  "One step closer to the middle, to give the review heuristic something to say.")

(defparameter +jev-unknown+
  "{\"model\":\"jev-latest\",\"answers\":{\"category\":{\"type\":\"choice\",\"choice\":\"unknown\",\"confidence\":0.9},\"missing-context\":{\"type\":\"noul\",\"noul\":0.9},\"requires-approval\":{\"type\":\"noul\",\"noul\":0.1}},\"usage\":{}}"
  "An unknown category: the original flags it for a human whatever the threshold.")

(defparameter +jev-cloudflare-envelope+
  "{\"success\":true,\"result\":{\"model\":\"typesafe/jev\",\"answers\":{\"category\":{\"type\":\"choice\",\"choice\":\"docs\",\"confidence\":0.8},\"missing-context\":{\"type\":\"noul\",\"noul\":0.1},\"requires-approval\":{\"type\":\"noul\",\"noul\":0.1}},\"usage\":{}}}"
  "Cloudflare's answer, wrapped in the one result envelope the route uses.")

(defmacro with-jev ((requests &key (answers '(list +jev-triage+)) (statuses '(list 200))
                                 (env '(quote (("TYPESAFE_API_KEY" . "ts-env"))))
                                 (auth "{}") config)
                    &body forms)
  "FORMS with the cell started on CONFIG, AUTH as the auth file, ENV as the
environment, and Jev answering ANSWERS with STATUSES in turn (the last
repeated): REQUESTS each (METHOD URL HEADERS CONTENT) it was sent, oldest first."
  `(with-cell-stop ((jev-start ,@config))
     (with-temp-auth (auth ,auth)
       (let ((nle::*auth-file-path* auth) (,requests '())
             (answers ,answers) (statuses ,statuses))
         (with-stubbed-fdefinitions
             ((nle::credential-env (name) (cdr (assoc name ,env :test #'equal)))
              (dex:request (url &rest args)
                           (setf ,requests (append ,requests
                                                   (list (list (getf args :method) url (getf args :headers)
                                                               (getf args :content)))))
                           (values (if (rest answers) (pop answers) (first answers))
                                   (if (rest statuses) (pop statuses) (first statuses))
                                   (make-hash-table :test 'equal))))
           ,@forms)))))

(defun jev-near (a b)
  "Whether numbers A and B agree to a millionth."
  (and (realp a) (realp b) (< (abs (- a b)) 1d-6)))

(defun jev-body (request)
  "The decoded JSON body of REQUEST."
  (nlk:decode-json (fourth request)))

;;; --- a triage -------------------------------------------------------------------------

(deftest jev-cell-posts-the-system-one-request ()
  (with-jev (requests :config ("backend" "typesafe"))
    (jev:triage "The export button writes an empty CSV.")
    (destructuring-bind (method url headers content) (first requests)
      (declare (ignore content))
      (is (eq :post method))
      (is (equal "https://api.typesafe.ai/v1/systemone" url))
      (is (equal "Bearer ts-env" (jev-header headers "authorization")))
      (is (equal "application/json" (jev-header headers "accept"))))
    (let* ((body (jev-body (first requests)))
           (questions (nlk:json-value body :object "questions")))
      (is (equal "The export button writes an empty CSV." (nlk:json-value body :string "state")))
      (is (equal "jev-latest" (nlk:json-value body :string "model")) "the backend's default model")
      (is (equal "choice" (nlk:json-value questions :string "category" "type")))
      (is (= 6 (hash-table-count (nlk:json-value questions :object "category" "criteria"))) "the triage rubric carries its six options")
      (is (equal "noul" (nlk:json-value questions :string "missing-context" "type")) "bool is the wire's noul")
      (is (equal "noul" (nlk:json-value questions :string "requires-approval" "type"))))))

(deftest jev-cell-reads-each-answer ()
  (with-jev (requests :config ("backend" "typesafe"))
    (multiple-value-bind (answers review backend) (jev:triage "a diff")
      (is (equal '(:category :missing-context :requires-approval)
                 (loop for (id) on answers by #'cddr collect id)) "in the order asked, question ids as keywords")
      (let ((category (getf answers :category)))
        (is (eq :choice (getf category :type)))
        (is (equal "bug" (getf category :choice)))
        (is (equal '("bug" "feature" "research" "docs" "operations" "unknown")
                   (mapcar #'car (getf category :probabilities))))
        (is (jev-near 0.55 (getf category :confidence))))
      (is (eq :bool (getf (getf answers :missing-context) :type)))
      (is (jev-near 0.2 (getf (getf answers :missing-context) :bool)))
      (is (equal "typesafe/jev-20260901" backend) "the model that answered")
      (is (null (getf review :enabled)) "no threshold, no review")
      (is (null (getf review :review-questions))))))

(deftest jev-cell-flags-what-a-threshold-says ()
  (with-jev (requests :config ("backend" "typesafe" "review_threshold" "0.75")
                      :answers (list +jev-triage+ +jev-plus-triage+ +jev-unknown+))
    (let ((review (nth-value 1 (jev:triage "a diff"))))
      (is (getf review :enabled))
      (is (jev-near 0.75 (getf review :threshold)))
      (is (equal '(:category :requires-approval) (getf review :review-questions)) "a low confidence and a likely authorization both ask for a look"))
    (let ((review (nth-value 1 (jev:triage "a diff"))))
      (is (member :missing-context (getf review :review-questions)) "0.48 of yes sits inside the 0.25-0.75 band"))
    (let ((review (nth-value 1 (jev:triage "a diff"))))
      (is (member :category (getf review :review-questions)) "an unknown category is flagged even at 0.9 confidence")
      (is (member :missing-context (getf review :review-questions)) "0.9 of yes on missing context is over 1-threshold too"))))

(deftest jev-cell-reaches-cloudflare-with-the-gateway-headers ()
  (with-jev (requests
             :config ("backend" "cloudflare"
                       "account_id" "0123456789abcdef0123456789abcdef"
                       "gateway_id" "my-gateway")
             :env '(("CLOUDFLARE_JEV_API_TOKEN" . "cf-env"))
             :answers (list +jev-cloudflare-envelope+))
    (multiple-value-bind (answers review backend) (jev:triage "a diff")
      (destructuring-bind (method url headers content) (first requests)
        (declare (ignore content))
        (is (eq :post method))
        (is (equal "https://api.cloudflare.com/client/v4/accounts/0123456789abcdef0123456789abcdef/ai/run" url))
        (is (equal "Bearer cf-env" (jev-header headers "authorization")))
        (is (equal "false" (jev-header headers "cf-aig-collect-log")))
        (is (equal "my-gateway" (jev-header headers "cf-aig-gateway-id"))))
      (let* ((body (jev-body (first requests)))
             (input (nlk:json-value body :object "input")))
        (is (equal "typesafe/jev" (nlk:json-value body :string "model")) "cloudflare's default model")
        (is (equal "a diff" (nlk:json-value input :string "state")) "the state rides under input")
        (is (= 3 (hash-table-count (nlk:json-value input :object "questions")))))
      (is (equal "docs" (getf (getf answers :category) :choice)) "the result envelope comes off")
      (is (equal "cloudflare/typesafe/jev" backend))
      (is (null (getf review :enabled))))))

(deftest jev-cell-refuses-a-judgment-it-cannot-make ()
  (with-jev (requests :config ("backend" "typesafe")
                      :env '())
    (is (search "no typesafe key"
                (refusal-text jev:jev-error (jev:triage "x"))) "a key is required")
    (is (search "one of :preset and :questions"
                (refusal-text jev:jev-error (jev:ask "x"))) "ask needs a preset or questions")
    (is (search "exactly one"
                (refusal-text jev:jev-error
                              (jev:ask "x" :preset :task-triage
                                         :questions '(:a (:type "bool" :instructions "?"))))))
    (is (search "at least two levels"
                (refusal-text jev:jev-error
                              (jev:ask "x" :questions '(:a (:type "score" :instructions "?"
                                                              :criteria ("one"))))))))
  (with-jev (requests :config ("backend" "cloudflare" "account_id" "short")
                      :env '(("CLOUDFLARE_JEV_API_TOKEN" . "cf-env")))
    (is (search "account_id"
                (refusal-text jev:jev-error (jev:triage "x"))) "a cloudflare account id is 32 hex digits")))

(deftest jev-cell-says-what-it-is-configured-for ()
  (with-jev (requests :config ("backend" "typesafe"))
    (let ((line (jev:status)))
      (is (search "backend typesafe" line))
      (is (search "key from TYPESAFE_API_KEY" line))
      (is (search "heuristic off" line)))
    (is (search "task-triage" (jev:presets)))
    (is (search "relevance" (jev:presets)))
    (is (null requests) "status and presets never touch the network")))
