;;;; cell-test.lisp --- exact names, one asked lookup, abstention, corrections, refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every exchange with TypeSafe is a stubbed DEX:REQUEST, every key store a
;;;; scratch auth.json, every correction file a scratch JSON file: nothing
;;;; touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "slash-router" "SLASH-ROUTER-CELL-" :start nodecode-slash-router:start-cell)

(define-cell-lifecycle-tests "slash-router"
  (:help :slash-router)
  (:command "slash-router")
  (:refused ("model" 5) ("base_url" #(1)))
  (:idle slash-router:slash-router-error
         (slash-router:route "/hlep")
         (slash-router:remember "hlep" "help" "show the commands")
         (slash-router:status)))

;;; --- fixtures -------------------------------------------------------------------

(defun sr-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun sr-key-for (content name)
  "The option key the request CONTENT offers for the command NAME, else none:
what a Jev that understood the token would answer."
  (let* ((body (ignore-errors (nlk:decode-json content)))
         (criteria (and body (nlk:json-value body :object "questions" "route" "criteria"))))
    (or (and (hash-table-p criteria)
             (loop for key being the hash-keys of criteria using (hash-value text)
                   when (and (stringp text)
                             (eql 0 (search (format nil "/~a:" name) text)))
                     return key))
        "none")))

(defmacro with-router ((requests &key toward (confidence 0.93) (statuses 200)
                                 (env '(("TYPESAFE_API_KEY" . "sr-env"))) (auth "{}") config)
                       &body forms)
  "FORMS with the cell started on CONFIG (KEY VALUE pairs, as the lifecycle
tests take them), AUTH as the auth file, ENV as the environment, a scratch
correction file, and TypeSafe answering TOWARD's command at CONFIDENCE with
STATUSES: REQUESTS each (METHOD URL HEADERS CONTENT) it was sent, oldest
first."
  `(with-cell-stop ((slash-router-start ,@config))
     (with-temp-auth (auth ,auth)
       (with-temp-file (store :contents "{}")
         (let ((nle::*auth-file-path* auth)
               (nodecode-slash-router::*store* store)
               (nodecode-slash-router::*min-interval* 0)
               (nodecode-slash-router::*last-call* nil)
               (,requests '()))
           (with-stubbed-fdefinitions
               ((nle::credential-env (name)
                  (cdr (assoc name ',env :test #'equal)))
                (dex:request (url &rest args)
                             (let ((content (getf args :content)))
                               (setf ,requests
                                     (append ,requests
                                             (list (list (getf args :method) url
                                                         (getf args :headers) content))))
                               (values (format nil "{\"model\":\"jev-test\",\"answers\":{\"route\":{\"type\":\"choice\",\"choice\":~s,\"confidence\":~a}},\"usage\":{}}"
                                               (sr-key-for content ,toward)
                                               ,confidence)
                                       ,statuses
                                       (make-hash-table :test 'equal)))))
             ,@forms))))))

(defun sr-state (request)
  "The state object the request REQUEST carried, decoded from its JSON string."
  (nlk:decode-json (nlk:json-value (nlk:decode-json (fourth request)) :string "state")))

(defun sr-criteria (request)
  "The routing question's criteria in REQUEST, as JSON text."
  (nlk:encode-json-object
   (nlk:json-value (nlk:decode-json (fourth request)) :object "questions" "route" "criteria")))

;;; --- a command already spelled right ------------------------------------------------

(deftest slash-router-cell-exact-names-and-aliases-cost-no-lookup ()
  (with-router (requests)
    (is-carrying (text (slash-router:route "/models"))
                 "exact match" "/models is a live command" "nothing sent")
    (is-carrying (text (slash-router:route "/NEW"))
                 "/new is a live command" "alias of /clear")
    (is (null requests) "neither name nor alias was sent anywhere")))

;;; --- one lookup --------------------------------------------------------------------

(deftest slash-router-cell-one-lookup-asks-jev-with-the-live-catalog ()
  (with-router (requests :toward "help")
    (is-carrying (text (slash-router:route "/hlep"))
                 "/hlep -> /help" "0.93" "jev-test")
    (is (= 1 (length requests)) "one paid lookup per token")
    (destructuring-bind (method url headers content) (first requests)
      (declare (ignore content))
      (is (eq :post method))
      (is (equal "https://api.typesafe.ai/v1/systemone" url))
      (is (equal "Bearer sr-env" (sr-header headers "authorization")))
      (is (equal "application/json" (sr-header headers "content-type"))))
    (let ((body (nlk:decode-json (fourth (first requests)))))
      (is (equal "jev-1.13.0" (nlk:json-value body :string "model")) "the plugin's routing model")
      (is (equal "choice" (nlk:json-value body :string "questions" "route" "type")))
      (is (equal "hlep" (nlk:json-value (sr-state (first requests)) :string "mistyped_slash_command")))
      (is-carrying (text (sr-criteria (first requests))) "/help:" "/models:" "No clear equivalent"))))

(deftest slash-router-cell-the-section-names-the-base-and-the-model ()
  (with-router (requests :toward "help"
                         :config ("base_url" "http://jev.invalid:8011" "model" "dgemma"))
    (slash-router:route "/hlep")
    (is (equal "http://jev.invalid:8011/v1/systemone" (second (first requests))))
    (is (equal "dgemma" (nlk:json-value (nlk:decode-json (fourth (first requests))) :string "model")))
    (is-carrying (text (slash-router:status))
                 "http://jev.invalid:8011/v1/systemone" "dgemma")))

;;; --- abstention ---------------------------------------------------------------------

(deftest slash-router-cell-a-weak-answer-asks-instead-of-routing ()
  (with-router (requests :toward "help" :confidence 0.42)
    (is-carrying (text (slash-router:route "/hlep"))
                 "no route for /hlep" "/help" "0.42" "0.85" "(slash-router:remember")))

;;; --- what a person taught -------------------------------------------------------------

(deftest slash-router-cell-a-correction-is-a-hint-carried-with-the-token ()
  (with-router (requests :toward "models")
    (is-carrying (text (slash-router:remember "modles" "models" "open the model picker"))
                 "remembered: /modles -> /models" "open the model picker")
    (is-carrying (text (slash-router:route "/modles")) "/modles -> /models")
    (let ((state (sr-state (first requests))))
      (is (equal "models" (nlk:json-value state :string "prior_decision" "target")))
      (is (equal "correction" (nlk:json-value state :string "prior_decision" "origin")))
      (is (equal "open the model picker" (nlk:json-value state :string "prior_decision" "meaning"))))
    (slash-router:route "/mdl")
    (let ((examples (nlk:json-value (sr-state (second requests)) :array "correction_examples")))
      (is (= 1 (length examples)) "a related token carries the correction as one example")
      (is (equal "modles" (nlk:json-value (aref examples 0) :string "token"))))))

(deftest slash-router-cell-forget-drops-a-correction ()
  (with-router (requests)
    (slash-router:remember "modles" "models" "open the model picker")
    (is-carrying (text (slash-router:forget "/modles")) "forgot /modles")
    (is-carrying (text (slash-router:forget)) "no corrections were saved")
    (is (null requests))))

(deftest slash-router-cell-remember-refuses-what-is-not-live ()
  (with-router (requests)
    (is-carrying (text (refusal-text slash-router:slash-router-error
                                      (slash-router:remember "modles" "modelz" "pick a model")))
                 "not a live command" "models")
    (is-carrying (text (refusal-text slash-router:slash-router-error
                                      (slash-router:remember "modles" "models" "")))
                 "say what /modles should mean")))

;;; --- the key, and a base of one's own --------------------------------------------------

(deftest slash-router-cell-no-key-refuses-but-exact-still-answers ()
  (with-router (requests :env nil)
    (is-carrying (text (slash-router:route "/help")) "exact match")
    (is-carrying (text (refusal-text slash-router:slash-router-error
                                      (slash-router:route "/hlep")))
                 "no TypeSafe key" "TYPESAFE_API_KEY")
    (is (null requests) "nothing was sent without a key")))

(deftest slash-router-cell-a-base-of-your-own-takes-no-key ()
  (with-router (requests :env nil :toward "help"
                         :config ("base_url" "http://jev.invalid:8011"))
    (slash-router:route "/hlep")
    (is (= 1 (length requests)) "the lookup went")
    (is (null (sr-header (third (first requests)) "authorization")) "with no bearer")))

;;; --- a catalog of one's own ---------------------------------------------------------------

(deftest slash-router-cell-a-catalog-of-your-own-is-routed ()
  (with-router (requests :toward "widgets")
    (is-carrying (text (slash-router:route "/widgts"
                                            :commands '(("widgets" . "list the widgets")
                                                        ("gadgets" . "list the gadgets"))))
                 "/widgts -> /widgets")
    (is-carrying (text (sr-criteria (first requests)))
                 "/widgets: list the widgets" "/gadgets: list the gadgets")
    (is (not (search "/models:" (sr-criteria (first requests))))
        "the live catalog was not offered")))

;;; --- /slash-router --------------------------------------------------------------------

(deftest slash-router-cell-the-command-says-where-it-stands ()
  (with-router (requests :toward "help")
    (is-carrying (text (cell-entry "nodecode-slash-router" "slash-router" ""))
                 "slash-router:" "no lookup made")
    (is-carrying (text (cell-entry "nodecode-slash-router" "slash-router" "check"))
                 "the service answered" "/hlep -> /help")
    (is-carrying (text (cell-entry "nodecode-slash-router" "slash-router" "whatever"))
                 "not one of its choices")))
