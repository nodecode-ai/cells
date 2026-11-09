;;;; cell-test.lisp --- the skill router against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every exchange with TypeSafe is a stubbed *POST* answering a JSON text,
;;;; the roster is three skills, and a turn's request text is a stubbed
;;;; *TURN-TEXT*: nothing here touches the network, the environment or the
;;;; operator's files.

(in-package #:nodecode.test)

(define-test-slice "skill-router" "SKILL-ROUTER-CELL-" :start nodecode-skill-router:start-cell)

(define-cell-lifecycle-tests "skill-router"
  (:config "base_url" "https://api.example.test")
  (:hooks :round)
  (:help :skill-router)
  (:command "skill-router")
  (:refused ("shortlist" "three")))

;;; --- fixtures -------------------------------------------------------------------

(defparameter +roster+
  '(("queue-records" "queue the album a friend recommended on the kitchen speaker"
     "Ask for the artist, then the record, then the room.")
    ("pdf-split" "split a PDF into one file per page" "Run qpdf --split-pages on the file.")
    ("tldr-communication" "explain something in plain terms" "Say it in four sentences, no jargon."))
  "Three skills, as SKILL-ROSTER answers them: name, description, own text.")

(defparameter +stage-one+
  "{\"model\":\"jev\",\"answers\":{\"which\":{\"type\":\"choice\",\"choice\":\"none_of_these\",\"probabilities\":{\"queue-records\":0.61,\"pdf-split\":0.05,\"tldr-communication\":0.04,\"none_of_these\":0.30}},\"gate::acts_on_user_system\":{\"type\":\"noul\",\"noul\":0.92},\"gate::would_follow_documented_procedure\":{\"type\":\"noul\",\"noul\":0.61},\"gate::prose_suffices\":{\"type\":\"noul\",\"noul\":0.20}}}"
  "Stage one's answer: queue-records leads the chunk and the gate is (0.92 + 0.61 + 0.80) / 3.")

(defparameter +stage-one-quiet+
  "{\"model\":\"jev\",\"answers\":{\"which\":{\"type\":\"choice\",\"choice\":\"none_of_these\",\"probabilities\":{\"queue-records\":0.61,\"pdf-split\":0.05,\"tldr-communication\":0.04}},\"gate::acts_on_user_system\":{\"type\":\"noul\",\"noul\":0.30},\"gate::would_follow_documented_procedure\":{\"type\":\"noul\",\"noul\":0.20},\"gate::prose_suffices\":{\"type\":\"noul\",\"noul\":0.80}}}"
  "Stage one's answer from a conversational turn: the gate is 0.23.")

(defparameter +stage-two+
  "{\"model\":\"jev\",\"answers\":{\"which\":{\"type\":\"choice\",\"choice\":\"queue-records\",\"probabilities\":{\"queue-records\":0.71,\"pdf-split\":0.11,\"none_of_these\":0.18}},\"fits::queue-records\":{\"type\":\"noul\",\"noul\":0.88},\"fits::pdf-split\":{\"type\":\"noul\",\"noul\":0.06},\"fits::tldr-communication\":{\"type\":\"noul\",\"noul\":0.03}}}"
  "Stage two's answer: queue-records wins and fits.")

(defun router-call-with (function &key (answers (list +stage-one+ +stage-two+))
                                  (config '("base_url" "https://api.example.test"))
                                  (roster +roster+) (texts nil) (status 200))
  "Call FUNCTION with one argument — a thunk answering the (URL HEADERS BODY)
it was sent, oldest first — inside a started cell on CONFIG, with *POST*
answering ANSWERS in turn (the last repeated), the roster ROSTER, and
*TURN-TEXT* answering TEXTS in turn."
  (with-cell-stop ((apply #'skill-router-start config))
    (let ((sent '()) (answers answers) (remaining texts))
      (let ((nodecode-skill-router::*post*
              (lambda (url headers body timeout)
                (declare (ignore timeout))
                (push (list url headers body) sent)
                (values (if (rest answers) (pop answers) (first answers)) status)))
            (nodecode-skill-router::*roster* roster)
            (nodecode-skill-router::*turn-text*
              (lambda (turn) (declare (ignore turn)) (pop remaining))))
        (funcall function (lambda () (reverse sent)))))))

(defun sent-questions (sent)
  "The questions object the first request in SENT carries, decoded."
  (nlk:json-value (nlk:decode-json (third (first sent))) :object "questions"))

(defun sent-body (sent)
  "The body of the first request in SENT, decoded."
  (nlk:decode-json (third (first sent))))

(defun sent-header (sent name)
  "The value NAME's header carries in the first request of SENT."
  (cdr (assoc name (second (first sent)) :test #'string-equal)))

;;; --- one routing decision ---------------------------------------------------------------

(deftest skill-router-cell-names-one-skill ()
  (router-call-with
   (lambda (sent)
     (let ((result (nodecode-skill-router::route-request "queue the new record on the kitchen speaker")))
       (is (equal "queue-records" (getf result :skill)))
       (is (= 2 (length (funcall sent))) "one request per stage")
       (is (equal "https://api.example.test/v1/systemone" (first (first (funcall sent)))))
       (is (equal "queue the new record on the kitchen speaker"
                  (nlk:json-value (sent-body (funcall sent)) :string "state" "request")))
       (is (equal "jev-latest" (nlk:json-value (sent-body (funcall sent)) :string "model")))
       (let ((questions (sent-questions (funcall sent))))
         (is (equal "choice" (nlk:json-value questions :string "which" "type")) "one choice over the roster")
         (is (search "queue the album" (third (first (funcall sent)))) "the roster's own line is the rubric")
         (is (equal "noul" (nlk:json-value questions :string "gate::acts_on_user_system" "type")) "three request judgments ride the first chunk"))
       (let ((narrow (nlk:decode-json (third (second (funcall sent))))))
         (is (search "None of these" (nlk:json-value narrow :string "questions" "which" "criteria" "none_of_these")) "stage two offers no match"))))))

(deftest skill-router-cell-stays-quiet-under-the-gate ()
  (router-call-with
   (lambda (sent)
     (let ((result (nodecode-skill-router::route-request "queue the new record")))
       (is (null (getf result :skill)))
       (is (= 1 (length (funcall sent))) "below the gate no second request is spent")))
   :answers (list +stage-one-quiet+)))

(deftest skill-router-cell-says-nothing-without-a-key ()
  (router-call-with
   (lambda (sent)
     (let ((result (nodecode-skill-router::route-request "queue the new record")))
       (is (null (getf result :skill)))
       (is (null (funcall sent)) "no key and no base of its own: nothing is sent")))
   :config '()))

(deftest skill-router-cell-leaves-long-requests-and-commands-alone ()
  (router-call-with
   (lambda (sent)
     (is (null (getf (nodecode-skill-router::route-request
                       (make-string 5000 :initial-element #\x))
                      :skill)))
     (is (null (getf (nodecode-skill-router::route-request "/skill-router status") :skill)))
     (is (null (funcall sent)) "neither a command nor an over-long request is routed"))))

(deftest skill-router-cell-carries-the-key-when-one-is-configured ()
  (let ((saved (uiop:getenv "SKILL_ROUTER_TEST_KEY")))
    (unwind-protect
         (progn
           (setf (uiop:getenv "SKILL_ROUTER_TEST_KEY") "sk-test")
           (router-call-with
            (lambda (sent)
              (nodecode-skill-router::route-request "queue the new record")
              (is (equal "Bearer sk-test" (sent-header (funcall sent) "authorization"))))
            :config '("base_url" "https://api.example.test"
                      "api_key_env" "SKILL_ROUTER_TEST_KEY")))
      (setf (uiop:getenv "SKILL_ROUTER_TEST_KEY") (or saved "")))))

;;; --- the line, and the round it lands in ------------------------------------------------

(deftest skill-router-cell-wraps-the-line ()
  (let ((block (nodecode-skill-router::suggestion-block "queue-records")))
    (is (search "<skill_relevance>" block))
    (is (search "queue-records" block))
    (is (search "Ignore this if it does not fit" block))
    (is (search "</skill_relevance>" block))))

(deftest skill-router-cell-records-the-line-on-the-first-round ()
  (router-call-with
   (lambda (sent)
     (let ((recorded '()))
       (let ((nodecode-skill-router::*record*
               (lambda (turn text) (push (list turn text) recorded))))
         (nodecode-skill-router::record-suggestion "turn-1" 0)
         (is (= 2 (length (funcall sent))) "the hook routes the turn's request")
         (is (equal "turn-1" (first (first recorded))))
         (is (search "queue-records" (second (first recorded))))
         (setf recorded '())
         (nodecode-skill-router::record-suggestion "turn-1" 1)
         (is (null recorded) "later rounds of the same turn say nothing more"))))
   :texts (list "queue the new record on the kitchen speaker")))

;;; --- the operator's side -----------------------------------------------------------------

(deftest skill-router-cell-answers-its-command ()
  (router-call-with
   (lambda (sent)
     (let ((status (nodecode-skill-router::run-slash "" nil)))
       (is (search "3 skills kept" status))
       (is (search "gate 0.30" status)))
     (let ((line (nodecode-skill-router::run-slash "queue the new record on the kitchen speaker" nil)))
       (is (search "queue-records" line))
       (is (= 2 (length (funcall sent))))))))
