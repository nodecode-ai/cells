;;;; cell-test.lisp --- the Jev skill router against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every exchange with the backend is a stubbed *POST* answering a JSON
;;;; text, the roster is three skills, and the adapter token is one test
;;;; variable: nothing here touches the network or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "jev-skill-router" "JEV-SKILL-ROUTER-CELL-"
  :start nodecode-jev-skill-router:start-cell)

(define-cell-lifecycle-tests "jev-skill-router"
  (:config "adapter_url" "https://adapter.example.test"
           "adapter_token_env" "JEV_ROUTER_TEST_TOKEN")
  (:help :jev-skill-router)
  (:command "jev-skill-router")
  (:refused ("shortlist" "many")))

;;; --- fixtures -------------------------------------------------------------------

(defparameter +roster+
  '(("pdf-split" "split a PDF into one file per page" "Run qpdf --split-pages on the file.")
    ("queue-records" "queue the album a friend recommended on the kitchen speaker"
     "Ask for the artist, then the record, then the room.")
    ("tldr-communication" "explain something in plain terms" "Say it in four sentences, no jargon."))
  "Three skills, as SKILL-ROSTER answers them: name, description, own text.")

(defparameter +stage-one+
  "{\"model\":\"jev\",\"answers\":{\"which\":{\"type\":\"choice\",\"choice\":\"queue-records\",\"probabilities\":{\"queue-records\":0.61,\"pdf-split\":0.05,\"tldr-communication\":0.04,\"none_of_these\":0.30}},\"gate::acts_on_user_system\":{\"type\":\"noul\",\"noul\":0.92},\"gate::would_follow_documented_procedure\":{\"type\":\"noul\",\"noul\":0.61},\"gate::prose_suffices\":{\"type\":\"noul\",\"noul\":0.20}},\"usage\":{\"total_tokens\":321}}"
  "Stage one's answer: queue-records leads and the gate is (0.92 + 0.61 + 0.80) / 3.")

(defparameter +stage-one-quiet+
  "{\"model\":\"jev\",\"answers\":{\"which\":{\"type\":\"choice\",\"choice\":\"none_of_these\",\"probabilities\":{\"queue-records\":0.61,\"pdf-split\":0.05,\"tldr-communication\":0.04}},\"gate::acts_on_user_system\":{\"type\":\"noul\",\"noul\":0.30},\"gate::would_follow_documented_procedure\":{\"type\":\"noul\",\"noul\":0.20},\"gate::prose_suffices\":{\"type\":\"noul\",\"noul\":0.80}}}"
  "Stage one's answer from a conversational turn: the gate is 0.23.")

(defparameter +stage-two+
  "{\"model\":\"jev\",\"answers\":{\"which\":{\"type\":\"choice\",\"choice\":\"queue-records\",\"probabilities\":{\"queue-records\":0.71,\"pdf-split\":0.11,\"none_of_these\":0.18}},\"fits::queue-records\":{\"type\":\"noul\",\"noul\":0.88},\"fits::pdf-split\":{\"type\":\"noul\",\"noul\":0.06},\"fits::tldr-communication\":{\"type\":\"noul\",\"noul\":0.03}}}"
  "Stage two's answer: queue-records wins and fits.")

(defparameter +stage-two-unfit+
  "{\"model\":\"jev\",\"answers\":{\"which\":{\"type\":\"choice\",\"choice\":\"pdf-split\",\"probabilities\":{\"queue-records\":0.21,\"pdf-split\":0.22,\"none_of_these\":0.57}},\"fits::queue-records\":{\"type\":\"noul\",\"noul\":0.11},\"fits::pdf-split\":{\"type\":\"noul\",\"noul\":0.19},\"fits::tldr-communication\":{\"type\":\"noul\",\"noul\":0.04}}}"
  "Stage two's answer when no candidate does the specific thing asked.")

(defparameter +adapter+ "https://adapter.example.test"
  "The endpoint of the operator's own every test in adapter mode sends to.")

(defparameter +token-variable+ "JEV_ROUTER_TEST_TOKEN"
  "The variable the test's section names for the adapter's bearer token.")

(defun router-call-with (function &key (answers (list +stage-one+ +stage-two+))
                                  (config (list "adapter_url" +adapter+
                                                "adapter_token_env" +token-variable+))
                                  (roster +roster+))
  "Call FUNCTION with one argument -- a thunk answering the (URL HEADERS BODY)
it was sent, oldest first -- inside a started cell on CONFIG, with *POST*
answering ANSWERS in turn (the last repeated) and the roster ROSTER."
  (with-cell-stop ((apply #'jev-skill-router-start config))
    (let ((sent '()) (answers answers) (saved (uiop:getenv +token-variable+)))
      (unwind-protect
           (progn
             (setf (uiop:getenv +token-variable+) "adapter-secret")
             (let ((nodecode-jev-skill-router::*post*
                     (lambda (url headers body timeout)
                       (declare (ignore timeout))
                       (push (list url headers body) sent)
                       (values (if (rest answers) (pop answers) (first answers)) 200)))
                   (nodecode-jev-skill-router::*roster* roster))
               (funcall function (lambda () (reverse sent)))))
        (setf (uiop:getenv +token-variable+) (or saved ""))))))

(defun answer-of (text &optional key)
  "TEXT decoded as an answer object, or its KEY."
  (let ((decoded (nlk:decode-json text)))
    (if key (gethash key decoded) decoded)))

(defun no-skill-p (answer)
  "Whether ANSWER named no skill: NIL, or the decoder's own reading of JSON null."
  (let ((value (gethash "suggested" answer)))
    (or (null value) (eq value :null))))

(defun sent-questions (sent)
  "The questions object the first request in SENT carries, decoded."
  (nlk:json-value (nlk:decode-json (third (first sent))) :object "questions"))

(defun sent-header (sent name)
  "The value NAME's header carries in the first request of SENT."
  (cdr (assoc name (second (first sent)) :test #'string-equal)))

;;; --- one selection ---------------------------------------------------------------------

(deftest jev-skill-router-cell-names-one-skill ()
  (router-call-with
   (lambda (sent)
     (let* ((text (nodecode-jev-skill-router::select-json
                   "queue the new record on the kitchen speaker"))
            (answer (answer-of text)))
       (is (equal "suggest" (gethash "decision" answer)))
       (is (equal "queue-records" (gethash "suggested" answer)))
       (is (= 2 (length (funcall sent))) "one request per stage")
       (is (equal (concatenate 'string +adapter+ "/") (first (first (funcall sent)))) "adapter mode sends to the adapter's root")
       (is (equal "Bearer adapter-secret" (sent-header (funcall sent) "authorization")))
       (is (equal "queue the new record on the kitchen speaker"
                  (nlk:json-value (nlk:decode-json (third (first (funcall sent))))
                                  :string "state" "request")))
       (is (equal "jev-latest" (nlk:json-value (nlk:decode-json (third (first (funcall sent))))
                                                :string "model")))
       (let ((questions (sent-questions (funcall sent))))
         (is (equal "choice" (nlk:json-value questions :string "which" "type")) "stage one is one choice over the roster")
         (is (search "queue the album" (third (first (funcall sent)))) "the roster's own index line is the rubric")
         (is (equal "noul" (nlk:json-value questions :string "gate::acts_on_user_system" "type")) "the three request judgments ride the first call"))
       (let ((narrow (nlk:decode-json (third (second (funcall sent))))))
         (is (search "SKILL.md: Ask for the artist" (third (second (funcall sent)))) "stage two shows a candidate's own text")
         (is (search "None of these" (nlk:json-value narrow :string "questions" "which"
                                                      "criteria" "none_of_these")) "stage two offers no match"))
       (is (search "<skill_relevance>" (gethash "suggestion_block" answer)))
       (is (equal "queue-records" (gethash "suggested" (answer-of text)))))
     (let ((answer (answer-of (nodecode-jev-skill-router::select-json
                               "queue the new record on the kitchen speaker"))))
       (is (numberp (gethash "gate" answer)) "the gate rides the answer")))))

(deftest jev-skill-router-cell-abstains-under-the-gate ()
  (router-call-with
   (lambda (sent)
     (let ((answer (answer-of (nodecode-jev-skill-router::select-json
                               "what is a cloud function"))))
       (is (equal "abstain" (gethash "decision" answer)))
       (is (no-skill-p answer))
       (is (search "below" (gethash "reason" answer)))
       (is (= 1 (length (funcall sent))) "below the gate no second call is spent")))
   :answers (list +stage-one-quiet+)))

(deftest jev-skill-router-cell-abstains-when-nothing-fits ()
  (router-call-with
   (lambda (sent)
     (let ((answer (answer-of (nodecode-jev-skill-router::select-json
                               "queue the new record"))))
       (is (equal "abstain" (gethash "decision" answer)))
       (is (no-skill-p answer))
       (is (search "no candidate fits" (gethash "reason" answer)))
       (is (= 2 (length (funcall sent))))))
   :answers (list +stage-one+ +stage-two-unfit+)))

(deftest jev-skill-router-cell-refuses-without-a-key ()
  (router-call-with
   (lambda (sent)
     (let ((answer (answer-of (nodecode-jev-skill-router::select-json
                               "queue the new record"))))
       (is (equal "abstain" (gethash "decision" answer)))
       (is (search "no key" (gethash "error" answer)) "a call with nothing to send names what to set")
       (is (null (funcall sent)) "and spends nothing")))
   :config (list "api_key_env" "JEV_ROUTER_TEST_KEY_UNSET")))

(deftest jev-skill-router-cell-requires-a-request ()
  (router-call-with
   (lambda (sent)
     (let ((answer (answer-of (nodecode-jev-skill-router::select-json "  "))))
       (is (search "request required" (gethash "error" answer)))
       (is (null (funcall sent)) "an empty request is refused before the call")))))

(deftest jev-skill-router-cell-wraps-the-line ()
  (let ((block (nodecode-jev-skill-router::suggestion-block "queue-records")))
    (is (search "<skill_relevance>" block))
    (is (search "queue-records" block))
    (is (search "Ignore this if it does not fit" block))
    (is (search "</skill_relevance>" block))))

(deftest jev-skill-router-cell-answers-its-command ()
  (router-call-with
   (lambda (sent)
     (let ((status (nodecode-jev-skill-router::run-slash "" nil)))
       (is (search "3 skills kept" status))
       (is (search "gate 0.30" status)))
     (let ((line (nodecode-jev-skill-router::run-slash
                  "queue the new record on the kitchen speaker" nil)))
       (is (search "queue-records" line))
       (is (search "fits 0.88" line))
       (is (= 2 (length (funcall sent))))))))
