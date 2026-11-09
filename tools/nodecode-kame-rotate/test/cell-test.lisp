;;;; cell-test.lisp --- the KAME rotation cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every key is a stubbed UIOP:GETENV, every endpoint a stubbed DEX:POST:
;;;; nothing reaches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "kame-rotate" "KAME-ROTATE-CELL-" :start nodecode-kame-rotate:start-cell)

(define-cell-lifecycle-tests "kame-rotate"
  (:config "base_url" "https://api.example.test/v1"
           "key_envs" (vector "KAME_TEST_KEY_A" "KAME_TEST_KEY_B"))
  (:help :kame)
  (:command "kame")
  (:running
   (is (search "kame:" (cell-entry "nodecode-kame-rotate" "kame" "status"))
       "the command reads the pool")
   (is (search "kame:" (cell-entry "nodecode-kame-rotate" "kame" ""))
       "and with no argument too"))
  (:refused ("timeout_seconds" "long") ("cooldown_seconds" 0))
  (:idle nodecode-kame-rotate:kame-rotate-error (nodecode-kame-rotate:status)))

;;; --- fixtures --------------------------------------------------------------------

(defparameter +kame-a+ "sk-test-a-1111" "The first key the environment holds.")

(defparameter +kame-b+ "sk-test-b-2222" "The second.")

(defparameter +served+ "{\"choices\":[{\"message\":{\"content\":\"the answer\"}}]}"
  "A chat-completions body carrying one answer.")

(defvar *kame-answers* '()
  "What DEX:POST answers next, oldest first: (STATUS TEXT HEADERS).")

(defvar *kame-asked* '()
  "Every call DEX:POST took, newest first: (URL HEADERS CONTENT).")

(defun kame-env (name)
  "What the environment answers for NAME: the two test keys, nothing else."
  (cond ((equal name "KAME_TEST_KEY_A") +kame-a+)
        ((equal name "KAME_TEST_KEY_B") +kame-b+)))

(defmacro with-kame-env (&body body)
  "BODY with UIOP:GETENV answering the two test keys."
  `(with-stubbed-fdefinition (uiop:getenv (name) (kame-env name))
     ,@body))

(defun kame-post (url &rest args)
  "DEX:POST's stand-in: log the call, answer the next scripted status."
  (push (list url (getf args :headers) (getf args :content)) *kame-asked*)
  (let ((answer (pop *kame-answers*)))
    (unless answer (error "unexpected request to ~a" url))
    (values (second answer) (first answer) (third answer))))

(defun kame-answer (status text &optional headers)
  "Script one answer to the next call."
  (setf *kame-answers* (append *kame-answers* (list (list status text headers)))))

(defmacro with-kame-wire (&body body)
  "BODY with DEX:POST answering the script DEX:POST's own vars hold."
  `(let ((*kame-answers* '())
         (*kame-asked* '()))
     (with-stubbed-fdefinition (dex:post (url &rest args) (apply #'kame-post url args))
       ,@body)))

(defun kame-header (headers name)
  "The value an alist HEADERS carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun kame-ask (prompt &key system model)
  "One KAME_CHAT call, the way the dispatch seam makes it."
  (nodecode-kame-rotate::tool-chat
   (apply #'nlk:make-json-object
          (loop for (key value) in `(("prompt" ,prompt) ("system" ,system) ("model" ,model))
                when value nconc (list key value)))))

;;; --- the call -------------------------------------------------------------------

(deftest kame-rotate-cell-serves-through-a-refused-key ()
  (with-kame-env
    (with-cell-stop ((kame-rotate-start "base_url" "https://api.example.test/v1"
                                        "key_envs" (vector "KAME_TEST_KEY_A" "KAME_TEST_KEY_B")
                                        "cooldown_seconds" 60))
      (with-kame-wire
        (kame-answer 429 "{\"error\":{\"message\":\"rate limit, slow down\"}}"
                     '(("retry-after" . "30")))
        (kame-answer 200 +served+ nil)
        (let ((answer (kame-ask "who?")))
          (is (search "the answer" answer) "the second key's content is the answer")
          (is (search "key 2 of 2" answer) "and the answer names the key that served it"))
        (let ((asked (reverse *kame-asked*)))
          (is (= 2 (length asked)) "an exhausted key and the next one")
          (destructuring-bind (url headers content) (first asked)
            (is (equal "https://api.example.test/v1/chat/completions" url)
                "/chat/completions is appended to the base URL")
            (is (equal (format nil "Bearer ~a" +kame-a+) (kame-header headers "authorization"))
                "the first call carries the first key")
            (is (equal "gpt-4o-mini" (nlk:json-value (nlk:decode-json content) :string "model"))
                "and the section's model when the call names none"))
          (destructuring-bind (url headers content) (second asked)
            (declare (ignore url content))
            (is (equal (format nil "Bearer ~a" +kame-b+) (kame-header headers "authorization"))
                "the second call carries the second key, not the first again")))
        (let ((state (nodecode-kame-rotate:status)))
          (is (search "cooling" state) "the refused key sits out")
          (is (search "rate limit, slow down" state) "with the endpoint's own words")
          (is (search "2. ready" state) "and the other key is ready"))))))

(deftest kame-rotate-cell-does-not-rotate-a-refusal-that-is-not-the-key ()
  (with-kame-env
    (with-cell-stop ((kame-rotate-start "base_url" "https://api.example.test/v1"
                                        "key_envs" (vector "KAME_TEST_KEY_A" "KAME_TEST_KEY_B")))
      (with-kame-wire
        (kame-answer 400 "{\"error\":{\"message\":\"no such model\"}}" nil)
        (kame-answer 200 +served+ nil)
        (let ((answer (kame-ask "who?" :model "nope")))
          (is (search "ERROR" answer) "a 400 is the call's failure, not a key's")
          (is (search "no such model" answer) "and the endpoint's words reach the model"))
        (is (= 1 (length *kame-asked*))
            "the next key is not asked a question the first was right to refuse")))))

(deftest kame-rotate-cell-refuses-a-section-without-a-base-url ()
  (with-kame-env
    (with-cell-stop ((kame-rotate-start "key_envs" (vector "KAME_TEST_KEY_A")))
      (let ((answer (kame-ask "who?")))
        (is (search "ERROR" answer) "the tool answers a failure, not a call")
        (is (search "base_url" answer) "naming the setting it needs")))))

(deftest kame-rotate-cell-tool-says-not-running ()
  (with-kame-env
    (let ((answer (kame-ask "who?")))
      (is (search "ERROR" answer) "a tool call with no cell running is a failure, not a crash")
      (is (search "not running" answer) "and says so, among the other keys")
      (is (search "kame-rotate" answer) "naming the section to fill in"))))

(deftest kame-rotate-cell-reset-forgets-the-pool ()
  (with-kame-env
    (with-cell-stop ((kame-rotate-start "base_url" "https://api.example.test/v1"
                                        "key_envs" (vector "KAME_TEST_KEY_A")))
      (with-kame-wire
        (kame-answer 429 "{}" '(("retry-after" . "30")))
        (kame-ask "who?")
        (is (search "cooling" (nodecode-kame-rotate:status)))
        (is (search "forgot" (cell-entry "nodecode-kame-rotate" "kame" "reset")))
        (is (search "1. ready" (nodecode-kame-rotate:status)) "reset leaves every key ready")))))
