;;;; cell-test.lisp --- the acedatacloud-media cell against a stubbed Ace Data
;;;; Cloud.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Ace Data Cloud request is a stubbed NLK:HTTP and every key a
;;;; stubbed NLE::CREDENTIAL-ENV: nothing reaches api.acedata.cloud, the
;;;; environment or the operator's files. Each tool is called the way a model's
;;;; call reaches it - through the real dispatch, contract and all - so the
;;;; schema the provider is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "acedatacloud-media" "ACEDATACLOUD-MEDIA-CELL-")

;;; --- fixtures --------------------------------------------------------------------

(defun acedatacloud-media-config (&rest pairs)
  "A whole shared config holding the `acedatacloud-media' section PAIRS make."
  (nlk:json-object "acedatacloud-media" (apply #'nlk:make-json-object pairs)))

(defun acedatacloud-media-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-acedatacloud-media:start-cell (apply #'acedatacloud-media-config pairs)))

(defmacro with-key ((&optional (key "adc-secret") (name "ADC_KEY")) &body body)
  "BODY with the credential seam answering KEY for the variable NAME."
  `(with-stubbed-fdefinition (nle::credential-env (variable)
                              (and (equal variable ,name) ,key))
     ,@body))

(defmacro with-media ((&rest pairs) &body body)
  "A started cell whose section is PAIRS, its key in ADC_KEY, waiting one
second between checks and none before answering a task."
  `(with-cell-stop ((acedatacloud-media-start "api" "https://api.acedata.test"
                                               "api_key_env" "ADC_KEY"
                                               "wait_seconds" 0
                                               "poll_seconds" 1
                                               ,@pairs))
     ,@body))

(defmacro with-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD and URL bound and answers (values TEXT STATUS).
ASKED collects (METHOD URL OPTIONS), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url options) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected Ace Data Cloud request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun tool-text (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun tool-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (tool-text id tool arguments)))

(defun payload-of (request)
  "The JSON payload REQUEST, a stub's (METHOD URL OPTIONS), sent."
  (nlk:decode-json (getf (third request) :content)))

(defun header-of (request name)
  (cdr (assoc name (getf (third request) :headers) :test #'equal)))

(defun answer-urls (answer)
  (let ((urls (gethash "urls" answer)))
    (if (vectorp urls) (coerce urls 'list) '())))

(defparameter +submitted-image+
  "{\"task_id\":\"task-image-1\"}"
  "What Ace Data Cloud answers a submitted picture.")

(defparameter +submitted-video+
  "{\"task_id\":\"task-video-1\"}"
  "What Ace Data Cloud answers a submitted video.")

(defparameter +finished-image+
  "{\"task_id\":\"task-image-1\",\"finished_at\":1730000000,\"response\":{\"success\":true,\"data\":[{\"image_url\":\"https://cdn.acedata.test/one.png\"}]}}"
  "What Ace Data Cloud answers for a finished picture.")

(defparameter +finished-video+
  "{\"task_id\":\"task-video-1\",\"finished_at\":1730000000,\"response\":{\"success\":true,\"data\":{\"video_url\":\"https://cdn.acedata.test/one.mp4\",\"duration\":4}}}"
  "What Ace Data Cloud answers for a finished video.")

(defparameter +running+
  "{\"task_id\":\"task-video-1\",\"finished_at\":null}"
  "What Ace Data Cloud answers for a render still going.")

(defparameter +search+
  "{\"success\":true,\"data\":{\"organic\":[{\"title\":\"Ace Data Cloud\",\"link\":\"https://example.com/a\",\"snippet\":\"media APIs\"}]}}"
  "What Ace Data Cloud answers a Google search.")

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "acedatacloud-media"
  (:config "api" "https://api.acedata.test")
  (:help :acedatacloud-media)
  (:command "media")
  (:refused ("api" "ftp://api.acedata.test")
             ("request_timeout" 1)
             ("poll_seconds" 0))
  (:idle nodecode-acedatacloud-media:acedatacloud-media-error
         (nodecode-acedatacloud-media:acedatacloud-image nil)
         (nodecode-acedatacloud-media:acedatacloud-task nil)))

;;; --- a picture ---------------------------------------------------------------------

(deftest acedatacloud-media-cell-makes-a-picture ()
  (with-key ()
    (with-media ()
      (with-wire (asked
                  ("/nano-banana/tasks" (values +finished-image+ 200))
                  ("/nano-banana/images" (values +submitted-image+ 200)))
        (let ((answer (tool-answer "c1" "acedatacloud_image"
                                    "{\"prompt\":\"a red fox in snow\",\"size\":\"16:9\"}")))
          (is (equal "succeeded" (nlk:json-value answer :string "status")))
          (is (equal '("https://cdn.acedata.test/one.png") (answer-urls answer)) "the picture's URL")
          (let* ((submit (first asked))
                 (payload (payload-of submit)))
            (is (eq :post (first submit)))
            (is (equal "https://api.acedata.test/nano-banana/images" (second submit)))
            (is (equal "nano-banana-2-lite" (nlk:json-value payload :string "model")))
            (is (equal "Bearer adc-secret" (header-of submit "authorization")) "a bearer token")
            (is (equal "a red fox in snow" (nlk:json-value payload :string "prompt"))))
          (let* ((poll (second asked))
                 (payload (payload-of poll)))
            (is (equal "https://api.acedata.test/nano-banana/tasks" (second poll)))
            (is (equal "task-image-1" (nlk:json-value payload :string "id")))))))))

(deftest acedatacloud-media-cell-edits-a-reference-picture ()
  (with-key ()
    (with-media ()
      (with-wire (asked
                  ("/nano-banana/tasks" (values +finished-image+ 200))
                  ("/nano-banana/images" (values +submitted-image+ 200)))
        (tool-answer "c2" "acedatacloud_image"
                      "{\"prompt\":\"make it night\",\"service\":\"nano-banana\",\"image_urls\":[\"https://example.com/a.png\"]}")
        (let ((payload (payload-of (first asked))))
          (is (equal "edit" (nlk:json-value payload :string "action")) "reference images make it an edit")
          (is (equal '("https://example.com/a.png")
                     (coerce (gethash "image_urls" payload) 'list))))))))

;;; --- a video, and the task it leaves behind -------------------------------------------

(deftest acedatacloud-media-cell-makes-a-video ()
  (with-key ()
    (with-media ()
      (with-wire (asked
                  ("/seedance/tasks" (values +finished-video+ 200))
                  ("/seedance/videos" (values +submitted-video+ 200)))
        (let ((answer (tool-answer "c3" "acedatacloud_video"
                                    "{\"prompt\":\"a kite over a hill\",\"duration\":5}")))
          (is (equal "succeeded" (nlk:json-value answer :string "status")))
          (is (equal '("https://cdn.acedata.test/one.mp4") (answer-urls answer)))
          (let* ((payload (payload-of (first asked)))
                 (part (aref (gethash "content" payload) 0)))
            (is (equal 5 (nlk:json-value payload :integer "duration")))
            (is (equal "text" (nlk:json-value part :string "type")) "Seedance takes the prompt in its content array")
            (is (equal "a kite over a hill" (nlk:json-value part :string "text")))))))))

(deftest acedatacloud-media-cell-answers-a-running-task ()
  (with-key ()
    (with-media ()
      (with-wire (asked
                  ("/seedance/tasks" (values +running+ 200)))
        (let ((answer (tool-answer "c4" "acedatacloud_task"
                                    "{\"service\":\"seedance\",\"task_id\":\"task-video-1\"}")))
          (is (equal "pending" (nlk:json-value answer :string "status")))
          (is (equal "task-video-1" (nlk:json-value answer :string "task_id")))
          (is (search "acedatacloud_task" (nlk:json-value answer :string "next_action")) "and says how to finish it")
          (is (equal "retrieve" (nlk:json-value (payload-of (first asked)) :string "action"))))))))

;;; --- music, search, and the key --------------------------------------------------------

(deftest acedatacloud-media-cell-refuses-an-unfinished-song ()
  (with-key ()
    (with-media ()
      (with-wire (asked ("/suno/audios" (values "{}" 200)))
        (let ((text (tool-text "c5" "acedatacloud_audio"
                               "{\"prompt\":\"a song about rain\",\"service\":\"suno\",\"custom\":true}")))
          (is (search "title" text) "custom mode names what is missing")
          (is (null asked) "and nothing was sent"))))))

(deftest acedatacloud-media-cell-searches-the-web ()
  (with-key ()
    (with-media ()
      (with-wire (asked ("/serp/google" (values +search+ 200)))
        (let ((answer (tool-answer "c6" "acedatacloud_lookup"
                                    "{\"mode\":\"web\",\"query\":\"ace data cloud\",\"number\":5}")))
          (is (equal "succeeded" (nlk:json-value answer :string "status")))
          (is (equal "google-search" (nlk:json-value answer :string "service")))
          (let ((payload (payload-of (first asked))))
            (is (equal "ace data cloud" (nlk:json-value payload :string "query")))
            (is (equal 5 (nlk:json-value payload :integer "number")))
            (is (equal "search" (nlk:json-value payload :string "type")))))))))

(deftest acedatacloud-media-cell-refuses-without-a-key ()
  (with-stubbed-fdefinition (nle::credential-env (variable)
                              (declare (ignore variable))
                              nil)
    (with-media ()
      (with-wire (asked ("/nano-banana/images" (values +submitted-image+ 200)))
        (let ((text (tool-text "c7" "acedatacloud_image" "{\"prompt\":\"a red fox\"}")))
          (is (search "no Ace Data Cloud key" text))
          (is (null asked) "nothing was sent and nothing was billed"))))))