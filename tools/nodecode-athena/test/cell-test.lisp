;;;; cell-test.lisp --- the athena cell against a stubbed store and launcher.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every catalogue read is a stubbed NLK:HTTP, and every program this cell
;;;; would run -- git clone, the app's start command, kill -- is a stubbed seam,
;;;; so nothing here reaches the network or starts a process. Each verb is called
;;;; the way a model's call reaches it, through the real dispatch, contract and
;;;; all, so the schema the provider is shown and the body that runs are one
;;;; shape.

(in-package #:nodecode.test)

;;; --- the scratch folder and the slice that defaults to it -------------------------

(defvar *athena-scratch* 0)

(defun athena-test-apps-dir ()
  "A folder of this test's own, so one test's apps are never another's."
  (namestring
   (ensure-directories-exist
    (merge-pathnames (format nil "athena-test-~d-~d/"
                              (get-universal-time) (incf *athena-scratch*))
                     (uiop:temporary-directory)))))

(define-test-slice "athena" "ATHENA-CELL-" :recipe "athena" :start nodecode-athena:start-cell
  :defaults (("apps_dir" (athena-test-apps-dir))
             ("catalog_url" "https://store.test/apps")))

;;; --- fixtures --------------------------------------------------------------------

(defparameter +catalogue+
  (concatenate
   'string
   "{\"items\":[{\"id\":1,\"slug\":\"voice-cloner\",\"title\":\"Voice Cloner\","
   "\"description\":\"Clone any voice from a minute of audio.\",\"version\":\"2.1\","
   "\"repoUrl\":\"https://github.com/example/voice-cloner\","
   "\"tags\":[{\"tag\":\"voice\",\"count\":9},{\"tag\":\"audio\",\"count\":4}]},"
   "{\"app\":{\"id\":2,\"slug\":\"image-upscaler\",\"title\":\"Image Upscaler\","
   "\"description\":\"Upscale an image four times.\",\"version\":\"1.0\","
   "\"repoUrl\":\"https://github.com/example/image-upscaler\",\"tags\":[]}}],"
   "\"total\":2,\"totalPages\":1}")
  "Two catalogue rows, as the store's API answers them: one bare, one wrapped in app.")

(defmacro with-athena-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values TEXT
STATUS), as NLK:HTTP does. ASKED collects (METHOD URL), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (declare (ignorable options))
                                   (push (list method url) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected catalogue request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun athena-apps-dir ()
  "The apps folder the running cell was started with."
  (namestring (nodecode-athena::apps-root)))

(defun athena-make-app (id &rest files)
  "An installed app ID in the running cell's folder, carrying FILES as start scripts."
  (let ((folder (merge-pathnames (format nil "~a/" id)
                                  (uiop:ensure-directory-pathname (athena-apps-dir)))))
    (ensure-directories-exist (merge-pathnames "touch" folder))
    (dolist (file files)
      (with-open-file (out (merge-pathnames file folder) :direction :output
                                                         :if-exists :supersede
                                                         :if-does-not-exist :create)
        (format out "#!/bin/sh~%")))
    folder))

(defun athena-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun athena-object (id tool arguments)
  "TOOL's answer, decoded; an answer that is not JSON says itself."
  (let ((text (athena-answer id tool arguments)))
    (handler-case (nlk:decode-json text)
      (error () (error "answer was not JSON: ~a" text)))))

;;; --- the lifecycle, from the one declaration --------------------------------------

(define-cell-lifecycle-tests "athena"
  (:config)
  (:help :athena)
  (:command "athena")
  (:refused ("apps_dir" "") ("timeout" 0)))

(deftest athena-cell-without-a-folder-refuses ()
  ;; The one setting no verb can work without: a section naming none is
  ;; refused at start, before anything installs.
  (let ((condition (signals-error nlk:config-refusal (athena-start "apps_dir" ""))))
    (is (search "apps_dir is not set" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "athena_search")) "and installs nothing")))

;;; --- athena_search ---------------------------------------------------------------

(deftest athena-cell-searches-the-catalogue ()
  (with-cell-stop ((athena-start))
    (with-athena-wire (asked ("/apps" (values +catalogue+ 200)))
      (let ((object (athena-object "athena-1" "athena_search" "{\"query\":\"voice clone\"}")))
        (is (= 2 (nlk:json-value object :integer "count")) "both rows come back, bare and wrapped")
        (let ((apps (nlk:json-array object "apps")))
          (is (equal "Voice Cloner" (nlk:json-value (aref apps 0) :string "name")))
          (is (equal "https://github.com/example/voice-cloner"
                     (nlk:json-value (aref apps 0) :string "install_with")) "install_with is the repo to clone")
          (is (equal "voice" (aref (nlk:json-value (aref apps 0) :array "tags") 0)) "tags come through as names"))
        (is (= 1 (length asked)) "one search is one request")
        (is (eq :get (first (first asked))))
        (is (search "q=voice%20clone" (second (first asked))) "the query rides the URL, encoded")))))

(deftest athena-cell-search-refuses-when-the-store-is-down ()
  (with-cell-stop ((athena-start))
    (with-athena-wire (asked ("/apps" (values "{\"error\":\"maintenance\"}" 503)))
      (let ((answer (athena-answer "athena-2" "athena_search" "{\"query\":\"upscaler\"}")))
        (is (search "HTTP 503" answer) "the store's own status is named")
        (is (search "maintenance" answer) "and its own words")))))

(deftest athena-cell-search-refuses-a-query-the-store-does-not-know ()
  (with-cell-stop ((athena-start))
    (with-athena-wire (asked ("/apps" (values "{\"items\":[],\"total\":0}" 200)))
      (let ((answer (athena-answer "athena-3" "athena_search" "{\"query\":\"nothing at all\"}")))
        (is (search "no app for" answer) "an empty catalogue is said, not returned as a blank list")))))

(deftest athena-cell-refuses-a-call-with-no-query ()
  (with-cell-stop ((athena-start))
    (with-athena-wire (asked)
      (let ((answer (athena-answer "athena-4" "athena_search" "{}")))
        (is (search "preflight" answer) "the contract refuses it before the body")
        (is (null asked) "and nothing is asked of the store")))))

;;; --- athena_install --------------------------------------------------------------

(deftest athena-cell-installs-by-cloning ()
  (with-cell-stop ((athena-start))
    (let ((asked '()))
      (let ((nodecode-athena::*run*
              (lambda (argv &key directory)
                (declare (ignore directory))
                (push argv asked)
                ;; what a successful git clone leaves behind
                (let ((target (car (last argv))))
                  (ensure-directories-exist (merge-pathnames "x" target))
                  (with-open-file (out (merge-pathnames "start.sh" target)
                                        :direction :output :if-exists :supersede
                                        :if-does-not-exist :create)
                    (format out "#!/bin/sh~%")))
                (values "Cloning into 'voice-cloner'..." 0))))
        (let ((object (athena-object "athena-5" "athena_install"
                                      "{\"url\":\"https://github.com/example/voice-cloner\"}")))
          (is (equal "voice-cloner" (nlk:json-value object :string "app")) "the folder is the repo's name")
          (is (equal "sh start.sh" (nlk:json-value object :string "script")) "the start script it found is named")
          (is (= 1 (length asked)))
          (is (equal '("git" "clone" "--depth" "1" "https://github.com/example/voice-cloner")
                     (butlast (first asked))) "a shallow clone of the named repo")
          (is (search "voice-cloner" (car (last (first asked)))) "into the folder the app takes"))))))

(deftest athena-cell-install-resolves-a-catalogue-name ()
  (with-cell-stop ((athena-start))
    (with-athena-wire (asked ("/apps" (values +catalogue+ 200)))
      (let ((nodecode-athena::*run*
              (lambda (argv &key directory)
                (declare (ignore directory))
                (ensure-directories-exist (merge-pathnames "x" (car (last argv))))
                (values "" 0))))
        (let ((object (athena-object "athena-6" "athena_install" "{\"url\":\"image upscaler\"}")))
          (is (equal "https://github.com/example/voice-cloner"
                     (nlk:json-value object :string "repo"))
              "a name is resolved to the repository the store names first"))))))

(deftest athena-cell-install-refuses-a-folder-already-taken ()
  (with-cell-stop ((athena-start))
    (athena-make-app "voice-cloner" "start.sh")
    (let ((nodecode-athena::*run* (lambda (argv &key directory)
                                    (declare (ignore argv directory))
                                    (error "nothing should be cloned"))))
      (let ((answer (athena-answer "athena-7" "athena_install"
                                    "{\"url\":\"https://github.com/example/voice-cloner\"}")))
        (is (search "already installed" answer))
        (is (search "voice-cloner" answer) "the refusal names the folder it would take")))))

;;; --- athena_launch, athena_stop -----------------------------------------------------

(deftest athena-cell-launches-an-app-and-names-its-url ()
  (with-cell-stop ((athena-start))
    (athena-make-app "voice-cloner" "start.sh")
    (let ((nodecode-athena::*launch*
            (lambda (command directory log)
              (declare (ignore directory))
              (with-open-file (out log :direction :output :if-exists :append
                                        :if-does-not-exist :create)
                (format out "up on http://127.0.0.1:7860~%"))
              (values :fake-process 4242))))
      (let ((object (athena-object "athena-8" "athena_launch" "{\"app\":\"voice-cloner\"}")))
        (is (= 4242 (nlk:json-value object :integer "pid")))
        (is (equal "sh start.sh" (nlk:json-value object :string "command")) "the folder's own start script runs")
        (is (equal "http://127.0.0.1:7860" (nlk:json-value object :string "url"))
            "the URL the app's log named is answered")))))

(deftest athena-cell-launch-refuses-an-app-with-nothing-to-run ()
  (with-cell-stop ((athena-start))
    (athena-make-app "empty-one")
    (let ((answer (athena-answer "athena-9" "athena_launch" "{\"app\":\"empty-one\"}")))
      (is (search "no start script" answer))
      (is (search "pass script" answer) "the refusal names the way out"))))

(deftest athena-cell-stops-what-it-started ()
  (with-cell-stop ((athena-start))
    (athena-make-app "voice-cloner" "start.sh")
    (let ((terminated nil))
      (let ((nodecode-athena::*launch* (lambda (command directory log)
                                         (declare (ignore command directory log))
                                         (values :fake-process 4242)))
            (nodecode-athena::*alive* (lambda (process) (eq process :fake-process)))
            (nodecode-athena::*terminate* (lambda (process)
                                            (setf terminated process)
                                            t)))
        (athena-object "athena-10" "athena_launch" "{\"app\":\"voice-cloner\"}")
        (let ((object (athena-object "athena-11" "athena_stop" "{\"app\":\"voice-cloner\"}")))
          (is (eq t (nlk:json-value object :boolean "stopped")))
          (is (eq :fake-process terminated) "the process this cell started is the one signalled"))))))

(deftest athena-cell-stop-refuses-an-app-that-is-not-running ()
  (with-cell-stop ((athena-start))
    (athena-make-app "voice-cloner" "start.sh")
    (let ((answer (athena-answer "athena-12" "athena_stop" "{\"app\":\"voice-cloner\"}")))
      (is (search "not running" answer)))))

;;; --- athena_apps, athena_logs -------------------------------------------------------

(deftest athena-cell-lists-what-is-installed ()
  (with-cell-stop ((athena-start))
    (athena-make-app "voice-cloner" "start.sh")
    (athena-make-app "image-upscaler" "start.py")
    (with-stubbed-fdefinition (nodecode-athena::app-running-p (id) (equal id "image-upscaler"))
      (let ((object (athena-object "athena-13" "athena_apps" "{}")))
        (is (= 2 (nlk:json-value object :integer "count")))
        (let ((apps (nlk:json-array object "apps")))
          (is (equal "image-upscaler" (nlk:json-value (aref apps 0) :string "app")) "by name")
          (is (eq t (nlk:json-value (aref apps 0) :boolean "running")))
          (is (null (nlk:json-value (aref apps 1) :boolean "running")) "a stopped app says so"))))))

(deftest athena-cell-logs-name-the-cause ()
  (with-cell-stop ((athena-start))
    (athena-make-app "voice-cloner")
    (let ((log (merge-pathnames "voice-cloner.log" (nodecode-athena::state-dir))))
      (ensure-directories-exist log)
      (with-open-file (out log :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
        (format out "loading weights~%torch.cuda.OutOfMemoryError: CUDA out of memory.~%"))
      (is (search "CUDA out of memory" (or (nodecode-athena::read-log-text "voice-cloner") ""))
          (format nil "the log is read back: ~s vs ~s" (namestring log)
                  (namestring (nodecode-athena::log-path "voice-cloner"))))
      (let ((object (athena-object "athena-14" "athena_logs"
                                    "{\"app\":\"voice-cloner\",\"why\":true}")))
        (is (search "GPU ran out of memory" (nlk:json-value object :string "cause")))
        (is (search "batch size" (nlk:json-value object :string "fix")) "the reading says what answers it"))
      (let ((object (athena-object "athena-15" "athena_logs" "{\"app\":\"voice-cloner\",\"lines\":1}")))
        (is (search "CUDA out of memory" (nlk:json-value object :string "lines"))
            "the last line of the log is answered")
        (is (not (search "loading weights" (nlk:json-value object :string "lines")))
            "and only as many lines as were asked for")))))

(deftest athena-cell-status-names-where-it-reads ()
  (with-cell-stop ((athena-start))
    (let ((object (athena-object "athena-16" "athena_status" "{}")))
      (is (equal "https://store.test/apps" (nlk:json-value object :string "catalog_url")))
      (is (= 0 (nlk:json-value object :integer "installed")) "nothing is installed yet")
      (is (equal (athena-apps-dir) (nlk:json-value object :string "apps_dir"))))))

;;; --- the command -----------------------------------------------------------------

(deftest athena-cell-command-answers-what-is-installed ()
  (with-cell-stop ((athena-start))
    (let ((text (nodecode-athena::run-athena nil "athena-session")))
      (is (search "nothing is installed yet" text) "an empty shelf is said plainly"))
    (athena-make-app "voice-cloner" "start.sh")
    (let ((text (nodecode-athena::run-athena nil "athena-session")))
      (is (search "voice-cloner" text))
      (is (search "stopped" text) "and each app's state is on its line"))))
