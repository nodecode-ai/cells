;;;; cell-test.lisp --- the cell over a fake CDP endpoint and a fake launcher.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here starts a browser and nothing opens a socket: the cell's one
;;;; network seam (NODECODE-BROWSER-PROFILES::*HTTP*) answers the DevTools
;;;; routes each test scripts and records every request it took, and its
;;;; launcher (::*LAUNCH*, ::*TERMINATE*, ::*ALIVE-P*, ::*SLEEP*) is a
;;;; recorder. The tools are called through the real dispatch seam
;;;; (EXECUTE-WIRE-CALL) and the command through CELL-ENTRY, so what is
;;;; proved is the declaration a provider is shown and the body that runs it.

(in-package #:nodecode.test)

(define-test-slice "browser-profiles" "BROWSER-PROFILES-CELL-"
  :start nodecode-browser-profiles:start-cell
  :defaults (("launch_timeout" 20)
             ("profiles" (bprof-profiles "work" (list :port 9250)))))

;;; --- the fake machine --------------------------------------------------------

(defvar *bprof-requests* '()
  "Every request the fake endpoint took, newest first: (METHOD URL).")

(defvar *bprof-launched* '()
  "The argv of every browser the fake launcher started, newest first.")

(defvar *bprof-terminated* '()
  "Every process the fake launcher was asked to stop.")

(defvar *bprof-calls* 0
  "A counter for wire call ids.")

(defun bprof-profiles (&rest pairs)
  "The profiles object from alternating NAME PLIST pairs."
  (apply #'nlk:make-json-object
         (loop for (name plist) on pairs by #'cddr
               collect name
               collect (apply #'nlk:make-json-object
                              (loop for (key value) on plist by #'cddr
                                    collect (substitute #\_ #\- (string-downcase (string key)))
                                    collect value)))))

(defun bprof-json-answer (&rest pairs)
  "PAIRS as the JSON text a route answers with."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun bprof-path (url)
  "URL's path, without scheme, host or query."
  (let ((uri (ignore-errors (quri:uri url))))
    (if uri (or (quri:uri-path uri) "") url)))

(defun bprof-route-p (fragment path)
  "Whether PATH is FRAGMENT, or under it when the fragment ends in a slash."
  (if (and (plusp (length fragment))
           (char= (char fragment (1- (length fragment))) #\/))
      (uiop:string-prefix-p fragment path)
      (equal fragment path)))

(defun bprof-fake-cdp (routes)
  "A stand-in for NLK:HTTP over ROUTES, each (METHOD FRAGMENT BODY &optional
STATUS): the first route whose fragment the request path carries answers."
  (setf *bprof-requests* '())
  (lambda (method url &key content &allow-other-keys)
    (declare (ignore content))
    (push (list method url) *bprof-requests*)
    (let* ((path (bprof-path url))
           (route (find-if (lambda (route)
                             (and (string-equal (string (first route)) (string method))
                                  (bprof-route-p (second route) path)))
                           routes)))
      (if route
          (values (third route) (or (fourth route) 200) nil nil)
          (values "{\"error\":\"no such route\"}" 404 nil nil)))))

(defmacro with-profile-machine (routes &body body)
  "The cell over ROUTES answered by the fake endpoint, with a launcher that
starts nothing. ROUTES is a list of (METHOD FRAGMENT BODY [STATUS])."
  `(let ((nodecode-browser-profiles::*http*
           (bprof-fake-cdp (list ,@(loop for route in routes collect `(list ,@route)))))
          (nodecode-browser-profiles::*find-binary* (lambda (key)
                                                       (declare (ignorable key))
                                                       "/opt/fake-chrome"))
          (nodecode-browser-profiles::*launch*
            (lambda (argv log)
              (declare (ignore log))
              (push argv *bprof-launched*)
              (list :fake-process (first argv))))
          (nodecode-browser-profiles::*terminate*
            (lambda (process) (push process *bprof-terminated*)))
          (nodecode-browser-profiles::*alive-p* (lambda (process)
                                                  (declare (ignore process))
                                                  t))
          (nodecode-browser-profiles::*sleep* (lambda (seconds) (declare (ignore seconds))))
          (*bprof-requests* '()) (*bprof-launched* '()) (*bprof-terminated* '()))
     ,@body))

(defun bprof-tool-text (name &rest arguments)
  "The text the tool NAME answers, called as the provider's call is."
  (princ-to-string
   (nle::message-content
    (execute-wire-call (format nil "bprof-call-~d" (incf *bprof-calls*)) name
                       (if arguments (apply #'bprof-json-answer arguments) "{}")))))

(defmacro bprof-carrying (form &body checks)
  "IS-CARRYING over FORM's own value, which needs no binding of its own."
  `(is-carrying (text ,form) ,@checks))

(defun bprof-argv-contains (fragment argv)
  "Whether some element of ARGV carries FRAGMENT."
  (and (find fragment argv :test #'search) t))

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "browser-profiles"
  (:fixture with-profile-machine ())
  (:config "profiles" (bprof-profiles "work" (list :port 9250)))
  (:help :browser-profiles)
  (:command "browser-profile")
  (:running (is (= 20 (nodecode-browser-profiles::setting :launch-timeout)) "launch_timeout defaults to twenty seconds")
            (is (equal '("work")
                       (mapcar (lambda (entry) (getf entry :name))
                               (nodecode-browser-profiles::setting :profiles))) "the section's profiles are read into entries")
            (is (null nodecode-browser-profiles::*active*) "nothing is active yet"))
  (:stopped (is (null nodecode-browser-profiles::*active*) "nothing is active after stop"))
  (:refused ("launch_timeout" 0))
  (:idle nodecode-browser-profiles::browser-profiles-error
         (nodecode-browser-profiles:available)))

;;; --- switching ---------------------------------------------------------------

(deftest browser-profiles-cell-switch-launches-a-local-profile ()
  (with-profile-machine ()
    (let ((ready nil))
      (let ((nodecode-browser-profiles::*http*
              (lambda (method url &key content &allow-other-keys)
                (declare (ignore method content))
                (push (list :get url) *bprof-requests*)
                (if (and ready (search "/json/version" url))
                    (values (bprof-json-answer "Browser" "Chrome/141") 200 nil nil)
                    (values "{\"error\":\"not up\"}" 404 nil nil))))
            (nodecode-browser-profiles::*launch*
              (lambda (argv log)
                (declare (ignore log))
                (setf ready t)
                (push argv *bprof-launched*)
                (list :fake-process (first argv)))))
        (with-cell-stop ((browser-profiles-start "profiles"
                                                  (bprof-profiles "work" (list :port 9250))))
          (let ((answer (nodecode-browser-profiles:switch "work")))
            (bprof-carrying answer ("work") ("9250") ("launched"))
            (is (equal "work" nodecode-browser-profiles::*active*)))
          (is (= 1 (length *bprof-launched*)) "one browser was launched")
          (let ((argv (first *bprof-launched*)))
            (is (bprof-argv-contains "--remote-debugging-port=9250" argv))
            (is (bprof-argv-contains "--user-data-dir=" argv))
            (is (bprof-argv-contains "browser-profiles/work" argv) "the data dir is this home's own, per profile")
            (is (equal "/opt/fake-chrome" (first argv))))
          (is (equal "http://127.0.0.1:9250" (nodecode-browser-profiles:cdp-url)))
          (is (search "Chrome/141" (nodecode-browser-profiles:version))))
        (is (= 1 (length *bprof-terminated*)) "stop terminates the browser it launched")
        (is (null nodecode-browser-profiles::*active*))))))

(deftest browser-profiles-cell-a-remote-profile-is-never-launched ()
  (with-profile-machine ((:get "/json/version" (bprof-json-answer "Browser" "Chrome/141")))
    (with-cell-stop ((browser-profiles-start
                      "profiles" (bprof-profiles "box" (list :host "10.0.0.5" :port 9222))))
      (bprof-carrying (nodecode-browser-profiles:switch "box") ("box") ("10.0.0.5:9222"))
      (is (null *bprof-launched*) "a remote profile launches nothing")
      (is (equal "box" nodecode-browser-profiles::*active*)))))

(deftest browser-profiles-cell-a-refusal-names-the-profile ()
  (with-profile-machine ()
    (with-cell-stop ((browser-profiles-start
                      "profiles" (bprof-profiles "work" (list :port "9250"))))
      (bprof-carrying (nodecode-browser-profiles:available)
                   ("profile 'work'") ("port") ("integer"))
      (is (signals-error nodecode-browser-profiles::browser-profiles-error
                         (nodecode-browser-profiles:switch "work")))
      (bprof-carrying (handler-case (progn (nodecode-browser-profiles:switch "nope") nil)
                     (nodecode-browser-profiles::browser-profiles-error (condition)
                       (princ-to-string condition)))
                   ("unknown profile") ("work")))))

(deftest browser-profiles-cell-no-profiles-answers-where-to-put-them ()
  (with-profile-machine ()
    (with-cell-stop ((nodecode-browser-profiles:start-cell
                      (browser-profiles-config "profiles" (nlk:make-json-object))))
      (bprof-carrying (nodecode-browser-profiles:available)
                   ("No browser profiles") ("config.jsonc")))))

;;; --- the tabs ----------------------------------------------------------------

(deftest browser-profiles-cell-tabs-open-and-close ()
  (with-profile-machine ((:get "/json/version" (bprof-json-answer "Browser" "Chrome/141"))
                         (:get "/json/list"
                               (nlk:encode-json-object
                                (vector (nlk:make-json-object "id" "A" "title" "One"
                                                               "url" "https://one.example")
                                        (nlk:make-json-object "id" "B" "title" "Two"
                                                               "url" "https://two.example"))))
                         (:put "/json/new" (bprof-json-answer "id" "C"))
                         (:get "/json/close/" "Target is closing")
                         (:get "/json/activate/" "Target activated"))
    (with-cell-stop ((browser-profiles-start "profiles"
                                              (bprof-profiles "work" (list :port 9250))))
      (nodecode-browser-profiles:switch "work")
      (let ((tabs (nodecode-browser-profiles:tabs)))
        (is (= 2 (length tabs)) "two tabs are listed")
        (is (equal "A" (getf (first tabs) :id)))
        (is (equal "https://two.example" (getf (second tabs) :url))))
      (bprof-carrying (nodecode-browser-profiles:open-tab "https://example.com/a b") ("C"))
      (is (find "/json/new" *bprof-requests* :key (lambda (request) (bprof-path (second request)))
                :test #'equal) "the tab is opened on the DevTools route")
      (bprof-carrying (nodecode-browser-profiles:close-tab "C") ("C"))
      (bprof-carrying (nodecode-browser-profiles:activate-tab "C") ("C")))))

;;; --- the two entrypoints -----------------------------------------------------

(deftest browser-profiles-cell-the-tool-and-the-command-answer ()
  (with-profile-machine ((:get "/json/version" (bprof-json-answer "Browser" "Chrome/141")))
    (with-cell-stop ((browser-profiles-start "profiles"
                                              (bprof-profiles "work" (list :port 9250))))
      (bprof-carrying (bprof-tool-text "browser_profile") ("work") ("9250"))
      (bprof-carrying (bprof-tool-text "browser_profile" "name" "work") ("active"))
      (bprof-carrying (bprof-tool-text "browser_profile" "disconnect" t) ("no longer active"))
      (bprof-carrying (cell-entry "nodecode-browser-profiles" "browser-profile" "" nil)
                   ("work") ("9250"))
      (bprof-carrying (cell-entry "nodecode-browser-profiles" "browser-profile" "work" nil)
                   ("active"))
      (bprof-carrying (cell-entry "nodecode-browser-profiles" "browser-profile" "off" nil)
                   ("no longer active"))
      (bprof-carrying (nodecode-browser-profiles:disconnect) ("No profile was active"))
      (bprof-carrying (handler-case
                       (cell-entry "nodecode-browser-profiles" "browser-profile" "bogus" nil)
                     (nodecode-browser-profiles::browser-profiles-error (condition)
                       (princ-to-string condition)))
                   ("unknown profile")))))
