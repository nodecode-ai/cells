;;;; cell-test.lisp --- the camofox cell, end to end over a scripted server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here opens a socket: the cell's one network seam
;;;; (NODECODE-CAMOFOX::*HTTP*) is bound to a fake server that answers the
;;;; routes each test scripts and records every request it took, so "the URL
;;;; never left" is a fact a test can read. The tools are called through the
;;;; real dispatch seam (EXECUTE-WIRE-CALL), so the schema the provider is
;;;; shown and the body that runs are the same declaration.

(in-package #:nodecode.test)

(define-test-slice "camofox" "CAMOFOX-CELL-" :start nodecode-camofox:start-cell)

;;; --- the fake server ---------------------------------------------------------

(defvar *requests* '()
  "Every request the fake server took, newest first: (METHOD URL CONTENT).")

(defvar *calls* 0
  "A counter for wire call ids.")

(defvar *show-tool-answers* nil
  "Whether TOOL-TEXT echoes what each tool answered: bind it true to read a
failing test's log.")

(defun request-path (url)
  "URL without its scheme, host and query: the route it names."
  (let ((uri (ignore-errors (quri:uri url))))
    (if uri (or (quri:uri-path uri) "") url)))

(defun route-matches-p (fragment path)
  "Whether a request PATH is FRAGMENT: exactly, or under it when the
fragment ends in a slash (a route carrying a user id)."
  (if (and (plusp (length fragment)) (char= (char fragment (1- (length fragment))) #\/))
      (uiop:string-prefix-p fragment path)
      (equal fragment path)))

(defun fake-server (routes)
  "A stand-in for NLK:HTTP over ROUTES, each (METHOD PATH-FRAGMENT BODY &optional
STATUS): the first route whose fragment the request path carries answers.
Every request lands in *REQUESTS*."
  (setf *requests* '())
  (lambda (method url &key content &allow-other-keys)
    (push (list method url content) *requests*)
    (let* ((path (request-path url))
           (route (find-if (lambda (route)
                             (and (string-equal (string (first route)) (string method))
                                  (route-matches-p (second route) path)))
                           routes)))
      (if route
          (values (third route) (or (fourth route) 200) nil nil)
          (values "{\"error\":\"no such route\"}" 404 nil nil)))))

(defmacro with-fake-camofox (&body body)
  "The cell over a server that answers no route: the shape of a test that
calls nothing but proves the wiring."
  `(let ((nodecode-camofox::*http* (fake-server '()))
         (nodecode-camofox::*sessions* (make-hash-table :test #'equal)))
     ,@body))

(defmacro with-server ((&rest routes) &body body)
  "The cell over a server answering ROUTES, with its tab table fresh. Each
route is (METHOD FRAGMENT BODY &optional STATUS), its elements evaluated
where the test is written."
  `(let ((nodecode-camofox::*http* (fake-server (list ,@(loop for route in routes
                                                             collect `(list ,@route)))))
         (nodecode-camofox::*sessions* (make-hash-table :test #'equal)))
     ,@body))

(defmacro with-camofox ((&rest pairs) &body body)
  "The cell started on the carrier's config, plus PAIRS, and stopped after."
  `(with-cell-stop ((camofox-start "url" "http://camofox.test:9377" ,@pairs))
     ,@body))

(defun requests-to (fragment)
  "The requests whose URL carries FRAGMENT, oldest first."
  (reverse (remove fragment *requests* :test-not #'search :key #'second)))

(defun json-answer (&rest pairs)
  "PAIRS as the JSON text a route answers with."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun tool-text (name &rest arguments)
  "The text the tool called NAME answers, called as the provider's call is."
  (let ((text (princ-to-string
               (nle::message-content
                (execute-wire-call (format nil "camofox-call-~d" (incf *calls*)) name
                                    (if arguments (apply #'json-answer arguments) "{}"))))))
    (when *show-tool-answers*
      (format t "~&; ~a => ~a~%" name text))
    text))

(defun octets (text)
  "TEXT's characters as the octet vector a binary route answers."
  (coerce (map 'list #'char-code text) '(vector (unsigned-byte 8))))

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "camofox"
  (:fixture with-fake-camofox)
  (:config "url" "http://camofox.test:9377")
  (:help :camofox)
  (:running
   (is (equal "http://camofox.test:9377" (nodecode-camofox::base-url)) "the section's url")
   (loop for name in '("browser_navigate" "browser_snapshot" "browser_click" "browser_type"
                       "browser_scroll" "browser_back" "browser_press" "browser_get_images"
                       "browser_vision" "browser_console" "browser_import_cookies")
         do (is (nle::find-tool name) name)))
  (:stopped
   (is (null (nle::find-tool "browser_navigate")) "stop takes every tool out of the catalog")
   (is (null nodecode-camofox::*camofox*) "and the settings with it"))
  (:refused ("url" "localhost:9377")))

(deftest camofox-cell-a-server-that-is-not-an-address-is-refused ()
  ;; The url is read at the start, so a typo lands on the folder's REFUSED
  ;; line and one standing notice instead of on every tool call.
  (with-fake-camofox
    (is (search "camofox.url is"
                (handler-case (progn (camofox-start "url" "camofox.test") "")
                  (nlk:config-refusal (condition) (nlk:config-refusal-detail condition)))))
    (is (null (nle::find-tool "browser_navigate")) "and nothing was registered")))

(deftest camofox-cell-no-url-refuses-every-tool ()
  ;; url is required by the declaration: while it is unanswered the tools are
  ;; there and say what to set, which is what the setup panel is for.
  (with-session-store ("camofox-nourl")
    (with-fake-camofox
      (with-cell-stop ((camofox-start))
        (let ((text (tool-text "browser_navigate" "url" "https://example.com")))
          (is (search "ERROR" text))
          (is (search "camofox.url is not set" text) "the refusal names the member"))
        (is (null *requests*) "and nothing was sent")))))

;;; --- navigating ---------------------------------------------------------------

(deftest camofox-cell-navigate-opens-a-tab-and-answers-the-page ()
  (with-session-store ("camofox-nav")
    (with-server (("POST" "/tabs" (json-answer "tabId" "tab-1" "url" "https://example.com/" "ok" t))
                  ("GET" "/tabs/tab-1/snapshot" (json-answer "snapshot" "- heading \"Example\""
                                                              "refsCount" 2))
                  ("GET" "/tabs" (json-answer "tabs" (vector (nlk:json-object "tabId" "tab-1"
                                                                                "url" "https://example.com/"
                                                                                "title" "Example Domain"))))
                  ("GET" "/health" (json-answer "vncPort" 6080)))
      (with-camofox ()
        (let ((text (tool-text "browser_navigate" "url" "https://example.com")))
          (is (search "\"success\":true" text) "the navigate answered")
          (is (search "\"title\":\"Example Domain\"" text) "with the page's title")
          (is (search "\"tab_created\":true" text) "and says the tab was made")
          (is (search "Example" text) "and carries a snapshot the model can act on"))
        (let ((sent (first (requests-to "/tabs"))))
          (is (equal :post (first sent)) "the tab was created by a POST")
          (is (search "listItemId" (third sent)) "carrying this session's key"))
        (is (equal 1 (length (requests-to "/tabs?userId"))) "tabs listed, not per call")
        (is (search "camofox" (prin1-to-string (first (requests-to "/health")))) "and the health probe")))))

(deftest camofox-cell-navigate-refuses-a-metadata-endpoint-and-a-secret ()
  ;; Both refusals are the original's: nothing leaves the process.
  (with-session-store ("camofox-policy")
    (with-server ()
      (with-camofox ()
        (let ((text (tool-text "browser_navigate" "url" "http://169.254.169.254/latest/meta-data/")))
          (is (search "metadata endpoint" text)))
        (let ((text (tool-text "browser_navigate" "url" "https://api.test/?key=sk-live-0123456789abcdef")))
          (is (search "API key" text)))
        (is (null *requests*) "nothing reached the server")))))

(deftest camofox-cell-navigate-replaces-a-tab-the-server-reaped ()
  ;; A session keeps the tab id it was given; the server reaps it between
  ;; turns. The next navigation must make a new tab instead of failing from
  ;; then on, which is what the original did (404/410 on navigate).
  (with-session-store ("camofox-gone")
    (with-server (("POST" "/tabs/tab-1/navigate" (json-answer "error" "gone") 404)
                  ("POST" "/tabs/tab-2/navigate" (json-answer "url" "https://example.com/"))
                  ("POST" "/tabs" (json-answer "tabId" "tab-2" "url" "https://example.com/"))
                  ("GET" "/tabs/tab-2/snapshot" (json-answer "snapshot" "- text \"Example\"" "refsCount" 1))
                  ("GET" "/tabs/tab-2/screenshot" (octets "PNG"))
                  ("GET" "/tabs" (json-answer "tabs" (vector (nlk:json-object "tabId" "tab-2"
                                                                                "url" "https://example.com/")))))
      (with-camofox ()
        (let ((session (nodecode-camofox::make-tab-session)))
          (setf (nodecode-camofox::tab-session-user-id session) "hermes_test"
                (nodecode-camofox::tab-session-session-key session) "task_test"
                (nodecode-camofox::tab-session-tab-id session) "tab-1")
          (setf (gethash (nodecode-camofox::task-key) nodecode-camofox::*sessions*) session))
        (let ((text (tool-text "browser_navigate" "url" "https://example.com")))
          (is (search "\"success\":true" text) "the navigation answered")
          (is (search "\"tab_created\":true" text) "with a fresh tab, not a failure")
          (let ((sent (first (requests-to "/tabs/tab-2"))))
            (is (search "tab-2" (prin1-to-string sent)) "and the new id is the one it drives")))
        (tool-text "browser_navigate" "url" "https://example.com")
        (is (= 1 (length (requests-to "/tabs/tab-1/navigate"))) "the dead id was tried once")
        (let ((creations (count-if (lambda (request)
                                     (and (eq :post (first request))
                                          (equal "/tabs" (request-path (second request)))))
                                   *requests*)))
          (is (= 1 creations) "and only one tab was ever created"))))))

;;; --- the tools on an open tab --------------------------------------------------

(deftest camofox-cell-every-tool-answers-through-the-server ()
  (with-session-store ("camofox-tools")
    (with-server (("POST" "/tabs/tab-1/navigate" (json-answer "url" "https://example.com/"))
                  ("POST" "/tabs" (json-answer "tabId" "tab-1" "url" "https://example.com/"))
                  ("GET" "/tabs/tab-1/snapshot"
                   (json-answer "snapshot" (format nil "- heading \"Example\"~%- img \"A logo\"~%  /url: https://example.com/logo.png~%- textbox \"Email\"")
                                "refsCount" 3))
                  ("POST" "/tabs/tab-1/click" (json-answer "url" "https://example.com/next"))
                  ("POST" "/tabs/tab-1/type" (json-answer "ok" t))
                  ("POST" "/tabs/tab-1/scroll" (json-answer "ok" t))
                  ("POST" "/tabs/tab-1/press" (json-answer "ok" t))
                  ("POST" "/tabs/tab-1/back" (json-answer "url" "https://example.com/"))
                  ("POST" "/tabs/tab-1/evaluate" (json-answer "result" "Example Domain"))
                  ("GET" "/tabs/tab-1/screenshot" (octets "PNGDATA"))
                  ("GET" "/tabs" (json-answer "tabs" (vector (nlk:json-object "tabId" "tab-1"
                                                                                "url" "https://example.com/")))))
      (with-camofox ()
        (tool-text "browser_navigate" "url" "https://example.com")
        (let ((text (tool-text "browser_snapshot")))
          (is (search "\"element_count\":3" text) "the snapshot counts the refs")
          (is (search "textbox" text) "and carries the tree"))
        (let ((text (tool-text "browser_get_images")))
          (is (search "logo.png" text) "an image's url")
          (is (search "A logo" text) "and its alt text"))
        (let ((text (tool-text "browser_click" "ref" "@e5")))
          (is (search "\"clicked\":\"e5\"" text) "the @ is taken off the ref")
          (is (search "next" text) "and the landing url answered"))
        (let ((text (tool-text "browser_type" "ref" "e3" "text" "sk-abcdefghijklmnop")))
          (is (search "[redacted]" text) "a key-shaped typed value is masked back")
          (is (search "\"element\":\"e3\"" text)))
        (is (search "\"scrolled\":\"down\"" (tool-text "browser_scroll" "direction" "down")))
        (is (= 5 (length (requests-to "/tabs/tab-1/scroll"))) "five steps, as the original scrolled")
        (is (search "\"pressed\":\"Enter\"" (tool-text "browser_press" "key" "Enter")))
        (is (search "https://example.com/" (tool-text "browser_back")) "back answers where it landed")
        (let ((text (tool-text "browser_console" "expression" "document.title")))
          (is (search "Example Domain" text) "the evaluated value"))
        (let ((text (tool-text "browser_console")))
          (is (search "Console log capture is not available" text) "capture is not available"))
        (let* ((text (tool-text "browser_vision" "question" "what is on the page?"))
               (path (nle::json-value (nlk:decode-json text) :string "screenshot_path")))
          (is (search "screenshot_path" text))
          (is (search "look" text) "it says how to look at the file")
          (is (probe-file path) "and the PNG is on disk"))
        (let ((scroll (third (first (requests-to "/scroll")))))
          (is (search "\"direction\":\"down\"" scroll) "the scroll carried the direction"))))))

(deftest camofox-cell-a-docker-loopback-url-is-rewritten ()
  ;; In Docker the browser opens page URLs inside its container, where
  ;; loopback is the container: the operator's alias is what reaches the
  ;; host, and the answer says the URL was rewritten.
  (with-session-store ("camofox-docker")
    (with-server (("POST" "/tabs" (json-answer "tabId" "tab-1" "url" "http://host.docker.internal:8080/x"))
                  ("GET" "/tabs" (json-answer "tabs" (vector (nlk:json-object "tabId" "tab-1"
                                                                                "url" "http://host.docker.internal:8080/x")))))
      (with-camofox ("rewrite_loopback_urls" t)
        (let* ((text (tool-text "browser_navigate" "url" "http://127.0.0.1:8080/x?y=1"))
               (sent (third (first (requests-to "/tabs")))))
          (is (search "http://host.docker.internal:8080/x?y=1" sent) "the browser got the alias")
          (is (search "Rewrote loopback URL" text) "and the answer says so")
          (let ((asked (search "\"requested_url\":\"http://127.0.0.1:8080/x?y=1\"" text)))
            (is asked "with the url the model asked for")))))))

(deftest camofox-cell-a-tool-with-no-tab-says-to-navigate-first ()
  (with-session-store ("camofox-notab")
    (with-server ()
      (with-camofox ()
        (is (search "call browser_navigate first" (tool-text "browser_snapshot")))
        (is (null *requests*) "and asks the server nothing")))))

(deftest camofox-cell-a-long-snapshot-is-cut-and-kept-in-a-file ()
  (with-session-store ("camofox-long")
    (with-server (("POST" "/tabs" (json-answer "tabId" "tab-1" "url" "https://example.com/"))
                  ("GET" "/tabs/tab-1/snapshot"
                   (json-answer "snapshot" (format nil "~{- line ~d~%~}" (loop for i below 400 collect i))
                                "refsCount" 400))
                  ("GET" "/tabs" (json-answer "tabs" (vector (nlk:json-object "tabId" "tab-1"
                                                                                "url" "https://example.com/")))))
      (with-camofox ("snapshot_threshold" 2000)
        (tool-text "browser_navigate" "url" "https://example.com")
        (let* ((text (tool-text "browser_snapshot"))
               (path (nle::json-value (nlk:decode-json text) :string "snapshot")))
          (is (search "more lines cut" path) "the cut says how many lines went")
          (is (< (length path) 2200) "and the snapshot the model reads is bounded"))))))


;;; --- cookies ------------------------------------------------------------------

(defun cookie-line (domain flag path secure expires name value)
  "One Netscape cookie file line: seven tab-separated fields."
  (format nil "~a~c~a~c~a~c~a~c~d~c~a~c~a"
          domain #\Tab flag #\Tab path #\Tab secure #\Tab expires #\Tab name #\Tab value))

(deftest camofox-cell-cookie-import-reads-only-the-cookies-dir ()
  (with-session-store ("camofox-cookies")
    (with-server (("POST" "/sessions/" (json-answer "ok" t)))
      (with-camofox ("api_key" "s3cret-token")
        (let* ((dir (merge-pathnames ".camofox/cookies/" (user-homedir-pathname)))
               (file (merge-pathnames "test.txt" dir)))
          (ensure-directories-exist file)
          (with-open-file (out file :direction :output :if-exists :supersede
                                     :if-does-not-exist :create)
            (format out "# Netscape HTTP Cookie File~%")
            (dolist (line (list (cookie-line ".example.com" "TRUE" "/" "FALSE" 1700000000 "session" "abc")
                                (cookie-line "#HttpOnly_.example.com" "TRUE" "/" "FALSE" 1700000000 "other" "def")
                                (cookie-line ".elsewhere.test" "TRUE" "/" "FALSE" 1700000000 "away" "123")))
              (write-line line out)))
          (let ((text (tool-text "browser_import_cookies" "cookies_path" "test.txt")))
            (is (search "\"imported\":3" text) "every cookie in the file"))
          (let* ((text (tool-text "browser_import_cookies" "cookies_path" "test.txt"
                                   "domain_suffix" ".example.com"))
                 (sent (first (requests-to "/sessions/"))))
            (is (search "\"imported\":2" text) "the suffix keeps one domain")
            (is (search "httpOnly" (third sent)) "the flags ride with the cookie")
            (is (search "session" (third sent)) "and the values"))
          (let ((escape (tool-text "browser_import_cookies" "cookies_path" "../escape.txt")))
            (is (search "must stay inside" escape) "a path outside the cookies dir is refused"))
          (is (search "Cookie file not found" (tool-text "browser_import_cookies"
                                                           "cookies_path" "absent.txt"))))))))

(deftest camofox-cell-cookie-import-needs-the-api-key ()
  ;; The server's cookie route checks the bearer token: without one the tool
  ;; says so instead of sending an import that would be refused.
  (with-session-store ("camofox-cookie-key")
    (with-server (("POST" "/sessions/" (json-answer "count" 1)))
      (with-camofox ()
        (let* ((file (merge-pathnames ".camofox/cookies/test.txt" (user-homedir-pathname))))
          (ensure-directories-exist file)
          (with-open-file (out file :direction :output :if-exists :supersede
                                     :if-does-not-exist :create)
            (write-line (cookie-line ".example.com" "TRUE" "/" "FALSE" 1700000000 "a" "b") out))
          (is (search "camofox.api_key is not set"
                      (tool-text "browser_import_cookies" "cookies_path" "test.txt")))
          (is (null *requests*) "and nothing was sent"))))))
