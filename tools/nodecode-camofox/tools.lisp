;;;; tools.lisp --- the eleven browser tools, over the Camofox REST routes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Each tool answers the JSON string the model reads; a refusal signals
;;;; CAMOFOX-ERROR, which the engine renders as "ERROR: <detail>". The names
;;;; and the arguments are the original plugin's, so a prompt or a skill that
;;;; says browser_navigate / browser_click keeps working.
;;;;
;;;; What the original spent most of its code on is kept and restated here:
;;;; a URL carrying a key-shaped secret or pointing at a cloud metadata
;;;; endpoint is refused before the request goes out; a loopback page URL is
;;;; rewritten to a host alias when the Camofox runs in Docker; an oversized
;;;; snapshot is cut at a line boundary with the whole tree written to a file;
;;;; a typed value the server echoes back is masked; a tab the server reaped
;;;; is recreated rather than failing every call from then on.

(in-package #:nodecode-camofox)

;;; --- argument declarations ------------------------------------------------
;;; One declaration is both the schema the provider is shown and the value
;;; the body runs on; a call that does not satisfy it is refused before the
;;; body, with the repair the model needs.

(nlk:define-contract navigate-arguments
  (:doc "Navigate the Camofox browser to a URL. Opens this session's tab the first time and loads the page, then answers the page title, the URL it landed on and an accessibility snapshot of the page — no separate browser_snapshot is needed after it. For plain-text or API endpoints a fetch tool is faster; use this when the page has to be interacted with.")
  ("url" :text :required t :doc "the page to open, e.g. 'https://example.com'"))

(nlk:define-contract click-arguments
  (:doc "Click the element a snapshot marked with a ref. Requires browser_navigate first, and a fresh snapshot after anything that changes the page.")
  ("ref" :text :required t :doc "the element's ref from the snapshot, e.g. '@e5' or 'e5'"))

(nlk:define-contract type-arguments
  (:doc "Type text into an input the snapshot marked with a ref: the field is cleared first. Requires browser_navigate first.")
  ("ref" :text :required t :doc "the input's ref from the snapshot, e.g. '@e3'")
  ("text" :string :required t :doc "the text to put in the field"))

(nlk:define-contract scroll-arguments
  (:doc "Scroll the page to reveal content above or below the viewport. Requires browser_navigate first.")
  ("direction" :string :required t :options '("up" "down") :doc "which way to scroll"))

(nlk:define-contract press-arguments
  (:doc "Press one key in the page: Enter to submit, Tab to move, Escape, or a shortcut. Requires browser_navigate first.")
  ("key" :text :required t :doc "the key, e.g. 'Enter', 'Tab', 'Escape', 'ArrowDown'"))

(nlk:define-contract vision-arguments
  (:doc "Screenshot the page and answer where the image was saved, so you can look at it with (look PATH) — use it for a CAPTCHA, a visual check, a layout the text snapshot does not carry. Requires browser_navigate first.")
  ("question" :text :required t :doc "what you want to know about the page, in one line"))

(nlk:define-contract console-arguments
  (:doc "Evaluate a JavaScript expression in the page and answer its value — DevTools-console semantics, JSON-serialized. Console log and JavaScript-error capture are not available on Camofox, so a call with no expression answers what the page is. Requires browser_navigate first.")
  ("expression" :string :doc "JavaScript to evaluate, e.g. 'document.title' or 'document.querySelectorAll(\"a\").length'"))

(nlk:define-contract import-cookies-arguments
  (:doc "Import a Netscape-format cookies.txt into this session's Camofox user, to reach a site you are signed in to without logging in interactively. The file must sit under camofox.cookies_dir; the server's cookie route needs camofox.api_key.")
  ("cookies_path" :text :required t :doc "the file, relative to camofox.cookies_dir, e.g. 'linkedin.txt'")
  ("domain_suffix" :string :doc "keep only cookies whose domain ends with this, e.g. '.linkedin.com'"))

;;; --- answers --------------------------------------------------------------

(defun answer (&rest members)
  "MEMBERS as the JSON text a tool answers with."
  (nlk:encode-json-object (apply #'nlk:make-json-object members)))

(defparameter +no-tab-refusal+
  "No browser tab yet in this session: call browser_navigate first."
  "What a tool says when nothing has been navigated.")

(defun require-tab (session)
  "SESSION's tab id, or the refusal naming what to do first."
  (or (tab-session-tab-id session)
      (fail "~a" +no-tab-refusal+)))

;;; --- navigation policy ----------------------------------------------------
;;; The original refused a URL embedding a secret, a cloud metadata endpoint
;;; and a host on the user's blocklist. The first two are here; the blocklist
;;; is Nodecode's guard cell's to make, not this one's.

(defparameter +metadata-hosts+
  '("169.254.169.254" "metadata.google.internal" "100.100.100.200" "fd00:ec2::254" "instance-data")
  "Cloud metadata endpoints: a page there answers the machine's own credentials.")

(defparameter +secret-in-url+
  (cl-ppcre:create-scanner
   "(?i)sk-[a-z0-9_-]{10,}|gh[pousr]_[a-z0-9]{10,}|xox[abprs]-[a-z0-9-]{10,}|aiza[a-z0-9_-]{20,}|glpat-[a-z0-9_-]{10,}|sk_live_[a-z0-9]{10,}|npm_[a-z0-9]{20,}|hf_[a-z0-9]{20,}")
  "Shapes of an API key or token: a prompt injection would otherwise be able
to exfiltrate one through a URL the model types.")

(defun url-host (url)
  "URL's host, lowercased, or \"\" when it cannot be parsed."
  (let ((uri (ignore-errors (quri:uri url))))
    (string-downcase (or (and uri (quri:uri-host uri)) ""))))

(defun url-scheme (url)
  "URL's scheme, lowercased, or \"\" when it cannot be parsed."
  (let ((uri (ignore-errors (quri:uri url))))
    (string-downcase (or (and uri (quri:uri-scheme uri)) ""))))

(defun secret-in-url-p (url)
  "Whether URL carries a key-shaped secret, plainly or URL-encoded."
  (flet ((hit (text)
           (or (cl-ppcre:scan +secret-in-url+ text)
               (let ((key (setting :api-key)))
                 (and (stringp key) (> (length key) 7) (search key text))))))
    (or (hit url) (hit (ignore-errors (quri:url-decode url))))))

(defun policy-url (url)
  "URL normalized, or a refusal: a secret in the URL and a metadata endpoint
are caught before anything leaves this process."
  (let ((text (string-trim '(#\Space #\Tab #\Return #\Newline) (or url ""))))
    (when (zerop (length text))
      (fail "url is empty: say which page to open, e.g. https://example.com"))
    (unless (or (uiop:string-prefix-p "http://" text) (uiop:string-prefix-p "https://" text)
                (uiop:string-prefix-p "about:" text) (uiop:string-prefix-p "file:" text))
      (setf text (concatenate 'string "https://" text)))
    (when (secret-in-url-p text)
      (fail "Blocked: the URL carries what looks like an API key or a token. ~
Secrets do not travel in URLs — open the page and put the secret in a form instead."))
    (when (member (url-host text) +metadata-hosts+ :test #'string=)
      (fail "Blocked: ~a is a cloud metadata endpoint; a page there answers the machine's own credentials."
            (url-host text)))
    text))

(defun loopback-host-p (host)
  "Whether HOST is this machine's own loopback, by name or by address."
  (let ((host (string-downcase (string-trim "[]" (or host "")))))
    (or (member host '("localhost" "localhost.localdomain") :test #'string=)
        (cl-ppcre:scan "^127\\.[0-9.]+$" host)
        (string= host "::1"))))

(defun rewrite-loopback (url)
  "URL as the browser inside a Docker container must open it => (values URL REWRITE).
A loopback page URL is opened by the browser in its own container, where
loopback is the container; the operator's alias names the host REWRITE is
not NIL for, so the tool result can say what it did."
  (if (not (setting :rewrite-loopback-urls))
      (values url nil)
      (let ((uri (ignore-errors (quri:uri url))))
        (if (and uri
                 (member (quri:uri-scheme uri) '("http" "https") :test #'string=)
                 (loopback-host-p (quri:uri-host uri)))
            (let* ((host (quri:uri-host uri))
                   (named (setting :loopback-host-alias))
                   (alias (if (and named (plusp (length named))) named "host.docker.internal")))
              (setf (quri:uri-host uri) alias)
              (values (quri:render-uri uri) (list :from host :to alias)))
            (values url nil)))))

;;; --- snapshots ------------------------------------------------------------

(defun store-snapshot (text)
  "TEXT written under the home => the path, or NIL when it could not be."
  (handler-case
      (let* ((path (nlk:home (format nil "cache/camofox/snapshot-~a.txt"
                                      (hex-of text 10)))))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create :external-format :utf-8)
          (write-string text out))
        (namestring path))
    (error () nil)))

(defun truncate-snapshot (text)
  "TEXT cut at a line boundary to camofox.snapshot_threshold, with the whole
snapshot written to a file the model can read when it needs the rest."
  (let ((limit (setting :snapshot-threshold)))
    (if (<= (length text) limit)
        text
        (let* ((path (store-snapshot text))
               (lines (uiop:split-string text :separator '(#\Newline)))
               (room (- limit (if path (+ 120 (length path)) 110)))
               (kept '())
               (used 0))
          (loop for line in lines
                while (<= (+ used (length line) 1) room)
                do (push line kept) (incf used (1+ (length line))))
          (let ((rest (- (length lines) (length kept))))
            (format nil "~{~a~%~}[... ~d more lines cut~@[; the whole snapshot is ~a — read it with (sh \"cat ~a\")~]]"
                    (nreverse kept) rest path path))))))

(defun snapshot-of (session)
  "=> (values SNAPSHOT REFS) for SESSION's tab, the text cut to the threshold."
  (let ((data (call-object :get (tab-path session "snapshot") :params (user-params session))))
    (let ((text (text-member data "snapshot"))
          (refs (json-member data "refsCount" 0)))
      (values (truncate-snapshot text) (if (numberp refs) (truncate refs) 0)))))

;;; --- images ---------------------------------------------------------------

(defun line-value (line prefix)
  "LINE's text after PREFIX, trimmed, or NIL when LINE does not start with it."
  (let* ((trimmed (string-left-trim '(#\Space #\Tab) line))
         (start (search prefix trimmed)))
    (and start (zerop start) (string-trim '(#\Space #\Tab) (subseq trimmed (length prefix))))))

(defun image-alt (line)
  "The alt text of an `img \"alt\"' snapshot line, or NIL."
  (let* ((start (search "img \"" line))
         (open (and start (+ start 5))))
    (when open
      (let ((close (position #\" line :start open)))
        (and close (subseq line open close))))))

(defun snapshot-images (snapshot)
  "Images in a snapshot: each `img \"alt\"' with the /url: line under it."
  (let ((lines (uiop:split-string snapshot :separator '(#\Newline))))
    (loop for line in lines
          for rest on (cdr lines)
          for alt = (image-alt line)
          for inline = (line-value line "/url:")
          for under = (and rest (line-value (first rest) "/url:"))
          for src = (or inline under)
          when (or alt src)
            collect (nlk:json-object "src" (or src "") "alt" (or alt "")))))

;;; --- masking --------------------------------------------------------------

(defparameter +secret-shapes+
  (cl-ppcre:create-scanner
   "(?i)(sk-[a-z0-9_-]{8,}|gh[pousr]_[a-z0-9]{10,}|xox[abprs]-[a-z0-9-]{10,}|aiza[a-z0-9_-]{20,}|glpat-[a-z0-9_-]{10,}|sk_live_[a-z0-9]{10,})")
  "What a typed value looking like a key is masked as.")

(defun mask-text (text)
  "TEXT with key-shaped runs replaced: what the model reads back after typing."
  (if (stringp text)
      (cl-ppcre:regex-replace-all +secret-shapes+ text "[redacted]")
      ""))

;;; --- the tabs -------------------------------------------------------------

(defun page-object (session url)
  "The body every tab route takes: the user it belongs to and the URL."
  (nlk:json-object "userId" (tab-session-user-id session) "url" url))

(defun create-tab (session url)
  "POST /tabs => the server's answer, SESSION's tab id taken from it.
Sixty seconds: a cold server spends about thirty building the browser
context the first tab needs."
  (setf (tab-session-tab-id session) nil)
  (let ((data (call-object :post "/tabs"
                            :object (nlk:json-object "userId" (tab-session-user-id session)
                                                      "listItemId" (tab-session-session-key session)
                                                      "url" url)
                            :timeout 60)))
    (let ((tab (json-member data "tabId")))
      (unless (and (stringp tab) (plusp (length tab)))
        (fail "Camofox made no tab: ~a"
              (or (text-member data "error") "the answer carried no tabId")))
      (setf (tab-session-tab-id session) tab))
    data))

(defun navigate-tab (session url)
  "Open URL in SESSION's tab => (values DATA CREATED-P).
A 404 or a 410 means the id this session kept is dead (the server reaped the
tab, or restarted its browser), so a fresh one is made instead of failing
every call from here on."
  (if (not (tab-session-tab-id session))
      (values (create-tab session url) t)
      (multiple-value-bind (body status)
          (call :post (tab-path session "navigate") :object (page-object session url) :timeout 60)
        (cond ((member status '(404 410))
               (setf (tab-session-tab-id session) nil)
               (values (create-tab session url) t))
              ((>= status 300)
               (fail "Navigation failed: ~a" (explained :post "navigate" status body)))
              (t (values (answer-object body) nil))))))

(defun tab-url (session)
  "The URL the server reports for SESSION's tab, or \"\"."
  (let ((mine (find (tab-session-tab-id session) (listed-tabs session)
                    :key (lambda (tab) (json-member tab "tabId")) :test #'equal)))
    (text-member mine "url")))

(defun tab-title (session data)
  "The page title: from the navigate answer, else from the tab listing."
  (or (let ((title (json-member data "title")))
        (and (stringp title) (plusp (length title)) (redact title)))
      (let ((mine (find (tab-session-tab-id session) (listed-tabs session)
                        :key (lambda (tab) (json-member tab "tabId")) :test #'equal)))
        (text-member mine "title"))))

(defun vnc-url ()
  "The VNC address the server exposes, when it says it has one."
  (let* ((answer (ignore-errors
                   (call-object :get "/health" :timeout 5)))
         (port (json-member answer "vncPort")))
    (when (and (integerp port) (<= 1 port 65535))
      (format nil "http://~a:~d" (or (url-host (base-url)) "localhost") port))))

;;; --- the tools ------------------------------------------------------------

(defun run-navigate (arguments)
  "browser_navigate: open the page, answer the title and a snapshot."
  (let* ((url (policy-url (navigate-arguments-url arguments))))
    (multiple-value-bind (browser-url rewrite) (rewrite-loopback url)
      (let ((session (session-for)))
        (multiple-value-bind (data created) (navigate-tab session browser-url)
          (let ((final (or (text-member data "url") "")))
            (when (and (member (url-scheme browser-url) '("http" "https") :test #'string=)
                       (member final '("" "about:blank" "about:newtab") :test #'string=))
              (fail "Navigation reported success but the tab is still blank (~a) after asking for ~a. ~
The page did not load — call browser_navigate again or check the Camofox log."
                    (if (plusp (length final)) final "no URL") browser-url))
            (let ((snapshot nil) (count 0))
              (ignore-errors (setf (values snapshot count) (snapshot-of session)))
              (let* ((members (list "success" (not (null (json-member data "ok" t)))
                                    "url" (if (plusp (length final)) (redact final) browser-url)
                                    "title" (tab-title session data)
                                    "snapshot" (or snapshot "")
                                    "element_count" count))
                     (members (if created (append members (list "tab_created" t)) members))
                     (members (if rewrite
                                  (append members (list "requested_url" url
                                                        "url_rewrite" (nlk:make-json-object
                                                                       "from" (getf rewrite :from)
                                                                       "to" (getf rewrite :to))
                                                        "warning"
                                                        (format nil "Rewrote loopback URL for a Docker-hosted ~
Camofox: ~a -> ~a" (getf rewrite :from) (getf rewrite :to))))
                                  members))
                     (members (let ((vnc (vnc-url)))
                                (if vnc
                                    (append members (list "vnc_url" vnc
                                                          "vnc_hint" "The browser is visible over VNC: share the ~
link with the operator so they can watch it."))
                                    members))))
                (apply #'answer members)))))))))

(defun run-snapshot (arguments)
  "browser_snapshot: the page's accessibility tree, refs included."
  (declare (ignore arguments))
  (let ((session (session-for)))
    (require-tab session)
    (multiple-value-bind (snapshot count) (snapshot-of session)
      (answer "success" t "snapshot" snapshot "element_count" count))))

(defun tab-action (suffix object)
  "POST OBJECT to SESSION's tab route SUFFIX => the server's answer."
  (let ((session (session-for)))
    (require-tab session)
    (multiple-value-bind (body status)
        (call :post (tab-path session suffix)
              :object (apply #'nlk:make-json-object
                             "userId" (tab-session-user-id session) object))
      (cond ((member status '(404 410))
             (fail "The browser tab is gone (the Camofox server reaped or restarted it). ~
Call browser_navigate to open the page again."))
            ((>= status 300) (fail "~a" (explained :post suffix status body)))
            (t (or (answer-object body) (nlk:make-json-object)))))))

(defun clean-ref (ref)
  "REF without the '@' the tool convention prefixes it with."
  (string-left-trim "@" (or ref "")))

(defun run-click (arguments)
  "browser_click: click the element a snapshot ref names."
  (let* ((ref (clean-ref (click-arguments-ref arguments)))
         (data (tab-action "click" (list "ref" ref))))
    (answer "success" t "clicked" ref "url" (text-member data "url"))))

(defun run-type (arguments)
  "browser_type: put text in the field a snapshot ref names."
  (let* ((ref (clean-ref (type-arguments-ref arguments)))
         (text (type-arguments-text arguments))
         (data (tab-action "type" (list "ref" ref "text" text))))
    (declare (ignore data))
    (answer "success" t "typed" (mask-text text) "element" ref)))

(defun run-scroll (arguments)
  "browser_scroll: five fixed steps, as the original's scroll was."
  (let ((direction (scroll-arguments-direction arguments)))
    (dotimes (step 5)
      (declare (ignore step))
      (tab-action "scroll" (list "direction" direction)))
    (answer "success" t "scrolled" direction)))

(defun run-back (arguments)
  "browser_back: the previous page in the tab's history."
  (declare (ignore arguments))
  (let ((data (tab-action "back" '())))
    (answer "success" t "url" (text-member data "url"))))

(defun run-press (arguments)
  "browser_press: one key in the page."
  (let ((key (press-arguments-key arguments)))
    (tab-action "press" (list "key" key))
    (answer "success" t "pressed" key)))

(defun run-get-images (arguments)
  "browser_get_images: every image in the snapshot, with its alt text."
  (declare (ignore arguments))
  (let ((session (session-for)))
    (require-tab session)
    (let* ((data (call-object :get (tab-path session "snapshot") :params (user-params session)))
           (images (coerce (snapshot-images (text-member data "snapshot")) 'vector)))
      (answer "success" t "images" images "count" (length images)))))

(defun run-vision (arguments)
  "browser_vision: screenshot the tab, save the PNG, answer where it is."
  (let* ((session (session-for))
         (question (vision-arguments-question arguments)))
    (require-tab session)
    (multiple-value-bind (body status)
        (call :get (tab-path session "screenshot") :params (user-params session)
              :timeout (max 60 (setting :timeout-seconds)) :binary t)
      (when (>= status 300)
        (fail "The screenshot failed: ~a" (explained :get "screenshot" status body)))
      (let* ((octets (if (vectorp body) body (error "the screenshot answered no bytes")))
             (path (nlk:home (format nil "browser_screenshots/camofox-~a.png" (hex-of octets 8)))))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :supersede
                                   :if-does-not-exist :create :element-type '(unsigned-byte 8))
          (write-sequence octets out))
        (let ((namestring (namestring path)))
          (answer "success" t "question" question "screenshot_path" namestring
                  "note" (format nil "Look at the screenshot with (look ~s) and answer the question ~
from what you see." namestring)))))))

(defparameter +risky-eval+
  '(("\\bdocument\\s*\\.\\s*cookie\\b" . "document.cookie")
    ("\\b(?:localStorage|sessionStorage)\\b" . "web storage")
    ("\\bindexedDB\\b" . "IndexedDB")
    ("\\bcaches\\s*\\.\\s*(?:open|match|keys)\\b" . "Cache Storage")
    ("\\bnavigator\\s*\\.\\s*(?:clipboard|credentials|serviceWorker)\\b" . "a navigator API")
    ("\\b(?:fetch|XMLHttpRequest|WebSocket|EventSource)\\s*\\(" . "a network request")
    ("\\bnavigator\\s*\\.\\s*sendBeacon\\s*\\(" . "a network beacon"))
  "What a restricted browser_console expression may not name.")

(defun eval-refusal (expression)
  "The refusal a restricted expression gets, or NIL when it may run."
  (when (and (setting :restrict-evaluate) (stringp expression))
    (let ((reason (loop for (pattern . why) in +risky-eval+
                        when (cl-ppcre:scan pattern expression :case-insensitive-mode t)
                          return why)))
      (when reason
        (format nil "Blocked: browser_console(expression=...) named ~a while camofox.restrict_evaluate ~
is on. Use browser_snapshot or browser_get_images to inspect the page, or set ~
camofox.restrict_evaluate: false in config.jsonc to allow it." reason)))))

(defun run-console (arguments)
  "browser_console: evaluate JavaScript in the page, or say what is missing."
  (let ((expression (console-arguments-expression arguments)))
    (when (or (null expression) (zerop (length expression)))
      (return-from run-console
        (answer "success" t "console_messages" #() "js_errors" #()
                "total_messages" 0 "total_errors" 0
                "note" (concatenate 'string
                                  "Console log capture is not available on Camofox: call it with an expression, "
                                  "or use browser_snapshot or browser_vision to inspect the page."))))
    (let ((refusal (eval-refusal expression)))
      (when refusal (fail "~a" refusal)))
    (let ((session (session-for)))
      (require-tab session)
      (let* ((data (tab-action "evaluate" (list "expression" expression)))
             (value (json-member data "result")))
        (answer "success" t "result" (if (or (null value) (eq value :null) (stringp value)
                                              (numberp value) (hash-table-p value) (vectorp value))
                                          (or value "")
                                          (princ-to-string value))
                "result_type" (value-type value))))))

(defun value-type (value)
  "A name for what a JavaScript expression answered, as JSON names it."
  (typecase value
    (null "null")
    (string "string")
    (integer "number")
    (float "number")
    (hash-table "object")
    (vector "array")
    (t (string-downcase (princ-to-string (type-of value))))))

;;; --- cookies --------------------------------------------------------------

(defparameter +cookie-max-bytes+ (* 5 1024 1024)
  "How large a cookie file may be: five megabytes.")

(defparameter +cookie-max-count+ 500
  "How many cookies one import may carry.")

(defun cookies-dir ()
  "The directory browser_import_cookies reads, expanding a leading ~."
  (let ((configured (setting :cookies-dir)))
    (namestring
     (uiop:ensure-directory-pathname
      (nlk:expand-home (if (and configured (plusp (length configured)))
                           configured
                           "~/.camofox/cookies"))))))

(defun join-tabs (items)
  "ITEMS as one string, a tab between them: the value of a cookie field that
carries one, which a cookie file splits on."
  (with-output-to-string (out)
    (loop for (item . rest) on items
          do (write-string item out)
             (when rest (write-char #\Tab out)))))

(defun netscape-cookies (text)
  "A Netscape cookie file as JSON objects: BOM, #HttpOnly_ prefix, CRLF and
tab-containing values handled, as camofox-browser's own parser does."
  (let ((cookies '()))
    (dolist (raw (uiop:split-string (string-left-trim '(#\Page #\Newline) text)
                                     :separator '(#\Newline)))
      (let* ((line (string-trim '(#\Space #\Tab #\Return) raw))
             (http-only (uiop:string-prefix-p "#HttpOnly_" line)))
        (unless (or (zerop (length line)) (and (char= (char line 0) #\#) (not http-only)))
          (let* ((body (if http-only (subseq line (length "#HttpOnly_")) line))
                 (parts (uiop:split-string body :separator '(#\Tab))))
            (when (>= (length parts) 7)
              (let ((expires (ignore-errors (parse-integer (nth 4 parts)))))
                (when expires
                  (push (nlk:json-object "name" (nth 5 parts)
                                          "value" (join-tabs (subseq parts 6))
                                          "domain" (nth 0 parts)
                                          "path" (nth 2 parts)
                                          "expires" expires
                                          "httpOnly" http-only
                                          "secure" (string-equal (nth 3 parts) "TRUE"))
                        cookies))))))))
    (nreverse cookies)))

(defun cookie-file (relative)
  "RELATIVE resolved under the cookies dir => its text, refused when it
escapes the directory, is missing, or is too large."
  (let* ((name (string-trim '(#\Space #\Tab) (or relative "")))
         (base (cookies-dir))
         (parsed (and (plusp (length name)) (ignore-errors (uiop:parse-native-namestring name)))))
    (unless parsed
      (fail "cookies_path is empty: name a file under ~a" base))
    (when (uiop:absolute-pathname-p parsed)
      (fail "cookies_path must be relative to ~a, not absolute: ~a" base name))
    (when (member ".." (uiop:split-string name :separator '(#\/)) :test #'equal)
      (fail "cookies_path must stay inside ~a, so no ../ in it: ~a" base name))
    (let ((path (namestring (uiop:merge-pathnames* parsed (uiop:ensure-directory-pathname base)))))
      (unless (uiop:string-prefix-p base path)
        (fail "cookies_path must stay inside ~a: ~a" base name))
      (unless (probe-file path)
        (fail "Cookie file not found: ~a (looked in ~a)" name base))
      (let ((size (with-open-file (in path :element-type '(unsigned-byte 8)) (file-length in))))
        (when (> size +cookie-max-bytes+)
          (fail "Cookie file too large: ~d bytes, the most is ~d." size +cookie-max-bytes+)))
      (uiop:read-file-string path))))

(defun run-import-cookies (arguments)
  "browser_import_cookies: load a cookies.txt into this session's Camofox user."
  (unless (let ((key (setting :api-key))) (and key (plusp (length key))))
    (fail "camofox.api_key is not set. Cookie import is disabled unless the Camofox server and this ~
cell share an API key."))
  (let* ((relative (import-cookies-arguments-cookies-path arguments))
         (suffix (import-cookies-arguments-domain-suffix arguments))
         (found (netscape-cookies (cookie-file relative)))
         (cookies (if (and suffix (plusp (length suffix)))
                      (remove-if-not (lambda (cookie) (uiop:string-suffix-p (json-member cookie "domain") suffix))
                                     found)
                      found)))
    (when (null cookies)
      (fail "No cookies in ~a~@[ for domains ending ~a~]." relative suffix))
    (when (> (length cookies) +cookie-max-count+)
      (fail "Too many cookies: ~d, the most is ~d per import." (length cookies) +cookie-max-count+))
    (let* ((session (session-for))
           (data (call-object :post (format nil "/sessions/~a/cookies" (tab-session-user-id session))
                              :object (nlk:json-object "cookies" (coerce cookies 'vector)))))
      (answer "success" t "imported" (or (json-member data "count") (length cookies))
              "user_id" (tab-session-user-id session)))))
