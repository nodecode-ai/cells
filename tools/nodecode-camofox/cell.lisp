;;;; cell.lisp --- the manual, the section, the eleven tools, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model finds the tools in its own catalog, listed with the schema
;;;; each declaration in tools.lisp projects; (help :camofox) carries the
;;;; rules for using them, and every request carries the one line the :HELP
;;;; clause gives. Nothing here starts a thread: the Camofox server is the
;;;; operator's, this cell only talks to it.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "camofox": {"url": "http://localhost:9377", "api_key": "", ...}
;;;; A vetoed section ("enabled": false) registers no tool at all.

(in-package #:nodecode-camofox)

(defparameter +primer+
  "Camofox drives a browser of your own: a self-hosted camofox-browser server (Camoufox, a Firefox
fork with fingerprint spoofing) that the operator runs and this cell talks to. The tools are
browser_navigate, browser_snapshot, browser_click, browser_type, browser_scroll, browser_back,
browser_press, browser_get_images, browser_vision, browser_console and browser_import_cookies.

  browser_navigate url          opens this session's tab and answers the title, the landed URL and
                                an accessibility snapshot with element refs. It must come first.
  browser_snapshot              the snapshot again, after something changed the page.
  browser_click ref             click the element a ref names, e.g. \"@e5\".
  browser_type ref text         clear the field a ref names and type text into it.
  browser_press key             Enter, Tab, Escape, ArrowDown, a shortcut.
  browser_scroll direction      up or down, five steps.
  browser_back                  the previous page in the tab's history.
  browser_get_images            every image on the page: its url and alt text.
  browser_vision question       screenshot the tab and answer where the PNG was saved; look at it
                                with (look PATH) and answer from what you see.
  browser_console expression    evaluate JavaScript in the page (DevTools semantics). Without one
                                it says so: Camofox captures no console log.
  browser_import_cookies path   load a Netscape cookies.txt from camofox.cookies_dir into this
                                session's browser user, so a signed-in site opens signed in.

Rules. Navigate before anything else: every other tool refuses until a tab exists. A ref belongs
to the snapshot it came from, so call browser_snapshot again after a click, a navigation or a
form submit that changed the page. Prefer a fetch tool for a plain page or an API endpoint; the
browser is for what has to be interacted with. A tab the server reaped answers \"the tab is gone\":
navigate again. A URL carrying a key-shaped secret, or one pointing at a cloud metadata endpoint,
is refused before the request leaves. An error is ERROR: CAMOFOX-ERROR with the reason: fix that,
do not repeat the call."
  "What (help :camofox) answers while the cell runs.")

(defun no-arguments (description)
  "The schema of a tool whose call carries nothing."
  (list :description description
        :parameters (nlk:json-object "type" "object" "properties" (nlk:make-json-object))))

(defparameter +snapshot-schema+
  (no-arguments
   "The current page as a text accessibility snapshot: its interactive elements, each with the ref
browser_click and browser_type take (like @e1, @e2). Use it to refresh after an interaction that
changed the page; browser_navigate already answers one. A snapshot over camofox.snapshot_threshold
characters is cut at a line boundary and the whole tree is written to a file the answer names.
Requires browser_navigate first.")
  "browser_snapshot takes nothing.")

(defparameter +back-schema+
  (no-arguments
   "Go back to the previous page in this tab's history. Requires browser_navigate first.")
  "browser_back takes nothing.")

(defparameter +images-schema+
  (no-arguments
   "Every image on the current page, with the URL and the alt text of each: the way to find an
image before asking browser_vision about it. Requires browser_navigate first.")
  "browser_get_images takes nothing.")

(defun start ()
  "Forget every tab this image held: a start is a fresh look at the server."
  (with-session-lock (clrhash *sessions*))
  (nle:on-stop (lambda () (with-session-lock (clrhash *sessions*)))))

(nle:define-cell camofox
  (:section ("camofox")
    (:guide "run the camofox-browser server yourself (npm install && npm start, or docker run -d -p 9377:9377 ghcr.io/jo-inc/camofox-browser), then put its address in url; api_key only when the server was started with one; managed_persistence keeps one browser profile across sessions; cookies_dir is where browser_import_cookies reads; - /cells shows whether the folder started")
    ("url" :string :required t
     :doc "the camofox-browser server's address, e.g. http://localhost:9377; this cell never installs or starts it")
    ("api_key" :secret
     :doc "the bearer token, only when the server was started with CAMOFOX_API_KEY; also what browser_import_cookies needs")
    ("managed_persistence" :boolean :default nil
     :doc "one browser profile per profile of this home, so cookies and logins survive a session, instead of a new Camofox user each time")
    ("user_id" :string
     :doc "the Camofox userId an app that owns the visible browser made: the tools then work inside it, and nothing hides it")
    ("session_key" :string
     :doc "the key tabs are opened under, matched when adopt_existing_tab takes an open one; unset, it is this session's own")
    ("adopt_existing_tab" :boolean :default nil
     :doc "take an already open tab carrying this session's key before opening a new one")
    ("cookies_dir" :path :default "~/.camofox/cookies"
     :doc "where browser_import_cookies reads Netscape cookie files")
    ("snapshot_threshold" :integer :default 15000 :min 1000
     :doc "the longest snapshot the model reads, in characters; the rest goes to a file")
    ("timeout_seconds" :integer :default 30 :min 5
     :doc "how long one request to the server may take")
    ("restrict_evaluate" :boolean :default nil
     :doc "refuse browser_console expressions naming cookies, web storage or the network")
    ("rewrite_loopback_urls" :boolean :default nil
     :doc "for a Camofox in Docker: rewrite localhost and 127.0.0.1 page URLs to loopback_host_alias, which the browser inside the container can reach")
    ("loopback_host_alias" :string :default "host.docker.internal"
     :doc "the host name a rewritten loopback page URL takes"))
  (:settings
   (lambda (values table)
     (declare (ignore table))
     (let ((url (string-right-trim "/" (or (getf values :url) ""))))
       (when (plusp (length url))
         (unless (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url))
           (nlk:config-error "camofox.url is ~s; it is the server's web address, http:// or https://"
                             url)))
       (setf (getf values :url) url)
       values)))
  (:tool "browser_navigate" (nlk:contract 'navigate-arguments) 'run-navigate)
  (:tool "browser_snapshot" +snapshot-schema+ 'run-snapshot)
  (:tool "browser_click" (nlk:contract 'click-arguments) 'run-click)
  (:tool "browser_type" (nlk:contract 'type-arguments) 'run-type)
  (:tool "browser_scroll" (nlk:contract 'scroll-arguments) 'run-scroll)
  (:tool "browser_back" +back-schema+ 'run-back)
  (:tool "browser_press" (nlk:contract 'press-arguments) 'run-press)
  (:tool "browser_get_images" +images-schema+ 'run-get-images)
  (:tool "browser_vision" (nlk:contract 'vision-arguments) 'run-vision)
  (:tool "browser_console" (nlk:contract 'console-arguments) 'run-console)
  (:tool "browser_import_cookies" (nlk:contract 'import-cookies-arguments) 'run-import-cookies)
  (:help :camofox "camofox: browser_navigate and the other browser_* tools drive a self-hosted Camoufox browser"
         +primer+)
  (:start #'start))
