;;;; cell.lisp --- the terminals, served from the gateway's own port.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two addresses. /terminal/ is the page (page/: index.html, terminal.js and
;;;; the vendored xterm.js), public like the web cell's page and carrying no
;;;; secret. /api/terminal/socket is one WebSocket per screen: the page says
;;;; hello with the operator token (a browser cannot put a Bearer on a socket,
;;;; so the token rides the first frame, as the gateway's own handshake takes
;;;; it; an upgrade that does carry the Bearer is let in at the door), then
;;;; opens a terminal or attaches to one by id. Typed input, resizes and close
;;;; go up as JSON text frames; the terminal's output comes down as binary
;;;; frames, the pty's own bytes, which xterm.js decodes. The gateway already
;;;; refuses any Host that is not loopback; this socket also refuses an Origin
;;;; other than the page's own, so another site's page cannot open it.
;;;;
;;;; A terminal belongs to the image, not to the socket: a page that goes away
;;;; detaches, and the next one attaches by id and redraws from the backlog.
;;;; Only close, the shell's own exit, or the image's end stops it.

(in-package #:nodecode-terminal)

(nlk:define-peripheral terminal :not-running t)

;;; --- the page -----------------------------------------------------------------

(defparameter *page-directory*
  (asdf:system-relative-pathname "nodecode-terminal" "page/")
  "Where the page's files are.")

(defparameter *page-types*
  '(("html" . "text/html; charset=utf-8")
    ("js" . "text/javascript; charset=utf-8")
    ("css" . "text/css; charset=utf-8"))
  "The media type of each kind of file the page has.")

(defparameter *page-headers*
  (list :cache-control "no-cache"
        :x-content-type-options "nosniff"
        :referrer-policy "no-referrer"
        ;; xterm.js writes its cell styles into a <style> of its own, hence
        ;; 'unsafe-inline' for styles alone; no script runs but the page's.
        :content-security-policy
        (format nil "default-src 'self'; connect-src 'self'; img-src 'self' data:; ~
                     style-src 'self' 'unsafe-inline'; script-src 'self'; base-uri 'none'; ~
                     form-action 'none'; frame-ancestors 'self'"))
  "What every file of the page is sent with.")

(defun page-file (name)
  "The page's file NAME with its media type, or NIL for a name it does not have."
  (when (and (plusp (length name))
             (every (lambda (char) (or (alphanumericp char) (find char "-_."))) name)
             (char/= #\. (char name 0)))
    (let* ((file (probe-file (merge-pathnames name *page-directory*)))
           (type (and file (cdr (assoc (pathname-type file) *page-types* :test #'string-equal)))))
      (and type (cons file type)))))

(defun serve-page (env)
  "GET /terminal/NAME: the page's file NAME, index.html for none."
  (let* ((name (subseq (getf env :path-info) (length "/terminal/")))
         (found (page-file (if (string= name "") "index.html" name))))
    (if (and found (member (getf env :request-method) '(:get :head)))
        (list 200 (list* :content-type (cdr found) *page-headers*) (car found))
        (list 404 '(:content-type "text/plain; charset=utf-8") '("not found")))))

(defun redirect-to-page (env)
  "GET /terminal: the page is a directory; the link's fragment rides the redirect."
  (declare (ignore env))
  (list 302 '(:location "/terminal/") '()))

;;; --- the socket ----------------------------------------------------------------

(defun json-text (&rest pairs)
  "PAIRS as one JSON object's text."
  (sb-ext:octets-to-string (nlk:encode-json-octets (apply #'nlk:make-json-object pairs))
                           :external-format :utf-8))

(defun token-matches-p (offered)
  "OFFERED is this gateway's operator token, compared in constant time."
  (let ((token (nth-value 1 (nle:gateway-endpoint))))
    (and (stringp offered) (stringp token)
         (= (length offered) (length token))
         (zerop (loop for a across offered for b across token
                      sum (logxor (char-code a) (char-code b)))))))

(defun foreign-origin-p (env)
  "ENV's socket was asked for by a page this gateway did not serve."
  (let* ((headers (getf env :headers))
         (origin (gethash "origin" headers))
         (host (gethash "host" headers)))
    (and origin (not (string-equal origin (format nil "http://~a" host))))))

(defun terminal-row (terminal)
  "What a page is told of TERMINAL."
  (nlk:make-json-object "id" (terminal-id terminal) "pid" (terminal-pid terminal)
                        "cwd" (or (terminal-cwd terminal) "")
                        "cols" (terminal-cols terminal) "rows" (terminal-rows terminal)))

(defparameter *ping-seconds* 30
  "How often a screen's socket is pinged. The gateway hangs up a connection
that sends nothing for its read timeout (75 s), and a terminal nobody types
into sends nothing; the browser's pong is what keeps it.")

(defstruct (screen (:constructor make-screen (ws authorized)))
  "One page's socket: whether it is let in, and the terminal it shows."
  ws
  authorized
  (terminal nil)
  (sink nil)
  (open t)
  (lock (sb-thread:make-mutex :name "terminal screen")))

(defun ping-screen (screen)
  "Ping SCREEN's socket every *PING-SECONDS* while it is open."
  (loop (loop repeat (* 10 *ping-seconds*)
              while (screen-open screen)
              do (sleep 0.1))
        (unless (screen-open screen) (return))
        (sb-thread:with-mutex ((screen-lock screen))
          (ignore-errors (wsd:send-ping (screen-ws screen))))))

(defun screen-send (screen kind data)
  "Send DATA down SCREEN's socket: KIND :TEXT a string, :BINARY octets."
  (sb-thread:with-mutex ((screen-lock screen))
    (ignore-errors
     (if (eq kind :binary)
         (wsd:send-binary (screen-ws screen) data)
         (wsd:send-text (screen-ws screen) data)))))

(defun screen-error (screen message)
  (screen-send screen :text (json-text "type" "error" "message" message)))

(defun screen-show (screen terminal how)
  "Make SCREEN show TERMINAL: told HOW (opened or attached), then its backlog,
then its output as it comes."
  (screen-leave screen)
  (let ((sink (lambda (kind data)
                (if (eq kind :output)
                    (screen-send screen :binary data)
                    (screen-send screen :text (json-text "type" "exit" "id" (terminal-id terminal) "code" data))))))
    (setf (screen-terminal screen) terminal (screen-sink screen) sink)
    (screen-send screen :text (apply #'json-text "type" how
                                     (alexandria:hash-table-plist (terminal-row terminal))))
    (let ((backlog (terminal-attach terminal sink)))
      (when (plusp (length backlog))
        (screen-send screen :binary backlog)))
    (let ((exit (terminal-exit terminal)))
      (when exit
        (screen-send screen :text (json-text "type" "exit" "id" (terminal-id terminal) "code" exit))))))

(defun screen-leave (screen)
  "SCREEN stops showing its terminal, which goes on running."
  (let ((terminal (screen-terminal screen)))
    (when terminal
      (terminal-detach terminal (screen-sink screen))
      (setf (screen-terminal screen) nil (screen-sink screen) nil))))

(defun on-frame (screen text)
  "One JSON frame TEXT from SCREEN's page."
  (let* ((frame (ignore-errors (nlk:decode-json text)))
         (type (and (hash-table-p frame) (gethash "type" frame)))
         (terminal (screen-terminal screen)))
    (flet ((field (key) (and (hash-table-p frame) (gethash key frame))))
      (cond ((not (stringp type)) (screen-error screen "a frame is a JSON object with a type"))
            ((string= type "hello")
             (if (or (screen-authorized screen) (token-matches-p (field "token")))
                 (progn (setf (screen-authorized screen) t)
                        (screen-send screen :text (json-text "type" "welcome")))
                 (progn (screen-error screen "not this gateway's token")
                        (wsd:close-connection (screen-ws screen)))))
            ((not (screen-authorized screen))
             (screen-error screen "say hello with the operator token first")
             (wsd:close-connection (screen-ws screen)))
            ((string= type "open")
             (handler-case
                 (screen-show screen (open-terminal :cwd (let ((cwd (field "cwd"))) (and (stringp cwd) (plusp (length cwd)) cwd))
                                                    :cols (field "cols") :rows (field "rows"))
                              "opened")
               (error (condition) (screen-error screen (princ-to-string condition)))))
            ((string= type "attach")
             (let ((found (and (stringp (field "id")) (find-terminal (field "id")))))
               (if found
                   (screen-show screen found "attached")
                   (screen-error screen "no such terminal"))))
            ((string= type "list")
             (screen-send screen :text (json-text "type" "terminals"
                                                  "terminals" (map 'vector #'terminal-row (list-terminals)))))
            ((null terminal) (screen-error screen "open or attach a terminal first"))
            ((string= type "input")
             (when (stringp (field "data"))
               (terminal-write terminal (field "data"))))
            ((string= type "resize")
             (terminal-resize terminal (field "cols") (field "rows")))
            ((string= type "close")
             ;; Still attached, so this page is told the exit like any other.
             (sb-thread:make-thread (lambda () (close-terminal terminal)) :name "terminal close"))
            (t (screen-error screen (format nil "no frame type ~s" type)))))))

(defun terminal-socket (env)
  "GET /api/terminal/socket: a page's WebSocket to the terminals."
  (if (foreign-origin-p env)
      (list 403 '(:content-type "application/json")
            (list (json-text "error" (nlk:make-json-object "code" "origin_refused"
                                                           "message" "a socket from a page this gateway did not serve"))))
      (let* ((ws (wsd:make-server env))
             (screen (make-screen ws (nle::gateway-http-authorized-p env))))
        (wsd:on :message ws (lambda (message)
                              (if (stringp message)
                                  (on-frame screen message)
                                  (screen-error screen "frames to the terminal are JSON text"))))
        (wsd:on :open ws (lambda ()
                           (sb-thread:make-thread #'ping-screen :name "terminal ping" :arguments (list screen))))
        (wsd:on :close ws (lambda (&key &allow-other-keys)
                            (setf (screen-open screen) nil)
                            (screen-leave screen)))
        (lambda (responder)
          (declare (ignore responder))
          (wsd:start-connection ws)))))

(nle:define-cell terminal
  (:section ("terminal")
    (:guide "nothing to set: open /terminal/ on the page's own address"))
  (:route "/terminal/" #'serve-page :public t)
  (:route "/terminal" #'redirect-to-page :public t)
  ;; Public at the HTTP door only: the socket lets nothing in before a hello
  ;; with the operator token (or an upgrade that carried its Bearer).
  (:route "/api/terminal/socket" #'terminal-socket :public t))
