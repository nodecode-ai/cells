;;;; stdio-test.lisp --- the process transport against the python fixture.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The whole cell is started over one fixture server per test; the
;;;; generated function is reached through its symbol name because the
;;;; symbol is interned only when the catalog arrives.

(in-package #:nodecode.test)

(defun mcp-probe-process (&optional (name "probe") &aux (client (mcp::server-client (mcp-server name))))
  "The fixture child behind server NAME, or NIL."
  (and client (mcp::stdio-transport-pid (mcp::client-transport client))))

(deftest mcp-cell-stdio-connects-lists-and-calls (with-mcp-cell ("probe" (mcp-fixture-entry)))
  (is (functionp stop) "start-cell returns a stop thunk")
  (is (eq :ready (mcp-wait-ready "probe")) "the fixture connects")
  (let ((snapshot (mcp::server-snapshot (mcp-server "probe"))))
    (is (= 10 (getf snapshot :tool-count)) "the catalog is listed")
    (is (equal "fixture" (nlk:json-value (getf snapshot :server-info) :string "name"))))
  (let ((answer (mcp:call "probe" "echo" :message "hi")))
    (is (uiop:string-prefix-p "echo: hi" answer) "a call answers the text blocks"))
  (let ((symbol (find-symbol "PROBE/ECHO" :nodecode-mcp)))
    (is (and symbol (fboundp symbol)) "the tool is a function")
    (is (eq :external (nth-value 1 (find-symbol "PROBE/ECHO" :nodecode-mcp))))
    (let ((answer (funcall symbol :message "x" :per-page 5 :include-snapshot t)))
      (is (search "\"per_page\": 5" answer) ":per-page folds to per_page")
      (is (search "\"includeSnapshot\": true" answer) ":include-snapshot folds to includeSnapshot")
      (is (search "\"message\": \"x\"" answer) "and the message rides")))
  (let ((answer (mcp:call "probe" "echo" "message" "s" "odd_key" 1)))
    (is (search "\"odd_key\": 1" answer) "string keys pass verbatim"))
  (is (search "echo: folded" (mcp:call "probe" "Echo" :message "folded"))))

(deftest mcp-cell-stdio-tool-error-and-unknown-tool (with-ready-mcp ("probe" (mcp-fixture-entry)))
  (let ((condition (signals-error mcp:mcp-tool-error (mcp:call "probe" "fail"))))
    (is (search "probe/fail: echo refused: fail requested" (princ-to-string condition))))
  (is (search "echo" (refusal-text mcp:mcp-unknown-tool (mcp:call "probe" "nonesuch"))))
  (is (signals-error mcp:mcp-unknown-server (mcp:call "ghost" "echo")))
  (is (eq :ready (mcp::server-state (mcp-server "probe"))) "none of that cost the connection"))

(deftest mcp-cell-stdio-timeout-keeps-the-connection-and-skips-the-late-reply ()
  (with-ready-mcp ("probe" (mcp-fixture-entry))
    (is (search "connection is kept"
                (refusal-text mcp:mcp-timeout (mcp:call "probe" "late" :ms 800 :timeout 0.3))))
    (is (eq :ready (mcp::server-state (mcp-server "probe"))) "the server stays ready")
    (is (uiop:string-prefix-p "echo: again" (mcp:call "probe" "echo" :message "again")))
    (is (= 1 (mcp::client-stale-count (mcp::server-client (mcp-server "probe")))))))

(deftest mcp-cell-stdio-death-then-lazy-reconnect (with-ready-mcp ("probe" (mcp-fixture-entry)))
  (let ((first-process (mcp-probe-process))
        (condition (signals-error mcp:mcp-offline (mcp:call "probe" "die"))))
    (is (search ".stderr.log" (princ-to-string condition)))
    (is (eq :error (mcp::server-state (mcp-server "probe"))) "the server is in error")
    (is (uiop:string-prefix-p "echo: back" (mcp:call "probe" "echo" :message "back")))
    (is (eq :ready (mcp::server-state (mcp-server "probe"))) "and the server is ready again")
    (is (not (eq first-process (mcp-probe-process))) "on a fresh process")
    (is (search "dying on request"
                (uiop:read-file-string (mcp::stderr-log-path "probe"))))))

(deftest mcp-cell-stdio-garbage-line-server-requests-and-list-changed ()
  (with-ready-mcp ("probe" (mcp-fixture-entry))
    (is (equal "exploded and survived"
               (handler-bind ((warning #'muffle-warning))
                 (mcp:call "probe" "explode"))))
    (is (equal "roots answered: -32601" (mcp:call "probe" "roots")))
    (is (equal "ping answered: {}" (mcp:call "probe" "ping_me")))
    (is (equal "changed" (mcp:call "probe" "changed")) "the changed tool answers")
    (is (= 11 (length (mcp::server-tools (mcp-server "probe")))))
    (let ((symbol (find-symbol "PROBE/ECHO2" :nodecode-mcp)))
      (is (and symbol (fboundp symbol)) "the new tool is a function"))
    (is (search "probe/echo2" (mcp::primer-text)) "and the primer names it")))

(deftest mcp-cell-stdio-handshake-timeout-fails-the-server ()
  (with-mcp-cell ("slow" (let ((entry (mcp-fixture-entry "--slow-init")))
                            (setf (gethash "timeout_ms" entry) 500)
                            entry))
    (is (eq :error (mcp-wait-ready "slow" :timeout 10)) "a slow initialize ends in error")
    (is (search "initialize" (mcp::server-error-text (mcp-server "slow"))))
    (is (null (mcp::server-client (mcp-server "slow"))) "with the child closed")))

(deftest mcp-cell-stdio-pages-and-stderr-and-stop ()
  (with-ready-mcp ("probe" (mcp-fixture-entry "--pages" "--stderr-noise"))
    (is (= 10 (length (mcp::server-tools (mcp-server "probe")))))
    (is (search "hello from stderr"
                (uiop:read-file-string (mcp::stderr-log-path "probe"))))
    (let ((pid (mcp-probe-process)))
      (is (nlk:pid-live-p pid) "the child is alive while running")
      (funcall stop)
      (setf stop nil)
      (is (not (nlk:pid-live-p pid)) "stop leaves no live child, reaped")
      (is (null mcp::*registry*) "and no registry")
      (is (not (fboundp (find-symbol "PROBE/ECHO" :nodecode-mcp)))))))

;;; --- a command that signs in on its own --------------------------------------
;;; mcp-remote and its kind sign in to a remote server themselves: they print
;;; the address to open on stderr and hold every request until a browser comes
;;; back to their loopback listener. The fixture's --sign-in does the same.

(defun mcp-sign-in-entry ()
  "The fixture signing in on its own, with a deadline only failing fast beats."
  (let ((entry (mcp-fixture-entry "--sign-in")))
    (setf (gethash "timeout_ms" entry) 30000)
    entry))

(defun mcp-landing (url &key (code "fixture-code") state)
  "The address a browser lands on once URL is approved: its redirect_uri with a
code and URL's state (or STATE)."
  (let ((params (quri:uri-query-params (quri:uri url))))
    (flet ((param (key) (cdr (assoc key params :test #'string=))))
      (format nil "~a?code=~a&state=~a" (param "redirect_uri") code (or state (param "state"))))))

(defun mcp-seconds-since (start)
  (/ (- (get-internal-real-time) start) internal-time-units-per-second))

(defun mcp-sign-in-url (name)
  (getf (mcp::server-sign-in (mcp-server name)) :url))

(deftest mcp-cell-stdio-sign-in-fails-fast-and-keeps-its-command ()
  (with-mcp-cell ("pay" (mcp-sign-in-entry))
    (let ((start (get-internal-real-time)))
      (is (eq :error (mcp-wait-ready "pay" :timeout 20)) "a command waiting on a browser settles in error")
      (is (< (mcp-seconds-since start) 10) "long before its 30 s deadline"))
    (let ((server (mcp-server "pay")) (url (mcp-sign-in-url "pay")))
      (is (search "pay needs a sign-in" (or server.error-text "")) "the error says what it waits for")
      (is (search "/mcp login pay" (or server.error-text "")) "and the verb that answers it")
      (is (uiop:string-prefix-p "https://auth.invalid/authorize?" (or url "")) "the address its command printed is kept")
      (is (and server.client (nlk:pid-live-p (mcp-probe-process "pay"))) "the command is kept: it is the one the browser answers")
      (is (search (or url "none") (mcp:status)) "status names the address whole")
      (let* ((start (get-internal-real-time))
             (text (refusal-text mcp:mcp-offline (mcp:call "pay" "echo" :message "x"))))
        (is (search (or url "none") text) "a call fails with the address to open")
        (is (< (mcp-seconds-since start) 6) "at once, not at its deadline"))
      (is (search (or url "none") (mcp:login "pay")) "login answers the address")
      (is (search "does not belong" (mcp:login "pay" "http://127.0.0.1:1/oauth/callback?code=c&state=s")) "an address for another listener is refused, never fetched")
      (is (search "does not belong" (mcp:login "pay" (mcp-landing url :state "stale"))) "and one for another sign-in")
      (is (search "signed in" (mcp:login "pay" (mcp-landing url))) "the landing address handed to its command connects it")
      (is (eq :ready server.state) "the server is ready")
      (is (null server.sign-in) "with nothing left waiting")
      (is (uiop:string-prefix-p "echo: paid" (mcp:call "pay" "echo" :message "paid")) "and calls go through"))))

(deftest mcp-cell-stdio-sign-in-asked-mid-session ()
  (with-mcp-cell ("pay" (mcp-sign-in-entry))
    (mcp-wait-ready "pay" :timeout 20)
    (mcp:login "pay" (mcp-landing (mcp-sign-in-url "pay")))
    (is (equal "signed out" (mcp:call "pay" "sign_out")) "signed in, then out")
    (let* ((start (get-internal-real-time))
           (text (refusal-text mcp:mcp-offline (mcp:call "pay" "echo" :message "held")))
           (url (mcp-sign-in-url "pay")))
      (is (and url (search url text)) "a held call fails with the new address")
      (is (< (mcp-seconds-since start) 6) "at once")
      (is (search "goes through once" text) "and says the held call still runs")
      (is (search "handed" (mcp:login "pay" (mcp-landing url))) "its landing address is handed over"))
    (is (uiop:string-prefix-p "echo: after" (mcp:call "pay" "echo" :message "after")) "the next call goes through")
    (is (eq :ready (mcp::server-state (mcp-server "pay"))))))

(deftest mcp-cell-stdio-an-address-on-stderr-that-is-answered-anyway ()
  (with-ready-mcp ("probe" (mcp-fixture-entry "--chatty"))
    (is (equal "chatty answered" (mcp:call "probe" "chatty")) "a server that answers is never taken for one waiting")
    (is (eq :ready (mcp::server-state (mcp-server "probe"))))
    (is (null (mcp::server-sign-in (mcp-server "probe"))))))

(deftest mcp-cell-stdio-authorization-request-reads-an-oauth-address ()
  (multiple-value-bind (url redirect)
      (mcp::authorization-request "https://access.example/mcp/oauth2/authorize?response_type=code&client_id=c1&redirect_uri=http%3A%2F%2Flocalhost%3A42286%2Foauth%2Fcallback&state=s1&scope=mcp")
    (is (uiop:string-prefix-p "https://access.example/mcp/oauth2/authorize?" url))
    (is (equal "http://localhost:42286/oauth/callback" redirect)))
  (is (null (mcp::authorization-request "[1] Connecting to remote server: https://mcp.example.com")) "a plain address is not one")
  (is (null (mcp::authorization-request "see https://x.example/authorize?client_id=c1")) "nor one without response_type=code"))
