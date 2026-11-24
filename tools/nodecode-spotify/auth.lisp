;;;; auth.lisp --- the user's own Spotify app and the tokens it grants.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Spotify lets no app share a public client: every user registers a small
;;;; app of their own and signs in against it. Authorization Code with PKCE
;;;; needs no secret, so the whole flow is three facts -- a verifier, an
;;;; authorize URL, one token POST -- and the tokens live in a 0600 file under
;;;; the home, never in the pushed folder and never in the shared config.
;;;;
;;;; Hermes ran a loopback listener for the callback. This rewrite opens no
;;;; port: Spotify redirects the browser to the redirect URI the user
;;;; registered, the page fails to load, and the address it landed on is the
;;;; code. /spotify login <that-url> finishes the exchange.

(in-package #:nodecode-spotify)

(defparameter +accounts-base+ "https://accounts.spotify.com"
  "Where Spotify's authorization and token endpoints are.")

(defparameter +api-base+ "https://api.spotify.com/v1"
  "Where the Web API is.")

(defparameter +redirect-uri+ "http://127.0.0.1:43827/spotify/callback"
  "The redirect URI a Spotify app registers: a loopback address is the one
place Spotify accepts plain http.")

(defparameter +scope+
  (format nil "~{~a~^ ~}"
          '("user-modify-playback-state" "user-read-playback-state"
            "user-read-currently-playing" "user-read-recently-played"
            "playlist-read-private" "playlist-read-collaborative"
            "playlist-modify-public" "playlist-modify-private"
            "user-library-read" "user-library-modify"))
  "What the seven tools need, asked for at sign-in.")

(defparameter +refresh-skew+ 120
  "Seconds before an expiry this cell refreshes: Spotify's clock is not ours.")

(defvar *auth-file* nil
  "The token file, or NIL for the home's own (spotify/auth.json under it).
A test binds it so no run touches the operator's sign-in.")

;;; --- where the state is -------------------------------------------------------

(defun auth-path ()
  "The token file: JSON, owner-readable only."
  (or *auth-file* (nlk:home "spotify/auth.json")))

(defun pending-path ()
  "The half-finished sign-in beside the token file."
  (merge-pathnames "pending.json" (auth-path)))

(defun token-store ()
  "The stored tokens, or NIL when there is no sign-in."
  (read-json-file (auth-path)))

(defun token-value (store key)
  "STORE's member KEY as a string, or NIL."
  (and store (nlk:json-value store :string key)))

;;; --- the section's values ----------------------------------------------------

(defun accounts-base ()
  (or (trimmed (setting :accounts-base-url)) +accounts-base+))

(defun api-base ()
  (or (trimmed (setting :api-base-url)) +api-base+))

(defun client-id ()
  (or (trimmed (setting :client-id)) (fail "the spotify section needs a client_id")))

(defun redirect-uri ()
  (or (trimmed (setting :redirect-uri)) +redirect-uri+))

(defun requested-scope ()
  (or (trimmed (setting :scopes)) +scope+))

;;; --- the token endpoints ------------------------------------------------------

(defun token-request (form)
  "POST the form-encoded FORM to /api/token and answer the decoded payload."
  (multiple-value-bind (body status)
      (nlk:http :post (format nil "~a/api/token" (accounts-base))
                :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                :content form
                :timeout 30)
    (cond ((>= status 400)
           (fail "Spotify refused the token request (~a): ~a" status (error-detail body)))
          (t (or (and (stringp body)
                      (handler-case (nlk:decode-json body) (error () nil)))
                 (fail "Spotify's token answer was not JSON"))))))

(defun save-tokens (payload &key previous)
  "Store PAYLOAD's tokens, keeping PREVIOUS's refresh token and scope when the
answer renewed rather than re-granted them. => the stored object."
  (let* ((access (nlk:json-value payload :string "access_token"))
         (refresh (or (nlk:json-value payload :string "refresh_token")
                      (and previous (token-value previous "refresh_token"))))
         (expires-in (or (nlk:json-value payload :integer "expires_in") 3600))
         (scope (or (nlk:json-value payload :string "scope")
                    (and previous (token-value previous "scope"))))
         (object (json-object-of "access_token" access
                                 "refresh_token" refresh
                                 "token_type" (nlk:json-value payload :string "token_type")
                                 "scope" scope
                                 "client_id" (client-id)
                                 "expires_at" (+ (now-seconds) expires-in)
                                 "obtained_at" (now-seconds))))
    (unless access (fail "Spotify's token answer carried no access token"))
    (write-json-file (auth-path) object)
    object))

(defun expiring-p (store)
  "Whether STORE's access token is gone or about to be."
  (let ((at (and store (nlk:json-value store :integer "expires_at"))))
    (and at (<= at (+ (now-seconds) +refresh-skew+)))))

(defun refreshed-store (store)
  "STORE renewed through its refresh token. => the new stored object."
  (let ((refresh (token-value store "refresh_token")))
    (unless refresh
      (fail "Spotify's access token expired and no refresh token was stored: run /spotify login"))
    (save-tokens (token-request (format nil "grant_type=refresh_token&refresh_token=~a&client_id=~a"
                                        (url-encode refresh) (url-encode (client-id))))
                 :previous store)))

(defun credentials (&key force)
  "=> (values ACCESS-TOKEN STORE), refreshed first when the token is about
to expire or FORCE."
  (let* ((store (token-store))
         (access (token-value store "access_token")))
    (unless store (fail "not signed in to Spotify: run /spotify login"))
    (when (or force (null access) (expiring-p store))
      (setf store (refreshed-store store)
            access (token-value store "access_token")))
    (unless access (fail "not signed in to Spotify: run /spotify login"))
    (values access store)))

;;; --- signing in ---------------------------------------------------------------

(defun authorize-url (state verifier)
  "The URL to approve, carrying the challenge for VERIFIER."
  (format nil "~a/authorize?~a" (accounts-base)
          (query-string (list (cons "client_id" (client-id))
                              (cons "response_type" "code")
                              (cons "redirect_uri" (redirect-uri))
                              (cons "scope" (requested-scope))
                              (cons "state" state)
                              (cons "code_challenge_method" "S256")
                              (cons "code_challenge" (pkce-challenge verifier))))))

(defun query-pairs (query)
  "QUERY, a URL's query string, as an alist of (NAME . VALUE)."
  (loop for pair in (uiop:split-string query :separator "&")
        for at = (position #\= pair)
        when at collect (cons (subseq pair 0 at) (subseq pair (1+ at)))))

(defun callback-code (argument)
  "=> (values CODE STATE) out of the URL the browser landed on, or a bare code."
  (let* ((text (nonblank argument "the redirected URL, or the code"))
         (at (position #\? text)))
    (if at
        (let ((pairs (query-pairs (subseq text (1+ at)))))
          (values (cdr (assoc "code" pairs :test #'string=))
                  (cdr (assoc "state" pairs :test #'string=))))
        (values text nil))))

(defun begin-login ()
  "Ask Spotify for a code: remember the verifier, answer what to open."
  (let* ((verifier (pkce-verifier))
         (state (base64url (nlk:random-bytes 12)))
         (redirect (redirect-uri)))
    (write-json-file (pending-path)
                     (json-object-of "code_verifier" verifier
                                     "state" state
                                     "redirect_uri" redirect
                                     "created_at" (now-seconds)))
    (format nil
            (concatenate 'string
                         "Open this, approve the access, and paste the address the browser "
                         "lands on back into /spotify login:~%~%~a~%~%~"
                         "The app's redirect URI must be ~a exactly:~2%"
                         "  Spotify dashboard: https://developer.spotify.com/dashboard~%"
                         "  (Create app, Web API ticked, redirect URI as above.)~2%"
                         "The page the browser lands on will not load: that is the callback, "
                         "and its address carries the code.~%"
                         "  /spotify login http://127.0.0.1:43827/spotify/callback?code=...&state=...~%"
                         "No local port is opened, so this works over SSH too.")
            (authorize-url state verifier) redirect)))

(defun account-name (access)
  "The signed-in account's display name or id, or NIL when Spotify will not say."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get (format nil "~a/me" (api-base))
                    :headers (list (cons "Authorization" (format nil "Bearer ~a" access)))
                    :timeout 20)
        (when (equal status 200)
          (let ((me (and (stringp body) (nlk:decode-json body))))
            (or (nlk:json-value me :string "display_name")
                (nlk:json-value me :string "id")))))
    (error () nil)))

(defun complete-login (argument)
  "Finish the exchange with the code ARGUMENT carries."
  (multiple-value-bind (code state) (callback-code argument)
    (let* ((pending (read-json-file (pending-path)))
           (verifier (and pending (nlk:json-value pending :string "code_verifier")))
           (expected (and pending (nlk:json-value pending :string "state")))
           (redirect (or (and pending (nlk:json-value pending :string "redirect_uri"))
                         (redirect-uri))))
      (unless code (fail "that address carries no code: paste the whole address the browser landed on"))
      (unless verifier
        (fail "no sign-in is in progress: run /spotify login first, then approve it"))
      (when (and expected state (not (string= state expected)))
        (fail "that callback belongs to another sign-in: run /spotify login again"))
      (let* ((payload (token-request (format nil
                                             (concatenate 'string
                                                          "grant_type=authorization_code&code=~a&redirect_uri=~a"
                                                          "&client_id=~a&code_verifier=~a")
                                             (url-encode code) (url-encode redirect)
                                             (url-encode (client-id)) (url-encode verifier))))
             (store (save-tokens payload)))
        (ignore-errors (delete-file (pending-path)))
        (let ((who (account-name (token-value store "access_token"))))
          (format nil "signed in to Spotify~@[ as ~a~]; ~a"
                  who (scope-line store)))))))

(defun scope-line (store)
  "What STORE may do, in one line."
  (let ((scope (token-value store "scope")))
    (if scope
        (format nil "scope: ~a" scope)
        "scope: unrecorded (the app's own)")))

(defun login (&optional argument)
  "Start a sign-in, or finish one with the address the browser landed on."
  (let ((argument (trimmed argument)))
    (if (and argument (plusp (length argument)))
        (complete-login argument)
        (begin-login))))

;;; --- the state of it ----------------------------------------------------------

(defun status ()
  "One line: signed in or not, and until when."
  (let ((store (token-store)))
    (cond ((null store) "not signed in to Spotify: /spotify login starts one")
          (t (let ((at (nlk:json-value store :integer "expires_at")))
               (format nil "signed in to Spotify (client ~a; app redirect ~a); ~@[access token good for ~d more minutes; ~]~arefresh token ~:[missing~;stored~]"
                       (or (token-value store "client_id") "unknown")
                       (redirect-uri)
                       (and at (plusp (- at (now-seconds)))
                            (ceiling (- at (now-seconds)) 60))
                       (scope-line store)
                       (token-value store "refresh_token")))))))

(defun logout ()
  "Forget the stored tokens. => one line."
  (let ((existed (probe-file (auth-path))))
    (when existed (ignore-errors (delete-file (auth-path))))
    (when (probe-file (pending-path)) (ignore-errors (delete-file (pending-path))))
    (if existed "signed out of Spotify" "no Spotify sign-in was stored")))
