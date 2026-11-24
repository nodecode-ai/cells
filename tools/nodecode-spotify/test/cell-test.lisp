;;;; cell-test.lisp --- the Spotify cell over the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Spotify endpoint is a stubbed NLK:HTTP, the token file is a file
;;;; under the scratch home, and the clock is bound: nothing reaches Spotify,
;;;; the network or the operator's sign-in.

(in-package #:nodecode.test)

(define-test-slice "spotify" "SPOTIFY-CELL-" :start nodecode-spotify:start-cell)

(define-cell-lifecycle-tests "spotify"
  (:config "client_id" "cid")
  (:help :spotify)
  (:command "spotify")
  (:running (is (equal nodecode-spotify::+primer+ (nle:help :spotify)) "(help :spotify) is the primer"))
  (:refused ("client_id" nil)))

;;; --- fixtures --------------------------------------------------------------------

(defparameter +now+ 1767225600
  "2026-01-01T00:00:00Z: the clock every test runs at.")

(defun spotify-sign-in (&key (access "acc-1") (refresh "ref-1") (expires-at (+ +now+ 3600)))
  "Write the token file the cell reads."
  (nodecode-spotify::write-json-file
   (nodecode-spotify::auth-path)
   (nlk:json-object "access_token" access "refresh_token" refresh
                    "client_id" "cid" "scope" "user-read-private"
                    "expires_at" expires-at)))

(defmacro with-spotify ((&rest config) &body body)
  "BODY with the cell started on CONFIG and the token file under a scratch home."
  `(with-cell-stop ((spotify-start ,@config))
     (let ((nodecode-spotify::*auth-file* (nlk:home "spotify-test/auth.json"))
           (nodecode-spotify::*now* +now+))
       (ignore-errors (delete-file (nodecode-spotify::*auth-file*)))
       (unwind-protect (progn ,@body)
         (ignore-errors (delete-file (nodecode-spotify::*auth-file*)))))))

(defmacro with-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL fragment: ROUTES are (FRAGMENT FORM),
FORM run with METHOD, URL, HEADERS and CONTENT bound and answering (values
TEXT STATUS). ASKED collects (METHOD URL HEADERS CONTENT), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition
           (nlk:http (method url &key headers content (timeout 30) (connect-timeout 10)
                              max-bytes binary stream pool proxy)
             (push (list method url headers content) ,log)
             (cond ,@(loop for (fragment form) in routes
                           collect `((search ,fragment url) ,form))
                   (t (error "unexpected request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defun header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

;;; --- the tools over a stubbed wire -------------------------------------------------

(deftest spotify-cell-search-answers-through-the-api ()
  (with-spotify ("client_id" "cid")
    (spotify-sign-in)
    (with-wire (asked
                ("/search" (values "{\"tracks\":{\"items\":[{\"name\":\"Blue Monday\"}]}}" 200)))
      (let ((answer (nodecode-spotify:spotify-search
                     (nlk:json-object "query" "blue monday" "types" (vector "track") "limit" 5))))
        (is (search "Blue Monday" answer) "the answer carries what Spotify said")
        (is (search "success" answer))
        (is (= 1 (length asked)))
        (let ((url (second (first asked))))
          (is (search "q=blue%20monday" url) "the query goes out escaped")
          (is (search "type=track" url))
          (is (search "limit=5" url)))
        (is (equal "Bearer acc-1" (header (third (first asked)) "Authorization")) "the stored access token is the bearer")))
    (with-wire (asked
                ("/search" (values "{}" 200)))
      (let ((answer (nodecode-spotify:spotify-search (nlk:json-object "query" "x"))))
        (is (search "type=track" (second (first asked))) "tracks are the default type")
        (is (search "limit=10" (second (first asked))) "ten results unless asked otherwise")
        (is (search "success" answer))))))

(deftest spotify-cell-playback-reads-explain-an-empty-player ()
  (with-spotify ("client_id" "cid")
    (spotify-sign-in)
    (with-wire (asked ("/me/player" (values "" 204)))
      (let ((answer (nodecode-spotify:spotify-playback (nlk:json-object "action" "get_state"))))
        (is (search "has_active_device\":false" answer) "nothing playing is a fact, not an error")
        (is (search "No active Spotify playback session" answer))
        (is (null (search "ERROR:" answer)))))))

(deftest spotify-cell-refreshes-an-expiring-token-before-the-call ()
  (with-spotify ("client_id" "cid")
    (spotify-sign-in :expires-at (+ +now+ 10))
    (with-wire (asked
                ("/api/token" (values "{\"access_token\":\"acc-2\",\"expires_in\":3600}" 200))
                ("/me/player/devices" (values "{\"devices\":[]}" 200)))
      (let ((answer (nodecode-spotify:spotify-devices (nlk:json-object "action" "list"))))
        (is (search "devices" answer))
        (is (= 2 (length asked)) "the refresh, then the call")
        (is (search "grant_type=refresh_token" (fourth (first asked))))
        (is (search "refresh_token=ref-1" (fourth (first asked))))
        (is (equal "Bearer acc-2" (header (third (second asked)) "Authorization"))))
      (is (equal "acc-2" (nodecode-spotify::token-value (nodecode-spotify::token-store) "access_token")) "the renewed token is kept"))))

(deftest spotify-cell-a-401-refreshes-and-retries-once ()
  (with-spotify ("client_id" "cid")
    (spotify-sign-in)
    (let ((first t))
      (with-wire (asked
                  ("/api/token" (values "{\"access_token\":\"acc-2\",\"expires_in\":3600}" 200))
                  ("/me/player/devices" (if (shiftf first nil)
                                             (values "{\"error\":{\"message\":\"expired\"}}" 401)
                                             (values "{\"devices\":[]}" 200))))
        (let ((answer (nodecode-spotify:spotify-devices (nlk:json-object "action" "list"))))
          (is (search "devices" answer))
          (is (= 3 (length asked)) "the call, the refresh, the retry"))))))

(deftest spotify-cell-a-refusal-answers-the-models-error-line ()
  (with-spotify ("client_id" "cid")
    (spotify-sign-in)
    (with-wire (asked ("/search" (values "{\"error\":{\"message\":\"Invalid limit\"}}" 400)))
      (let ((answer (nodecode-spotify:spotify-search (nlk:json-object "query" "x"))))
        (is (uiop:string-prefix-p "ERROR: " answer) "a failed call is a failed fact")
        (is (search "Invalid limit" answer))))
    (with-wire (asked ("/me/player/seek" (values "" 204)))
      (is (search "position_ms is required"
                  (nodecode-spotify:spotify-playback (nlk:json-object "action" "seek"))) "a missing argument is named")
      (is (null asked) "and nothing is sent"))))

(deftest spotify-cell-plays-a-bare-track-list-inside-its-album ()
  (with-spotify ("client_id" "cid")
    (spotify-sign-in)
    (with-wire (asked
                ("/tracks/a" (values "{\"uri\":\"spotify:track:a\",\"album\":{\"uri\":\"spotify:album:al\"}}" 200))
                ("/me/player/play" (values "" 204))
                ("/me/player/queue" (values "" 204)))
      (let ((answer (nodecode-spotify:spotify-playback
                     (nlk:json-object "action" "play" "uris" (vector "a" "b")))))
        (is (search "spotify:album:al" answer) "the album is the context")
        (is (= 3 (length asked)) "one lookup, one play, one queue")
        (is (search "context_uri" (fourth (second asked))))
        (is (search "uri=spotify%3Atrack%3Ab" (second (third asked))) "the rest queue behind it")))))

(deftest spotify-cell-reads-ids-uris-and-links-alike ()
  (is (equal "5x" (nodecode-spotify::spotify-id "spotify:track:5x" "track")))
  (is (equal "5x" (nodecode-spotify::spotify-id "5x" "track")))
  (is (equal "7z" (nodecode-spotify::spotify-id
                    "https://open.spotify.com/intl-de/track/7z?si=abc" "track")) "a localized link, its share query and all")
  (signals-error nodecode-spotify:spotify-error (nodecode-spotify::spotify-id "spotify:album:9" "track"))
  (is (equal '("spotify:track:a" "spotify:track:b")
             (nodecode-spotify::spotify-uris (list "a" "spotify:track:b" "a") "track")) "duplicates go, order stays"))

;;; --- the sign-in ----------------------------------------------------------------

(deftest spotify-cell-signs-in-with-a-pasted-callback ()
  (with-spotify ("client_id" "cid")
    (let ((started (cell-entry "nodecode-spotify" "spotify" "login")))
      (is (search "code_challenge_method=S256" started) "PKCE, no client secret")
      (is (search "client_id=cid" started))
      (is (search "redirect_uri=http%3A%2F%2F127.0.0.1%3A43827" started) "the loopback redirect the app registers"))
    (with-wire (asked
                ("/api/token" (values "{\"access_token\":\"acc-9\",\"refresh_token\":\"ref-9\",\"expires_in\":3600,\"token_type\":\"Bearer\",\"scope\":\"user-read-private\"}" 200))
                ("/me" (values "{\"display_name\":\"someone\"}" 200)))
      (let ((answer (cell-entry "nodecode-spotify" "spotify"
                                "login http://127.0.0.1:43827/spotify/callback?code=abc")))
        (is (search "signed in to Spotify as someone" answer))
        (is (search "code_verifier=" (fourth (first asked))) "the verifier finishes the exchange")
        (is (search "grant_type=authorization_code" (fourth (first asked))))))
    (is (search "signed in to Spotify" (cell-entry "nodecode-spotify" "spotify" "status")))
    (is (search "signed out" (cell-entry "nodecode-spotify" "spotify" "logout")))
    (is (search "not signed in" (cell-entry "nodecode-spotify" "spotify" "status")))))

(deftest spotify-cell-says-when-nobody-is-signed-in ()
  (with-spotify ("client_id" "cid")
    (with-wire (asked)
      (let ((answer (nodecode-spotify:spotify-queue (nlk:json-object "action" "get"))))
        (is (uiop:string-prefix-p "ERROR: " answer))
        (is (search "not signed in to Spotify" answer) "and names the way in")
        (is (null asked) "without asking Spotify anything")))))
