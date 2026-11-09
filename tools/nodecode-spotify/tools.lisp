;;;; tools.lisp --- the Spotify Web API and the seven tools over it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One call, API-REQUEST, owns the bearer token (refreshed on expiry and
;;;; once on a 401), Spotify's error documents and the 204 that means done.
;;;; Every tool is a dispatch over `action' on top of it, answering the JSON of
;;;; what Spotify said -- or a failure text beginning "ERROR: ", which is what
;;;; the engine records as a failed call.
;;;;
;;;; Spotify accepts an id, a spotify:<type>:<id> URI or an open.spotify.com
;;;; link wherever this cell takes an item; SPOTIFY-URI normalizes all three,
;;;; and refuses a URI of the wrong type rather than sending it.

(in-package #:nodecode-spotify)

;;; --- the one call -------------------------------------------------------------

(defun api-request (method path &key params body (retry t))
  "One Spotify Web API call. => (values DECODED STATUS), DECODED :EMPTY for a
204 or an empty body. A 401 forces one token refresh and retries once."
  (multiple-value-bind (token) (credentials)
    (multiple-value-bind (text status)
        (nlk:http method (with-query (concatenate 'string (api-base) path) params)
                  :headers (list (cons "Authorization" (format nil "Bearer ~a" token))
                                 (cons "Content-Type" "application/json"))
                  :content (and body (nlk:encode-json-object body))
                  :timeout 30)
      (cond ((and (= status 401) retry)
             (credentials :force t)
             (api-request method path :params params :body body :retry nil))
            ((>= status 400)
             (fail "~a" (friendly-error status (error-detail text) path)))
            ((or (= status 204) (and (stringp text) (zerop (length text))))
             (values :empty status))
            (t (values (if (and (stringp text) (plusp (length text)))
                           (handler-case (nlk:decode-json text) (error () text))
                           text)
                       status))))))

(defun answer (action result &key (status 200) extra)
  "The success text: success, action, EXTRA's members, then result -- or the
empty marker a 204 answers with."
  (nlk:encode-json-object
   (apply #'nlk:make-json-object
          (append (list "success" t "action" action) extra
                  (if (eq result :empty)
                      (list "status_code" status "empty" t)
                      (list "result" result))))))

(defmacro with-tool-answer (&body body)
  "BODY's string, or its refusal as the failure text the engine records."
  `(handler-case (progn ,@body)
     (spotify-error (condition) (format nil "ERROR: ~a" condition))
     (error (condition) (format nil "ERROR: Spotify tool failed: ~a" condition))))

;;; --- what a call's arguments say -----------------------------------------------

(defun arg (args key)
  (nlk:json-value args :string key))

(defun arg-int (args key)
  (nlk:json-value args :integer key))

(defun arg-object (args key)
  "ARGS' member KEY when it is an object -- the player's offset."
  (let ((value (gethash key args)))
    (and (hash-table-p value) value)))

(defun arg-list (args &rest keys)
  "The first of KEYS ARGS carries as a string or an array of strings, as a
list of non-blank strings."
  (loop for key in keys
        for raw = (gethash key args)
        when raw
          return (remove ""
                         (if (vectorp raw)
                             (loop for item across raw when (stringp item) collect (trimmed item))
                             (list (trimmed raw)))
                         :test #'equal)))

(defun arg-bool (args key &optional default)
  "ARGS' member KEY as a boolean; the strings true/false, yes/no, on/off, 1/0 count."
  (let ((raw (gethash key args)))
    (cond ((eq raw t) t)
          ((null raw) default)
          ((stringp raw)
           (let ((text (string-downcase (trimmed raw))))
             (cond ((member text '("1" "true" "yes" "on") :test #'equal) t)
                   ((member text '("0" "false" "no" "off") :test #'equal) nil)
                   (t default))))
          (t default))))

(defun arg-limit (args &optional (default 20))
  "LIMIT clamped to Spotify's 1..50 window."
  (min 50 (max 1 (or (arg-int args "limit") default))))

(defun arg-offset (args)
  (max 0 (or (arg-int args "offset") 0)))

(defun page-params (args)
  "The paging params every list endpoint takes."
  (list (cons "limit" (arg-limit args)) (cons "offset" (arg-offset args))
        (cons "market" (arg args "market"))))

;;; --- ids, URIs and links -------------------------------------------------------

(defun url-path (text)
  "TEXT's path when it is a URL."
  (let* ((scheme (search "://" text))
         (start (and scheme (+ scheme 3)))
         (slash (and start (position #\/ text :start start))))
    (let ((path (cond ((and slash (subseq text slash)))
                      (start "")
                      (t text))))
      ;; A link may carry a share query (?si=...) the item type does not.
      (subseq path 0 (or (position-if (lambda (c) (member c '(#\? #\#))) path)
                         (length path))))))

(defun check-item-type (type expected)
  (when (and expected (not (string-equal type expected)))
    (fail "Expected a Spotify ~a, got ~a." expected type)))

(defun spotify-id (value &optional expected-type)
  "The bare id of VALUE: an id, a spotify:<type>:<id> URI, or a link."
  (let* ((text (nonblank (and (stringp value) value) "a Spotify id, URI or link"))
         (parts (if (uiop:string-prefix-p "spotify:" text)
                    (uiop:split-string (subseq text 8) :separator ":")
                    '())))
    (when (and (< (length parts) 2) (search "open.spotify.com" text))
      (setf parts (remove "" (uiop:split-string (url-path text) :separator "/")
                          :test #'equal))
      ;; A localized web player keeps /intl-de/ in front of the type.
      (when (and parts (uiop:string-prefix-p "intl-" (first parts))) (pop parts)))
    (if (<= 2 (length parts))
        (progn (check-item-type (first parts) expected-type) (second parts))
        text)))

(defun spotify-uri (value &optional expected-type)
  "Like SPOTIFY-ID but answering a URI; a bare id needs EXPECTED-TYPE."
  (let ((text (nonblank (and (stringp value) value) "a Spotify URI, id or link")))
    (cond ((uiop:string-prefix-p "spotify:" text)
           (let ((parts (uiop:split-string (subseq text 8) :separator ":")))
             (when (<= 2 (length parts)) (check-item-type (first parts) expected-type))
             text))
          (expected-type (format nil "spotify:~a:~a" expected-type (spotify-id text expected-type)))
          (t text))))

(defun spotify-uris (values &optional expected-type)
  "Every value as a URI, first occurrence kept, at least one required."
  (let ((uris '()))
    (dolist (value values)
      (let ((uri (spotify-uri value expected-type)))
        (unless (member uri uris :test #'equal) (setf uris (append uris (list uri))))))
    (or uris (fail "at least one Spotify item is required"))))

(defun infer-context-type (text)
  "The item type a context URI or link names, when the text says."
  (cond ((or (uiop:string-prefix-p "spotify:album:" text) (search "/album/" text)) "album")
        ((or (uiop:string-prefix-p "spotify:playlist:" text) (search "/playlist/" text)) "playlist")
        ((or (uiop:string-prefix-p "spotify:artist:" text) (search "/artist/" text)) "artist")
        (t nil)))

;;; --- spotify_playback ---------------------------------------------------------

(defun empty-playback (action status flag message)
  "The answer a read gives when Spotify has nothing playing."
  (answer action :empty :status status :extra (list flag nil "message" message)))

(defun play-tracks (args uris)
  "Play a bare list of tracks: the first inside its album, the rest queued.

Spotify's desktop client answers a bare {uris: [...]} with 204 and plays
nothing, and drops what was playing; a request carrying a context plays. So
the first track's album is the context, the offset points at the track, and
the album keeps playing after the queued ones run out."
  (let ((device (arg args "device_id"))
        (ids (mapcar (lambda (uri) (spotify-id uri "track")) uris)))
    (multiple-value-bind (track)
        (api-request :get (format nil "/tracks/~a" (url-encode (first ids)))
                      :params '(("market" . "from_token")))
      (let ((album (and (hash-table-p track) (nlk:json-value track :string "album" "uri")))
            (uri (and (hash-table-p track) (nlk:json-value track :string "uri"))))
        (unless album (fail "Spotify did not say which album track ~a is on" (first ids)))
        (let ((result (api-request :put "/me/player/play"
                                    :params (list (cons "device_id" device))
                                    :body (json-object-of
                                           "context_uri" album
                                           "offset" (nlk:json-object "uri" uri)
                                           "position_ms" (arg-int args "position_ms"))))
              (queued (rest uris)))
          (dolist (queued-uri queued)
            (api-request :post "/me/player/queue"
                         :params (list (cons "uri" (spotify-uri queued-uri "track"))
                                       (cons "device_id" device))))
          (answer "play" result :extra (list "started" uri "context_uri" album
                                              "queued" (coerce queued 'vector))))))))

(defun spotify-playback (args)
  "spotify_playback: play, pause, skip, seek, shuffle, repeat, volume, the
current state, or what was played recently."
  (with-tool-answer
    (let ((action (string-downcase (or (arg args "action") "get_state")))
          (device (arg args "device_id")))
      (flet ((command (method path &optional (extra nil))
               (answer action (api-request method path
                                            :params (append extra (list (cons "device_id" device))))))
             (required-int (key)
               (or (arg-int args key) (fail "~a is required for action=~s" key action))))
        (cond
          ((string= action "get_state")
           (multiple-value-bind (result status)
               (api-request :get "/me/player" :params (list (cons "market" (arg args "market"))))
             (if (eq result :empty)
                 (empty-playback action status "has_active_device"
                                  "No active Spotify playback session was found. Open Spotify on a device and start playback, or transfer playback to an available device.")
                 (answer action result))))
          ((string= action "get_currently_playing")
           (multiple-value-bind (result status)
               (api-request :get "/me/player/currently-playing"
                             :params (list (cons "market" (arg args "market"))))
             (if (eq result :empty)
                 (empty-playback action status "is_playing"
                                  "Spotify is not currently playing anything. Start playback in Spotify and try again.")
                 (answer action result))))
          ((string= action "play")
           (let ((uris (arg-list args "uris"))
                 (context (trimmed (arg args "context_uri"))))
             (if (and uris (null context))
                 (play-tracks args uris)
                 (let ((body (json-object-of
                              "context_uri" (and context (spotify-uri context (infer-context-type context)))
                              "uris" (and uris (coerce (spotify-uris uris "track") 'vector))
                              "offset" (arg-object args "offset")
                              "position_ms" (arg-int args "position_ms"))))
                   (answer action (api-request :put "/me/player/play"
                                                :params (list (cons "device_id" device))
                                                :body body))))))
          ((string= action "pause") (command :put "/me/player/pause"))
          ((string= action "next") (command :post "/me/player/next"))
          ((string= action "previous") (command :post "/me/player/previous"))
          ((string= action "seek")
           (command :put "/me/player/seek"
                    (list (cons "position_ms" (required-int "position_ms")))))
          ((string= action "set_repeat")
           (let ((state (string-downcase (or (trimmed (arg args "state")) ""))))
             (unless (member state '("track" "context" "off") :test #'equal)
               (fail "state must be one of: track, context, off"))
             (command :put "/me/player/repeat" (list (cons "state" state)))))
          ((string= action "set_shuffle")
           (command :put "/me/player/shuffle"
                    (list (cons "state" (if (arg-bool args "state") "true" "false")))))
          ((string= action "set_volume")
           (command :put "/me/player/volume"
                    (list (cons "volume_percent"
                                (min 100 (max 0 (required-int "volume_percent")))))))
          ((string= action "recently_played")
           (let ((after (arg-int args "after")) (before (arg-int args "before")))
             (when (and after before) (fail "Provide only one of 'after' or 'before'"))
             (answer action (api-request :get "/me/player/recently-played"
                                          :params (list (cons "limit" (arg-limit args))
                                                        (cons "after" after) (cons "before" before))))))
          (t (fail "Unknown spotify_playback action: ~a" action)))))))
