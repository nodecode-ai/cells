;;;; items.lisp --- the six tools that read and change what Spotify holds:
;;;; devices, the queue, the catalogue, playlists, albums and the library.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-spotify)

;;; --- spotify_devices ---------------------------------------------------------

(defun spotify-devices (args)
  "spotify_devices: the Spotify Connect devices, or playback moved to one."
  (with-tool-answer
    (let ((action (string-downcase (or (arg args "action") "list"))))
      (cond ((string= action "list")
             (answer action (api-request :get "/me/player/devices")))
            ((string= action "transfer")
             (let ((id (nonblank (arg args "device_id") "device_id for action='transfer'")))
               (answer action (api-request :put "/me/player"
                                            :body (nlk:json-object
                                                   "device_ids" (vector id)
                                                   "play" (arg-bool args "play"))))))
            (t (fail "Unknown spotify_devices action: ~a" action))))))

;;; --- spotify_queue -----------------------------------------------------------

(defun spotify-queue (args)
  "spotify_queue: what is queued next, or one more item behind it."
  (with-tool-answer
    (let ((action (string-downcase (or (arg args "action") "get"))))
      (cond ((string= action "get")
             (answer action (api-request :get "/me/player/queue")))
            ((string= action "add")
             (let ((uri (spotify-uri (arg args "uri"))))
               (answer action (api-request :post "/me/player/queue"
                                            :params (list (cons "uri" uri)
                                                          (cons "device_id" (arg args "device_id"))))
                       :extra (list "uri" uri))))
            (t (fail "Unknown spotify_queue action: ~a" action))))))

;;; --- spotify_search ----------------------------------------------------------

(defparameter +search-types+
  '("album" "artist" "playlist" "track" "show" "episode" "audiobook")
  "What Spotify's search can be asked for.")

(defun spotify-search (args)
  "spotify_search: the catalogue, by query."
  (with-tool-answer
    (let ((query (trimmed (arg args "query"))))
      (unless (and query (plusp (length query))) (fail "query is required"))
      (let ((types (remove-if-not (lambda (type) (member type +search-types+ :test #'equal))
                                   (mapcar #'string-downcase
                                           (or (arg-list args "types" "type") (list "track"))))))
        (unless types
          (fail "types must contain one or more of: ~{~a~^, ~}" +search-types+))
        (answer "search"
                (api-request :get "/search"
                              :params (list (cons "q" query)
                                            (cons "type" (format nil "~{~a~^,~}" types))
                                            (cons "limit" (arg-limit args 10))
                                            (cons "offset" (arg-offset args))
                                            (cons "market" (arg args "market"))
                                            (cons "include_external" (arg args "include_external")))))))))

;;; --- spotify_playlists -------------------------------------------------------

(defun playlist-path (args &optional suffix)
  (format nil "/playlists/~a~a"
          (url-encode (spotify-id (arg args "playlist_id") "playlist"))
          (or suffix "")))

(defun spotify-playlists (args)
  "spotify_playlists: list, read, page, create, edit, add to, remove from."
  (with-tool-answer
    (let ((action (string-downcase (or (arg args "action") "list"))))
      (cond
        ((string= action "list")
         (answer action (api-request :get "/me/playlists"
                                      :params (list (cons "limit" (arg-limit args))
                                                    (cons "offset" (arg-offset args))))))
        ((string= action "get")
         (answer action (api-request :get (playlist-path args)
                                      :params (list (cons "market" (arg args "market"))))))
        ;; get embeds only the first 100 items and pages no further; items does.
        ((string= action "items")
         (answer action (api-request :get (playlist-path args "/items") :params (page-params args))))
        ((string= action "create")
         (answer action (api-request :post "/me/playlists"
                                      :body (nlk:json-object
                                             "name" (nonblank (arg args "name") "name for action='create'")
                                             "public" (arg-bool args "public")
                                             "collaborative" (arg-bool args "collaborative")
                                             "description" (arg args "description")))))
        ((string= action "add_items")
         (answer action (api-request :post (playlist-path args "/items")
                                      :body (json-object-of
                                             "uris" (coerce (spotify-uris (arg-list args "uris")) 'vector)
                                             "position" (arg-int args "position")))))
        ((string= action "remove_items")
         (answer action (api-request :delete (playlist-path args "/items")
                                      :body (json-object-of
                                             "items" (coerce (mapcar (lambda (uri)
                                                                        (nlk:json-object "uri" uri))
                                                                      (spotify-uris (arg-list args "uris")))
                                                              'vector)
                                             "snapshot_id" (arg args "snapshot_id")))))
        ((string= action "update_details")
         (answer action (api-request :put (playlist-path args)
                                      :body (json-object-of
                                             "name" (arg args "name")
                                             "public" (arg-bool args "public")
                                             "collaborative" (arg-bool args "collaborative")
                                             "description" (arg args "description")))))
        (t (fail "Unknown spotify_playlists action: ~a" action))))))

;;; --- spotify_albums ----------------------------------------------------------

(defun spotify-albums (args)
  "spotify_albums: one album's metadata or its track list."
  (with-tool-answer
    (let ((action (string-downcase (or (arg args "action") "get")))
          (path (format nil "/albums/~a"
                        (url-encode (spotify-id (or (arg args "album_id") (arg args "id"))
                                                 "album")))))
      (cond ((string= action "get")
             (answer action (api-request :get path :params (list (cons "market" (arg args "market"))))))
            ((string= action "tracks")
             (answer action (api-request :get (concatenate 'string path "/tracks")
                                          :params (page-params args))))
            (t (fail "Unknown spotify_albums action: ~a" action))))))

;;; --- spotify_library ---------------------------------------------------------

(defun spotify-library (args)
  "spotify_library: saved tracks or saved albums, picked by kind."
  (with-tool-answer
    (let* ((kind (string-downcase (or (trimmed (arg args "kind")) "")))
           (item-type (cond ((string= kind "tracks") "track")
                            ((string= kind "albums") "album")
                            (t (fail "kind must be one of: tracks, albums"))))
           (action (string-downcase (or (arg args "action") "list"))))
      (cond ((string= action "list")
             (answer action (api-request :get (format nil "/me/~a" kind)
                                          :params (page-params args))))
            ((string= action "save")
             (answer action (api-request :put "/me/library"
                                          :params (list
                                                   (cons "uris"
                                                         (format nil "~{~a~^,~}"
                                                                 (spotify-uris (arg-list args "uris" "items")
                                                                                item-type)))))))
            ((string= action "remove")
             (let ((items (arg-list args "ids" "items")))
               (unless items (fail "ids/items is required for action='remove'"))
               (answer action (api-request :delete "/me/library"
                                            :params (list
                                                     (cons "uris"
                                                           (format nil "~{~a~^,~}"
                                                                   (spotify-uris items item-type))))))))
            (t (fail "Unknown spotify_library action: ~a" action))))))
