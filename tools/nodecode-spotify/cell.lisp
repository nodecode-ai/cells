;;;; cell.lisp --- the declaration: the section, the seven tools, /spotify, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tools keep the original plugin's names, actions and arguments, so a
;;;; habit formed on Hermes still works here. What differs is the sign-in: no
;;;; loopback listener, no `hermes spotify` command -- /spotify login prints
;;;; the URL, and the address the browser lands on finishes it.

(in-package #:nodecode-spotify)

;;; --- the manual ---------------------------------------------------------------

(defparameter +primer+
  (concatenate
   'string
   "Spotify, through the user's own Spotify app: seven tools over the Web API.
"
   "  spotify_search     query, types (track/album/artist/playlist/show/episode/audiobook)
"
   "  spotify_playback   action=get_state|get_currently_playing|play|pause|next|previous|
"
   "                     seek|set_repeat|set_shuffle|set_volume|recently_played;
"
   "                     play takes uris, context_uri (an album/playlist/artist) and
"
   "                     position_ms; the rest take device_id, state, position_ms or volume_percent
"
   "  spotify_devices    action=list|transfer, device_id, play
"
   "  spotify_queue      action=get|add, uri, device_id
"
   "  spotify_playlists  action=list|get|items|create|add_items|remove_items|update_details,
"
   "                     playlist_id, name, description, public, uris, position, snapshot_id
"
   "  spotify_albums     action=get|tracks, album_id, market, limit, offset
"
   "  spotify_library    kind=tracks|albums, action=list|save|remove, uris or ids, limit, offset
"
   "Every item may be an id, a spotify:<type>:<id> URI or an open.spotify.com link.
"
   "An album or playlist link is enough for play: pass it as context_uri.
"
   "Search, playlists, albums, the library, queue reads and the device list work on a
"
   "Free account; playback control needs Premium and an active Spotify Connect device.
"
   "Signing in, with the operator: /spotify status | login [URL] | logout. The client id
"
   "is the `spotify' section's client_id: the user creates an app at
"
   "https://developer.spotify.com/dashboard with the redirect URI the section names.
"
   "Nothing plays on this machine: the tools steer Spotify wherever it is open.")
  "What (help :spotify) answers while the cell runs.")

;;; --- the schemas the model reads ------------------------------------------------

(defun text-of (description)
  (nlk:json-object "type" "string" "description" description))

(defun int-of (description)
  (nlk:json-object "type" "integer" "description" description))

(defun bool-of (description)
  (nlk:json-object "type" "boolean" "description" description))

(defun enum-of (&rest values)
  (nlk:json-object "type" "string" "enum" (coerce values 'vector)))

(defun array-of (description)
  (nlk:json-object "type" "array" "items" (nlk:json-object "type" "string")
                   "description" description))

(defparameter +playback-schema+
  (list :description "Control Spotify playback, inspect the active playback state, or fetch recently played tracks."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties"
         (nlk:json-object
          "action" (enum-of "get_state" "get_currently_playing" "play" "pause" "next" "previous"
                             "seek" "set_repeat" "set_shuffle" "set_volume" "recently_played")
          "device_id" (text-of "the Spotify Connect device to act on; the active one when left out")
          "market" (text-of "an ISO 3166-1 alpha-2 country code, for the catalogue reads")
          "context_uri" (text-of "an album, playlist or artist to play: a URI or an open.spotify.com link")
          "uris" (array-of "tracks to play, in order")
          "offset" (nlk:json-object "type" "object"
                                    "description" "where inside context_uri to start: {position} or {uri}")
          "position_ms" (int-of "where inside the item to start, in milliseconds")
          "state" (nlk:json-object "type" "string"
                                   "description" "set_repeat: track, context or off. set_shuffle: true or false")
          "volume_percent" (int-of "set_volume: 0 to 100")
          "limit" (int-of "recently_played: how many (max 50)")
          "after" (int-of "recently_played: only plays after this Unix ms cursor")
          "before" (int-of "recently_played: only plays before this Unix ms cursor"))
         "required" (vector "action"))))

(defparameter +devices-schema+
  (list :description "List Spotify Connect devices or transfer playback to a different device."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object
                       "action" (enum-of "list" "transfer")
                       "device_id" (text-of "transfer: the device to move playback to")
                       "play" (bool-of "transfer: start playing on the new device"))
         "required" (vector "action"))))

(defparameter +queue-schema+
  (list :description "Inspect the user's Spotify queue or add an item to it."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object
                       "action" (enum-of "get" "add")
                       "uri" (text-of "add: the track or episode to queue")
                       "device_id" (text-of "add: the device to queue on; the active one when left out"))
         "required" (vector "action"))))

(defparameter +search-schema+
  (list :description "Search the Spotify catalog for tracks, albums, artists, playlists, shows or episodes."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object
                       "query" (text-of "what to search for")
                       "types" (array-of "what to look for: track, album, artist, playlist, show, episode, audiobook")
                       "type" (text-of "one type, when a single one is meant")
                       "limit" (int-of "how many of each type (max 50, default 10)")
                       "offset" (int-of "index of the first result")
                       "market" (text-of "an ISO 3166-1 alpha-2 country code")
                       "include_external" (text-of "audio, to include externally hosted audio"))
         "required" (vector "query"))))

(defparameter +playlists-schema+
  (list :description "List, inspect, create, update and modify Spotify playlists."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object
                       "action" (enum-of "list" "get" "items" "create" "add_items"
                                          "remove_items" "update_details")
                       "playlist_id" (text-of "the playlist: an id, a URI or a link")
                       "market" (text-of "an ISO 3166-1 alpha-2 country code")
                       "limit" (int-of "list and items: page size (max 50); get returns the first 100 items")
                       "offset" (int-of "list and items: index of the first entry; use items past 100")
                       "name" (text-of "create and update_details: the playlist's name")
                       "description" (text-of "create and update_details: its description")
                       "public" (bool-of "create and update_details: public or private")
                       "collaborative" (bool-of "create and update_details: open to collaborators")
                       "uris" (array-of "add_items and remove_items: the tracks, in order")
                       "position" (int-of "add_items: where in the playlist they go")
                       "snapshot_id" (text-of "remove_items: the version the removal was planned against"))
         "required" (vector "action"))))

(defparameter +albums-schema+
  (list :description "Fetch Spotify album metadata or an album's tracks."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object
                       "action" (enum-of "get" "tracks")
                       "album_id" (text-of "the album: an id, a URI or a link")
                       "id" (text-of "album_id, under the other name")
                       "market" (text-of "an ISO 3166-1 alpha-2 country code")
                       "limit" (int-of "tracks: page size (max 50)")
                       "offset" (int-of "tracks: index of the first track"))
         "required" (vector "action"))))

(defparameter +library-schema+
  (list :description "List, save or remove the user's saved Spotify tracks or albums. `kind` picks which."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object
                       "kind" (nlk:json-object "type" "string" "enum" (vector "tracks" "albums")
                                                "description" "which library to work on")
                       "action" (enum-of "list" "save" "remove")
                       "limit" (int-of "list: page size (max 50)")
                       "offset" (int-of "list: index of the first entry")
                       "market" (text-of "an ISO 3166-1 alpha-2 country code")
                       "uris" (array-of "save: the tracks or albums to save")
                       "ids" (array-of "remove: the tracks or albums to remove")
                       "items" (array-of "save and remove: the items, under the other name"))
         "required" (vector "kind" "action"))))

;;; --- the command ----------------------------------------------------------------

(defun run-command (args session-id)
  "The /spotify command: status, login [URL], logout."
  (declare (ignore session-id))
  (let* ((text (or (trimmed args) ""))
         (at (position #\Space text))
         (verb (string-downcase (if at (subseq text 0 at) text)))
         (rest (if at (trimmed (subseq text (1+ at))) "")))
    (handler-case
        (cond ((or (string= verb "") (string= verb "status")) (status))
              ((string= verb "login") (login rest))
              ((string= verb "logout") (logout))
              ((string= verb "help") +primer+)
              (t (format nil "~a is no subcommand: /spotify status | login [URL] | logout" verb)))
      (spotify-error (condition) (format nil "spotify: ~a" condition)))))

;;; --- the section ----------------------------------------------------------------

(defun read-settings (values table)
  "The section's values, or the refusal that names what is missing. The client
id is what makes the folder usable at all: without it no tool can get a token."
  (declare (ignore table))
  (when (zerop (length (or (getf values :client-id) "")))
    (error 'nlk:config-refusal
           :detail (concatenate
                    'string
                    "the spotify section needs client_id: create an app at "
                    "https://developer.spotify.com/dashboard with the redirect URI the "
                    "section names (http loopback is allowed), paste its Client ID into "
                    "the section, restart the cell, then run /spotify login")))
  values)

;;; --- the declaration ------------------------------------------------------------

(nle:define-cell spotify
  (:section ("spotify")
    (:guide (concatenate
             'string
             "create a Spotify app (Web API ticked, the redirect URI as declared) ; copy its "
             "Client ID here ; restart the cell ; /spotify login ; approve in the browser ; "
             "/spotify login <the address it landed on>"))
    ("client_id" :string :required t
     :doc "[set] your Spotify app's Client ID (developer.spotify.com/dashboard)")
    ("redirect_uri" :string :default +redirect-uri+
     :doc "the redirect URI that app registers: a loopback address, http allowed")
    ("scopes" :string :default +scope+
     :doc "what the sign-in asks for; the ten the tools need unless you know better")
    ("api_base_url" :string :default +api-base+
     :doc "where the Web API is; a market or local proxy changes it")
    ("accounts_base_url" :string :default +accounts-base+
     :doc "where the sign-in endpoints are"))
  (:settings #'read-settings)
  (:tool "spotify_playback" +playback-schema+ #'spotify-playback)
  (:tool "spotify_devices" +devices-schema+ #'spotify-devices)
  (:tool "spotify_queue" +queue-schema+ #'spotify-queue)
  (:tool "spotify_search" +search-schema+ #'spotify-search)
  (:tool "spotify_playlists" +playlists-schema+ #'spotify-playlists)
  (:tool "spotify_albums" +albums-schema+ #'spotify-albums)
  (:tool "spotify_library" +library-schema+ #'spotify-library)
  (:command "spotify" #'run-command
            :description "Spotify: the sign-in's state, a sign-in, or a sign-out"
            :argument-hint "status | login [URL] | logout")
  (:help :spotify
         "Spotify's Web API through the user's own app: play, queue, search, playlists, albums, the library"
         +primer+))
