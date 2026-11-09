;;;; nodecode-spotify.asd --- Spotify's Web API as seven tools, and /spotify
;;;; for the sign-in. A folder of nodecode-ai/cells.
;;;;
;;;; SPDX-License-Identifier: MIT

(defsystem "nodecode-spotify"
  :description "Spotify: playback, devices, the queue, search, playlists, albums and the library, through the user's own Spotify app"
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "auth")
               (:file "tools")
               (:file "items")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-spotify/test"))))

(defsystem "nodecode-spotify/test"
  :description "Spotify cell tests: the lifecycle, every tool over a stubbed wire, the sign-in."
  :license "MIT"
  :depends-on ("nodecode-spotify" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:nodecode.test '#:run-spotify-tests)))
