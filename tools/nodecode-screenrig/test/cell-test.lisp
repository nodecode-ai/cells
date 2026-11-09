;;;; cell-test.lisp --- the cell against a stubbed screenRIG control API.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches screenRIG and no
;;;; token is needed. What is proved is the cell's own wiring: the section (a
;;;; healthy start, and a missing token refused), the five tools through the
;;;; dispatch the model calls them by -- the path, the bearer token and the
;;;; body each sends, and the text each answers -- and the /screenrig command.

(in-package #:nodecode.test)

(define-test-slice "screenrig" "SCREENRIG-CELL-" :start nodecode-screenrig:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defparameter *requests* nil
  "Every request the stub answered, newest first, as (METHOD URL HEADERS CONTENT).")

(defparameter *routes* nil
  "Alist of (PATH-PREFIX . (TEXT STATUS)): the answer to the first path that starts with it.")

(defparameter +screens+
  "{\"items\":[{\"id\":\"scr-1\",\"label\":\"Kitchen TV\",\"state\":\"active\",\"online\":true,\"timezone\":\"Europe/Oslo\",\"effective_playlist\":{\"name\":\"Breakfast\"}},{\"id\":\"scr-2\",\"label\":\"Lobby TV\",\"state\":\"active\",\"online\":false,\"effective_playlist\":{\"name\":\"Welcome\"}}],\"next_cursor\":null}"
  "Two screens, as GET /api/screens answers them.")

(defparameter +screen-one+
  "{\"id\":\"scr-1\",\"label\":\"Kitchen TV\",\"state\":\"active\",\"online\":true,\"timezone\":\"Europe/Oslo\",\"folder\":{\"name\":\"Cafe\"},\"effective_playlist\":{\"name\":\"Breakfast\"}}"
  "One screen, as GET /api/screens/scr-1 answers it.")

(defparameter +playlists+
  "{\"items\":[{\"id\":\"pl-1\",\"name\":\"Breakfast\",\"revision\":3,\"updated_at\":\"2026-11-01T09:00:00Z\"},{\"id\":\"pl-2\",\"name\":\"Welcome\",\"revision\":1,\"updated_at\":\"2026-10-30T18:12:00Z\"}],\"next_cursor\":null}"
  "Two playlists, as GET /api/playlists answers them.")

(defparameter +media+
  "{\"items\":[{\"id\":\"med-1\",\"name\":\"salmon.jpg\",\"kind\":\"image\",\"bytes\":20481},{\"id\":\"med-2\",\"name\":\"welcome.mp4\",\"kind\":\"video\",\"bytes\":913408}],\"next_cursor\":null}"
  "One image and one video, as GET /api/media answers them.")

(defvar *playlist-pages* 0
  "How many times the paged playlist list was asked, for the cursor test.")

(defun cursor-pages (url)
  "Two pages of playlists: the first offers a cursor, the second ends the list."
  (declare (ignore url))
  (if (= (incf *playlist-pages*) 1)
      (values "{\"items\":[{\"id\":\"pl-2\",\"name\":\"Welcome\",\"revision\":1}],\"next_cursor\":\"c2\"}"
              200 nil nil)
      (values "{\"items\":[{\"id\":\"pl-1\",\"name\":\"Breakfast\",\"revision\":3}],\"next_cursor\":null}"
              200 nil nil)))

(defparameter +assigned+
  "{\"id\":\"scr-1\",\"label\":\"Kitchen TV\",\"state\":\"active\",\"playlist_id\":\"pl-1\"}"
  "A screen after a PATCH assigned pl-1 to it.")

(defun path-of (url)
  "URL's path, without the host or the query."
  (let* ((start (or (search "//" url) -1))
         (rest (subseq url (min (length url) (+ start 2))))
         (slash (position #\/ rest))
         (path (if slash (subseq rest slash) "/"))
         (query (position #\? path)))
    (if query (subseq path 0 query) path)))

(defun stub-api (method url &rest options)
  "NLK:HTTP over *ROUTES*: the answer of the first path URL starts with."
  (push (list method url (getf options :headers) (getf options :content)) *requests*)
  (let ((route (find-if (lambda (route) (uiop:string-prefix-p (first route) (path-of url)))
                        *routes*)))
    (cond ((null route)
           (values "{\"code\":\"not_found\",\"detail\":\"no such route\"}" 404 nil nil))
          ((functionp (cdr route)) (funcall (cdr route) url))
          (t (values (second route) (third route) nil nil)))))

(defmacro with-screenrig-wire (&body body)
  "BODY with NLK:HTTP stubbed and the routes fresh: nothing leaves the machine."
  `(let ((*requests* '())
         (*playlist-pages* 0)
         (*routes* (list (cons "/api/screens/scr-1" (list +screen-one+ 200))
                         (cons "/api/screens" (list +screens+ 200))
                         (cons "/api/playlists" (list +playlists+ 200))
                         (cons "/api/media" (list +media+ 200)))))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (apply #'stub-api method url options))
       ,@body)))

(defun screenrig-called (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "screenrig-call" name arguments)))

(defun screenrig-request (path)
  "The newest request whose path starts with PATH, or NIL."
  (find-if (lambda (request) (uiop:string-prefix-p path (path-of (second request))))
           *requests*))

(defun screenrig-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "screenrig"
  (:fixture with-screenrig-wire)
  (:config "token" "tok-1")
  (:help :screenrig)
  (:command "screenrig")
  (:running
   (is (nle::find-tool "screenrig_screens") "the screens tool is registered")
   (is (nle::find-tool "screenrig_screen") "and the one-screen tool")
   (is (nle::find-tool "screenrig_playlists") "and the playlists")
   (is (nle::find-tool "screenrig_media") "and the media library")
   (is (nle::find-tool "screenrig_assign") "and the one write"))
  (:stopped
   (is (null (nle::find-tool "screenrig_screens")) "stop takes every tool back"))
  (:refused
   ("token" ""))
  (:idle nodecode-screenrig:screenrig-error
         (nodecode-screenrig:screens nil)))

;;; --- the reads -------------------------------------------------------------------

(deftest screenrig-cell-screens-read-through-the-api (with-screenrig-wire)
  (with-cell-stop ((screenrig-start "token" "tok-1"))
    (let ((answer (screenrig-called "screenrig_screens" (nlk:make-json-object)))
          (request (screenrig-request "/api/screens")))
      (is (search "Kitchen TV" answer) "the label is in the answer")
      (is (search "Breakfast" answer) "and what the screen plays")
      (is (equal "Bearer tok-1" (screenrig-header request "authorization")) "the section's token is the bearer sent")
      (is (equal "/api/screens" (path-of (second request))) "a plain list read"))))

(deftest screenrig-cell-screens-matching-narrows-locally (with-screenrig-wire)
  (with-cell-stop ((screenrig-start "token" "tok-1"))
    (let ((answer (screenrig-called "screenrig_screens"
                                    (nlk:json-object "matching" "lobby"))))
      (is (search "Lobby" answer) "the matching screen is listed")
      (is (null (search "Kitchen" answer)) "and the other is not")
      (is (search "offline" answer) answer))))

(deftest screenrig-cell-one-screen-resolves-a-label-then-reads-it (with-screenrig-wire)
  (with-cell-stop ((screenrig-start "token" "tok-1"))
    (let ((answer (screenrig-called "screenrig_screen" (nlk:json-object "screen" "kitchen"))))
      (is (search "Kitchen TV" answer) "the tool lists it")
      (is (search "Europe/Oslo" answer) "with the timezone the detail carries")
      (is (search "/api/screens/scr-1" (second (car *requests*))) "the label was resolved to the id first"))))

(deftest screenrig-cell-media-asks-for-the-kind-asked-with-screenrig-wire (with-screenrig-wire)
  (with-cell-stop ((screenrig-start "token" "tok-1"))
    (let ((answer (screenrig-called "screenrig_media" (nlk:json-object "kind" "video"))))
      (is (search "welcome.mp4" answer) "the video is listed")
      (is (search "primitive=video" (second (screenrig-request "/api/media"))) "and only videos were asked for"))))

(deftest screenrig-cell-lists-follow-the-next-cursor (with-screenrig-wire)
  (setf *routes* (acons "/api/playlists" #'cursor-pages *routes*))
  (with-cell-stop ((screenrig-start "token" "tok-1"))
    (let ((answer (screenrig-called "screenrig_playlists" (nlk:make-json-object))))
      (is (search "Breakfast" answer) "the second page's playlist is in the answer")
      (is (search "Welcome" answer) "beside the first page's")
      (is (search "after=c2" (second (first *requests*))) "and the cursor was followed"))))

(deftest screenrig-cell-a-refused-token-is-an-error-the-model-reads (with-screenrig-wire)
  (setf *routes* (acons "/api/screens"
                        (list "{\"code\":\"forbidden\",\"detail\":\"This agent credential lacks the screens capability.\"}" 403)
                        (remove "/api/screens" *routes* :key #'first :test #'equal)))
  (with-cell-stop ((screenrig-start "token" "tok-1"))
    (let ((answer (screenrig-called "screenrig_screens" (nlk:make-json-object))))
      (is (search "ERROR" answer) "a failure, not a table")
      (is (search "lacks the screens capability" answer) "carrying screenRIG's own detail")
      (is (search "dashboard" answer) "and where the token comes from"))))

;;; --- the write -------------------------------------------------------------------

(deftest screenrig-cell-assign-puts-the-playlist-on-the-screen (with-screenrig-wire)
  (setf *routes* (acons "/api/screens/scr-1" (list +assigned+ 200) *routes*))
  (with-cell-stop ((screenrig-start "token" "tok-1"))
    (let ((answer (screenrig-called "screenrig_assign"
                                    (nlk:json-object "screen" "Kitchen TV" "playlist" "Breakfast")))
          (request (screenrig-request "/api/screens/scr-1")))
      (is (equal :patch (first request)) "the write is a PATCH")
      (is (equal "{\"playlist_id\":\"pl-1\"}" (fourth request)) "of the playlist's id alone")
      (is (search "assigned Breakfast to Kitchen TV" answer) "and the answer names both"))))

(deftest screenrig-cell-assign-reports-a-pending-review (with-screenrig-wire)
  (setf *routes* (acons "/api/screens/scr-1"
                        (list "{\"status\":\"pending_review\",\"review\":{\"id\":\"rev-9\"}}" 202)
                        *routes*))
  (with-cell-stop ((screenrig-start "token" "tok-1"))
    (let ((answer (screenrig-called "screenrig_assign"
                                    (nlk:json-object "screen" "scr-1" "playlist" "pl-1"))))
      (is (search "pending review" answer) "it says the write waits")
      (is (search "rev-9" answer) "and names the review"))))

(deftest screenrig-cell-assign-refuses-a-name-naming-two-with-screenrig-wire (with-screenrig-wire)
  (with-cell-stop ((screenrig-start "token" "tok-1"))
    (let ((answer (screenrig-called "screenrig_assign"
                                    (nlk:json-object "screen" "tv" "playlist" "Welcome"))))
      (is (search "matches 2 screens" answer) "both matches are named")
      (is (null (screenrig-request "/api/screens/scr-1")) "and nothing was written"))))

;;; --- the command -----------------------------------------------------------------

(deftest screenrig-cell-command-lists-screens (with-screenrig-wire)
  (with-cell-stop ((screenrig-start "token" "tok-1"))
    (let ((answer (nodecode-screenrig::run-command "" nil)))
      (is (search "Kitchen TV" answer) "the slash command lists the screens"))))
