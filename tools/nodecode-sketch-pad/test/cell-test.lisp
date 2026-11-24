;;;; cell-test.lisp --- the pad's page, the drawing it saves, and every refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials out: the pad is served on 127.0.0.1 by this cell, and
;;;; the one HTTP call the tests make is to that loopback port. What is pinned
;;;; is what a user gets -- a drawing the model can be handed a path to, a page
;;;; that saves, and a newest drawing that does not move -- and what a bad
;;;; payload never costs: a file that is not a PNG, an empty sketch and one
;;;; over the cap are all refused before anything is written.

(in-package #:nodecode.test)

(define-test-slice "sketch-pad" "SKETCH-PAD-" :start nodecode-sketch-pad:start-cell)

;;; --- fixtures -----------------------------------------------------------------

(defun sketch-pad-directory ()
  "A folder of this test's own under the scratch home, so no two tests share one."
  (merge-pathnames (format nil "sketch-pad-~12,'0x/" (random (expt 16 10))) (nlk:home)))

(defmacro with-pad ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS -- an ephemeral pad port and a folder of
this test's own unless PAIRS names them -- and its folder deleted afterwards."
  (let ((started (gensym "STARTED")) (directory (gensym "DIRECTORY")))
    `(let* ((,started (sketch-pad-start
                        "port" 0
                        "directory" (namestring (sketch-pad-directory))
                        ,@pairs))
            (,directory (namestring (nodecode-sketch-pad::sketches-directory))))
       (nlk:with-cleanup ((progn (funcall ,started)
                                 (uiop:delete-directory-tree (pathname ,directory) :validate t
                                                                                :if-does-not-exist :ignore)))
         ,@body))))

(defun sketch-pad-png (&optional (width 720) (height 480) (extra 0))
  "A PNG header: the eight magic bytes and an IHDR naming WIDTH and HEIGHT,
PADDED with EXTRA zero bytes so a test can pass the cap."
  (let ((octets (make-array (+ 25 extra) :element-type '(unsigned-byte 8)
                                       :initial-element 0)))
    (loop for byte across #(137 80 78 71 13 10 26 10)
          for i from 0 do (setf (aref octets i) byte))
    (setf (aref octets 8) 0 (aref octets 9) 0 (aref octets 10) 0 (aref octets 11) 13)
    (loop for (letter . i) in `((#\I . 12) (#\H . 13) (#\D . 14) (#\R . 15))
          do (setf (aref octets i) (char-code letter)))
    (setf (aref octets 16) (ldb (byte 8 24) width) (aref octets 17) (ldb (byte 8 16) width)
          (aref octets 18) (ldb (byte 8 8) width) (aref octets 19) (ldb (byte 8 0) width)
          (aref octets 20) (ldb (byte 8 24) height) (aref octets 21) (ldb (byte 8 16) height)
          (aref octets 22) (ldb (byte 8 8) height) (aref octets 23) (ldb (byte 8 0) height))
    octets))

(defun sketch-pad-png-base64 (&rest arguments)
  (cl-base64:usb8-array-to-base64-string (apply #'sketch-pad-png arguments)))

(defun sketch-pad-tool (name &rest pairs)
  "NAME as the engine calls it: the declared schema decoded, then the body."
  (let* ((entry (nle::find-tool name))
         (schema (second entry))
         (object (apply #'nlk:make-json-object pairs)))
    (is-present entry (format nil "~a is registered" name))
    ;; A contract decodes the object into its record; a hand-written schema
    ;; hands the object through, as the engine does.
    (funcall (third entry) (if (listp schema) object (nlk:contract-decode schema object)))))

(defun sketch-pad-port ()
  "The port the pad answers on."
  (nodecode-sketch-pad::pad-port))

(defun sketch-pad-get (path)
  (nlk:http :get (format nil "http://127.0.0.1:~d~a" (sketch-pad-port) path)
            :timeout 10))

(defun sketch-pad-post (path body)
  (nlk:http :post (format nil "http://127.0.0.1:~d~a" (sketch-pad-port) path)
            :headers (list (cons "content-type" "application/json"))
            :content body :timeout 10))

;;; --- the drawing, kept -------------------------------------------------------------

(deftest sketch-pad-cell-a-drawing-is-kept-and-named ()
  (with-pad ()
    (let* ((answer (sketch-pad-tool "sketch_pad_save"
                                     "png_base64" (sketch-pad-png-base64)))
           (last (sketch-pad-tool "sketch_pad_last")))
      (is (search "saved " answer) "the save names what it kept")
      (is (search "720x480" answer) "with the drawing's own size, read from its IHDR")
      (is (search "last.png" last) "and the newest drawing is the one just saved")
      (is (search "also kept as" last) "with the dated file that keeps it")
      (let ((path (nodecode-sketch-pad::newest-sketch-file)))
        (is-present path "there is a file on disk")
        (is (probe-file path) "at the path the tool answered")
        (is (search (file-namestring path) last) "which is the one sketch_pad_last names")
        (is (= (length (sketch-pad-png))
               (with-open-file (in path :element-type '(unsigned-byte 8))
                 (file-length in))) "of exactly the bytes that were sent")
        (is (search ".png" (file-namestring path)) "with a .png name")))))

(deftest sketch-pad-cell-the-newest-drawing-is-the-last-save ()
  (with-pad ()
    (sketch-pad-tool "sketch_pad_save" "png_base64" (sketch-pad-png-base64 100 50))
    (let ((first (getf (nodecode-sketch-pad::last-sketch) :width)))
      (sketch-pad-tool "sketch_pad_save" "png_base64" (sketch-pad-png-base64 300 200))
      (is (= 100 first) "the first drawing was read back as it was saved")
      (is (= 300 (getf (nodecode-sketch-pad::last-sketch) :width)) "last.png follows the newest save, not the first")
      (is (= 2 (length (directory (merge-pathnames "sketch-*.png"
                                                   (nodecode-sketch-pad::sketches-directory))))) "and both drawings are kept, one file each"))))

(deftest sketch-pad-cell-a-drawing-that-is-not-a-png-is-refused ()
  (with-pad ("max_bytes" 1024)
    (let ((jpeg (sketch-pad-tool "sketch_pad_save"
                                  "png_base64" (cl-base64:usb8-array-to-base64-string
                                                 (make-array 400 :element-type '(unsigned-byte 8)
                                                                 :initial-element 255))))
          (big (sketch-pad-tool "sketch_pad_save"
                                 "png_base64" (sketch-pad-png-base64 10 10 5000))))
      (is (search "not a PNG" jpeg) "bytes without the PNG signature are refused")
      (is (search "cap is" big) "and one over max_bytes is refused, naming the cap")
      (is (null (nodecode-sketch-pad::newest-sketch-file)) "nothing refused was written"))))

;;; --- the page and the save it posts --------------------------------------------------

(deftest sketch-pad-cell-the-page-saves-over-loopback ()
  (with-pad ()
    (multiple-value-bind (page status) (sketch-pad-get "/")
      (is (= 200 status) "the pad page answers")
      (is (search "canvas" page) "with a canvas to draw on")
      (is (search "toDataURL" page) "and a Save that reads the canvas as a PNG"))
    (multiple-value-bind (body status)
        (sketch-pad-post "/save"
                         (nlk:encode-json-object
                          (nlk:json-object "png_base64" (sketch-pad-png-base64 64 32))))
      (is (= 200 status) "a save posted like the page's is accepted")
      (let* ((answer (nlk:decode-json body))
             (path (nlk:json-value answer :string "path")))
        (is-present path "and answers the file it kept")
        (is (probe-file path) "which is on disk")
        (multiple-value-bind (last-body last-status) (sketch-pad-get "/last")
          (is (= 200 last-status) "and /last answers the newest drawing")
          (let ((newest (nlk:decode-json last-body)))
            (is (search "last.png" (nlk:json-value newest :string "path")) "under the one path the newest drawing always has")
            (is (= 64 (nlk:json-value newest :integer "width")) "the one just saved, not an older one")
            (is (equal path (nlk:json-value newest :string "archive")) "with the dated file this save wrote beside it")))))
    (multiple-value-bind (body status) (sketch-pad-post "/save" "{\"png_base64\": \"bm90IGEgcG5n\"}")
      (is (= 400 status) "a body that is not a PNG is a 400, not a saved file")
      (is (search "PNG" body) "and says why"))
    (multiple-value-bind (body status) (sketch-pad-get "/nowhere")
      (declare (ignore body))
      (is (= 404 status) "an unknown path is a 404"))))

;;; --- the cell's own lifecycle ---------------------------------------------------------

(define-cell-lifecycle-tests "sketch-pad"
  (:config "port" 0 "directory" (namestring (sketch-pad-directory)))
  (:help :sketch)
  (:command "sketch")
  (:running
   (is (integerp (sketch-pad-port)) "the pad answers on a port it was given")
   (is (search ":" (nodecode-sketch-pad::pad-url)) "and pad-url names it")
   (is-present (entry (nle::find-tool "sketch_pad_last")) "sketch_pad_last is registered")
   (is-present (entry (nle::find-tool "sketch_pad_save")) "and sketch_pad_save")
   (is (search "No sketch yet" (sketch-pad-tool "sketch_pad_last")) "with no drawing yet, the tool says so rather than refusing"))
  (:stopped
   (is (null (sketch-pad-port)) "stop stops serving the pad")
   (is (null (nle::find-tool "sketch_pad_last")) "and takes the tools back out"))
  (:refused ("port" "not a number") ("max_bytes" 10) ("max_bytes" -1))
  (:idle nodecode-sketch-pad::sketch-pad-error
         (nodecode-sketch-pad::last-sketch)
         (nodecode-sketch-pad::save-sketch (sketch-pad-png))))
