;;;; cell-test.lisp --- the snapcompact cell, drawing offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; There is nothing to stub: a frame is drawn and written here, from the
;;;; font and the encoder in this folder, and read back off the disk. Every
;;;; call goes through the real dispatch, contract and all, so the schema the
;;;; provider is shown and the body that runs are one shape. Frames land in a
;;;; fresh folder below the scratch home and leave with it.

(in-package #:nodecode.test)

(define-test-slice "snapcompact" "SNAPCOMPACT-CELL-")

(defun snapcompact-config (&rest pairs)
  "A whole shared config holding the `snapcompact' section PAIRS make."
  (nlk:json-object "snapcompact" (apply #'nlk:make-json-object pairs)))

(defun snapcompact-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-snapcompact:start-cell (apply #'snapcompact-config pairs)))

(defun snapcompact-folder ()
  "A fresh empty folder for one test's frames, removed afterwards."
  (merge-pathnames (format nil "snapcompact-test-~d-~d/" (get-universal-time) (random 1000000))
                   (uiop:temporary-directory)))

(defmacro with-snapcompact ((&key max-frames scale) &body body)
  "BODY with the cell started, its frames under a folder of this test's own."
  (let ((folder (gensym "FOLDER")))
    `(let ((,folder (snapcompact-folder)))
       (ensure-directories-exist ,folder)
       (unwind-protect
            (with-cell-stop ((snapcompact-start "output_dir" (namestring ,folder)
                                                ,@(when max-frames
                                                    `("max_frames" ,max-frames))
                                                ,@(when scale
                                                    `("scale" ,scale))))
              ,@body)
         (ignore-errors (uiop:delete-directory-tree ,folder :validate t))))))

(defun snapcompact-answer (id arguments)
  "The snapcompact tool's answer to ARGUMENTS, as the model reads it."
  (nle::message-content (execute-wire-call id "snapcompact" arguments)))

(defun snapcompact-object (id arguments)
  (nlk:decode-json (snapcompact-answer id arguments)))

(defun png-magic-p (path)
  "Whether the file at PATH opens the eight bytes every PNG starts with."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((head (make-array 8 :element-type '(unsigned-byte 8))))
      (and (= 8 (read-sequence head in))
           (equalp head #(137 80 78 71 13 10 26 10))))))

(defun png-size (path)
  "PATH's pixel width and height, read out of its IHDR, or NIL."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((head (make-array 24 :element-type '(unsigned-byte 8))))
      (when (= 24 (read-sequence head in))
        (flet ((big-endian (at)
                 (logior (ash (aref head at) 24) (ash (aref head (1+ at)) 16)
                         (ash (aref head (+ at 2)) 8) (aref head (+ at 3)))))
          (values (big-endian 16) (big-endian 20)))))))

(defun visible-chars (text)
  "TEXT's characters as a page carries them: every one but the line breaks."
  (- (length text) (count #\Newline text)))

(defun snapcompact-lines (count &optional (width 40))
  "COUNT lines of WIDTH characters each, each line numbered so it is told apart."
  (format nil "~{~a~^~%~}"
          (loop for line from 1 to count
                collect (format nil "~2,'0d ~a" line
                                (make-string (- width 3) :initial-element
                                             (code-char (+ 97 (mod line 26))))))))

;;; --- the lifecycle, from the one declaration ---------------------------------------

(define-cell-lifecycle-tests "snapcompact"
  (:config "output_dir" (namestring (snapcompact-folder)))
  (:help :snapcompact)
  (:command "snapcompact")
  (:refused ("output_dir" "")
             ("columns" 3)
             ("rows" 2)))

(deftest snapcompact-cell-refuses-without-an-output-folder ()
  ;; The one setting no verb can work without.
  (let ((condition (signals-error nlk:config-refusal (snapcompact-start))))
    (is (search "output_dir" (princ-to-string condition)) "the refusal names the setting")
    (is (null (nle::find-tool "snapcompact")) "and installs nothing")))

;;; --- the tool ---------------------------------------------------------------------

(deftest snapcompact-cell-draws-a-frame ()
  (with-snapcompact ()
    (is (nle::find-tool "snapcompact") "the tool is registered")
    (let* ((text (snapcompact-lines 4))
           (object (snapcompact-object "sc-1" (nlk:encode-json-object (nlk:json-object "text" text))))
           (frames (nlk:json-value object :array "frames"))
           (path (nlk:json-value (aref frames 0) :string "path")))
      (is (equal 1 (nlk:json-value object :integer "frame_count")) "four short lines are one frame")
      (is (and path (probe-file path)) "the frame was written")
      (is (png-magic-p path) "and it opens as a PNG")
      (multiple-value-bind (width height) (png-size path)
        (is (equal (nlk:json-value (aref frames 0) :integer "width") width)
            "the width the frame names is the file's")
        (is (equal (nlk:json-value (aref frames 0) :integer "height") height)
            "so is the height")
        (is (< width 2000) "a page of a hundred columns stays inside one screen"))
      (is (equal (visible-chars text) (nlk:json-value object :integer "chars"))
          "every visible character of the text is on the page")
      (is (search "(look" (nlk:json-value object :string "guide")) "the answer says how to read it back")
      (is (search path (nlk:json-value object :string "guide")) "and names the frame to look at"))))

(deftest snapcompact-cell-splits-a-long-text-into-pages ()
  (with-snapcompact ()
    (let* ((text (snapcompact-lines 130))
           (object (snapcompact-object "sc-2" (nlk:encode-json-object (nlk:json-object "text" text)))))
      (is (equal 3 (nlk:json-value object :integer "frame_count"))
          "130 rows at sixty to a page are three frames")
      (let ((frames (nlk:json-value object :array "frames")))
        (is (equal (visible-chars text)
                   (reduce #'+ frames :key (lambda (frame)
                                             (nlk:json-value frame :integer "chars"))))
            "and every line is in one of them, once")))))

(deftest snapcompact-cell-renders-a-file ()
  (with-snapcompact ()
    (let* ((path (merge-pathnames "transcript.txt" (uiop:temporary-directory)))
           (text (format nil "user: render this~%ai: done~%")))
      (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string text out))
      (let ((object (snapcompact-object
                      "sc-3" (nlk:encode-json-object (nlk:json-object "file" (namestring path))))))
        (is (equal 1 (nlk:json-value object :integer "frame_count")) "a file renders like a text")
        (is (equal (visible-chars text) (nlk:json-value object :integer "chars"))
            "with the file's own characters")
        (is (search "transcript" (nlk:json-value (aref (nlk:json-value object :array "frames") 0)
                                                 :string "path"))
            "and the file names the archive")))))

(deftest snapcompact-cell-drops-the-oldest-pages-past-the-cap ()
  (with-snapcompact (:max-frames 1)
    (let ((object (snapcompact-object
                   "sc-4" (nlk:encode-json-object (nlk:json-object "text" (snapcompact-lines 130))))))
      (is (equal 1 (nlk:json-value object :integer "frame_count")) "one frame, the newest")
      (is (equal 2 (nlk:json-value object :integer "dropped_frames")) "the two oldest are dropped")
      (is (plusp (nlk:json-value object :integer "dropped_chars")) "and counted"))))

(deftest snapcompact-cell-refuses-an-empty-call ()
  (with-snapcompact ()
    (let ((text (snapcompact-answer "sc-5" "{}")))
      (is (search "no text to render" text) "the tool says what is missing")
      (is (search "ERROR:" text) "as a failed call")))
  (with-snapcompact ()
    (let ((text (snapcompact-answer "sc-6" (nlk:encode-json-object
                                             (nlk:json-object "file" "/nonexistent/notes.txt")))))
      (is (search "does not exist" text) "a missing file is named"))))

(deftest snapcompact-cell-reads-back-at-a-third-the-tokens ()
  (with-snapcompact (:scale 1)
    (let* ((text (snapcompact-lines 60))
           (object (snapcompact-object "sc-7" (nlk:encode-json-object (nlk:json-object "text" text)))))
      (is (< (nlk:json-value object :integer "image_tokens")
             (nlk:json-value object :integer "text_tokens"))
          "a dense page costs fewer tokens than its words do as text"))))

;;; --- /snapcompact -----------------------------------------------------------------

(deftest snapcompact-cell-command-renders-a-file ()
  (with-snapcompact ()
    (let* ((path (merge-pathnames "notes.txt" (uiop:temporary-directory))))
      (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string (snapcompact-lines 3) out))
      (let ((text (nodecode-snapcompact:snapcompact-command (namestring path) nil)))
        (is (search "1 frame" text) "the command answers with the frame it wrote")
        (is (search "notes" text) "named for the file")))))

(deftest snapcompact-cell-command-alone-asks-for-a-file ()
  (with-snapcompact ()
    (multiple-value-bind (text dialog) (nodecode-snapcompact:snapcompact-command "" nil)
      (is (search "/snapcompact" text) "typed alone it explains itself")
      (is dialog "and answers the dialog every command answers then"))))
