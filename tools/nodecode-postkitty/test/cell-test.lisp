;;;; cell-test.lisp --- the table's counting, the five tools, and /postkitty.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test runs in a scratch folder of its own and reads or writes only
;;;; that folder. Nothing here reaches the network: the only outside program a
;;;; tool starts is ffmpeg, and a box without it is a status the test allows.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(define-test-slice "postkitty" "POSTKITTY-CELL-" :start nodecode-postkitty:start-cell)

(defun kitty-scratch ()
  "A folder of this test's own under the temporary directory, as a namestring."
  (let ((folder (merge-pathnames (format nil "postkitty-cell-~a/" (gensym)) (uiop:temporary-directory))))
    (ensure-directories-exist folder)
    (namestring folder)))

(defmacro with-kitty ((folder &rest pairs) &body body)
  "BODY with the cell started on a scratch FOLDER, stopped on the way out."
  `(let ((,folder (kitty-scratch)))
     (with-cell-stop ((postkitty-start "output_dir" ,folder ,@pairs))
       ,@body)))

(defun kitty-args (&rest pairs)
  "Tool arguments as the JSON string the model's call carries."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun kitty-tool (name &rest pairs)
  "NAME called the way the model calls it -- its registered arguments and its
body -- answered as decoded JSON."
  (let ((entry (nle::find-tool name)))
    (unless entry (error "no tool named ~a is registered" name))
    (nlk:decode-json (funcall (third entry) (nlk:decode-json (apply #'kitty-args pairs))))))

(defun kitty-png (path width height)
  "A file PATH whose header is a WIDTH by HEIGHT PNG, which is all the probe reads."
  (let ((bytes (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for (index . value) in '((0 . 137) (1 . 80) (2 . 78) (3 . 71) (4 . 13) (5 . 10)
                                   (6 . 26) (7 . 10) (12 . 73) (13 . 72) (14 . 68) (15 . 82))
          do (setf (aref bytes index) value))
    (setf (aref bytes 18) (ldb (byte 8 8) width) (aref bytes 19) (ldb (byte 8 0) width)
          (aref bytes 22) (ldb (byte 8 8) height) (aref bytes 23) (ldb (byte 8 0) height))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create
                               :element-type '(unsigned-byte 8))
      (write-sequence bytes out))
    path))

(defun kitty-job (folder &optional (name nil))
  "The job folder's pathname under FOLDER."
  (merge-pathnames (format nil "~a/" name) folder))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "postkitty"
  (:help :postkitty)
  (:command "postkitty")
  (:running
   (is (nle::find-tool "postkitty_scan") "scan is registered")
   (is (nle::find-tool "postkitty_copy") "and copy")
   (is (nle::find-tool "postkitty_make") "and make")
   (is (nle::find-tool "postkitty_redo") "and redo")
   (is (nle::find-tool "postkitty_check") "and check")
   (is (= 5 (count-if (lambda (entry) (eql 0 (search "postkitty_" (first entry)))) nle::*tools*)) "five of them, and no others"))
  (:refused ("loudness_lufs" -100) ("default_platforms" 5)))

(deftest postkitty-cell-stop-takes-every-tool-back ()
  (let ((stop (postkitty-start "output_dir" (kitty-scratch))))
    (is (nle::find-tool "postkitty_scan") "started")
    (funcall stop)
    (is (null (nle::find-tool "postkitty_scan")) "and the stop took it back")
    (is (null (nle::find-tool "postkitty_check")) "all of them")))

;;; --- counting ------------------------------------------------------------------

(deftest postkitty-cell-counts-as-each-platform-does ()
  (is (= 5 (nodecode-postkitty::x-weighted-length "hello")) "plain letters count one")
  (is (= 23 (nodecode-postkitty::x-weighted-length "https://example.com/a/b?c=1")) "a link counts 23")
  (is (= 31 (nodecode-postkitty::x-weighted-length "see https://example.com now")) "and the words around it still count")
  (is (= 2 (nodecode-postkitty::utf16-length (string (code-char #x1F600)))) "an astral character is two UTF-16 units")
  (is (= 1 (nodecode-postkitty::grapheme-length (coerce (list #\e (code-char #x0301)) 'string))) "a letter and its mark are one grapheme")
  (is (= 2 (nodecode-postkitty::count-tags "a #one and #two" #\#)) "hashtags")
  (is (= 1 (nodecode-postkitty::count-tags "ask @someone" #\@)) "mentions")
  (is (= 4 (nodecode-postkitty::utf8-length (coerce (list (code-char #x00E9) (code-char #x00E9)) 'string))) "two bytes a letter here"))

(deftest postkitty-cell-a-thread-is-numbered-and-inside-the-limit ()
  (let* ((draft (format nil "~{~a~^ ~}" (loop for index below 120 collect (format nil "word~d" index))))
         (parts (nodecode-postkitty::split-thread draft "x_weighted" 280)))
    (is (> (length parts) 1) "a long draft is split")
    (is (search "1/" (first parts)) "each part says where it is in the thread")
    (loop for part in parts do
      (is (<= (nodecode-postkitty::count-text part "x_weighted") 280) "and stays inside X's 280"))))

;;; --- scan -----------------------------------------------------------------------

(deftest postkitty-cell-scan-opens-a-job-and-fits-the-draft (with-kitty (folder))
  (let* ((answer (kitty-tool "postkitty_scan" "text" "A short launch note. https://example.com"
                              "platforms" (vector "x" "linkedin")))
         (job (nlk:json-value answer :string "job"))
         (fit (nlk:json-value answer :object "fit")))
    (is (stringp job) "a job id comes back")
    (is (probe-file (merge-pathnames (format nil "~a/manifest.sexp" job) folder)) "and its folder is there")
    (is (= 280 (nlk:json-value fit :integer "x" "limit")) "X's limit is the table's")
    (is (= 44 (nlk:json-value fit :integer "x" "counted")) "and the link counts 23 of the draft's 44")
    (is (= 1 (nlk:json-value answer :integer "text" "urls")) "the one link is counted")))

(deftest postkitty-cell-scan-plans-one-target-per-fit (with-kitty (folder))
  (let* ((png (kitty-png (merge-pathnames "team.png" folder) 1600 1200))
         (answer (kitty-tool "postkitty_scan" "inputs" (vector (namestring png))))
         (plan (nlk:json-array answer "plan")))
    (is (plusp (length plan)) "a photo plans the targets that take photos")
    (is (loop for entry across plan always (member (nlk:json-value entry :string "id")
                                                   '("instagram_feed" "instagram_story" "x_image" "linkedin_image"
                                                     "threads_image" "bluesky_image" "facebook_feed") :test #'equal)) "and only those")
    (is (= 1600 (nlk:json-value (aref (nlk:json-array answer "inputs") 0) :integer "width")) "the photo's width is read from its header")))

;;; --- copy -----------------------------------------------------------------------

(deftest postkitty-cell-copy-counts-splits-and-writes (with-kitty (folder))
  (let* ((job (nlk:json-value (kitty-tool "postkitty_scan" "text" "draft") :string "job"))
         (long (format nil "~{~a~^ ~}" (loop for index below 90 collect (format nil "word~d" index))))
         (answer (kitty-tool "postkitty_copy" "job" job
                              "drafts" (nlk:make-json-object "x" long "threads" "short enough")
                              "alt_text" (nlk:make-json-object "threads_image" "two people at a table")))
         (x (nlk:json-value answer :object "drafts" "x"))
         (threads (nlk:json-value answer :object "drafts" "threads")))
    (is (null (nlk:json-value x :boolean "ok")) "a 90-word draft does not fit X")
    (is (plusp (nlk:json-value x :integer "overflow")) "and the answer says by how much")
    (is (search "1/" (or (aref (nlk:json-array x "thread") 0) "")) "a thread is offered")
    (is (eq t (nlk:json-value threads :boolean "ok")) "a short draft fits Threads")
    (is (probe-file (merge-pathnames (format nil "~a/ready/x/thread.txt" job) folder)) "the thread is written")
    (is (probe-file (merge-pathnames (format nil "~a/ready/threads/post.txt" job) folder)) "so is the post")
    (is (probe-file (merge-pathnames (format nil "~a/ready/threads/alt.txt" job) folder)) "and the alt text")))

;;; --- make and redo ----------------------------------------------------------------

(deftest postkitty-cell-make-a-text-only-job-lists-what-copy-made (with-kitty (folder))
  (let ((job (nlk:json-value (kitty-tool "postkitty_scan" "text" "draft") :string "job")))
    (kitty-tool "postkitty_copy" "job" job "drafts" (nlk:make-json-object "x" "hello"))
    (let ((made (kitty-tool "postkitty_make" "job" job)))
      (is (zerop (length (nlk:json-array made "targets"))) "a text-only job renders nothing")
      (is (search "text only" (or (nlk:json-value made :string "note") "")) "and says so")
      (is (probe-file (merge-pathnames (format nil "~a/board.html" job) folder)) "the page is written"))))

(deftest postkitty-cell-make-every-planned-target-answers-a-status (with-kitty (folder))
  (let* ((png (kitty-png (merge-pathnames "team.png" folder) 1600 1200))
         (job (nlk:json-value (kitty-tool "postkitty_scan" "inputs" (vector (namestring png))) :string "job"))
         (made (kitty-tool "postkitty_make" "job" job))
         (rows (nlk:json-array made "targets")))
    (is (plusp (length rows)) "every planned target is attempted")
    (loop for row across rows do
      (is (member (nlk:json-value row :string "status") '("ready" "skipped" "failed") :test #'equal) "a status the model can report"))
    (is (probe-file (merge-pathnames (format nil "~a/board.html" job) folder)) "the page is written")))

(deftest postkitty-cell-redo-needs-a-target-it-planned (with-kitty (folder))
  (let ((job (nlk:json-value (kitty-tool "postkitty_scan" "text" "draft") :string "job")))
    (is (search "is required" (or (nlk:json-value (kitty-tool "postkitty_redo" "job" job) :string "error") "")) "no target is a refusal")
    (is (search "not planned" (or (nlk:json-value (kitty-tool "postkitty_redo" "job" job "target" "tiktok") :string "error") "")) "and so is a name this job never planned")))

;;; --- check ------------------------------------------------------------------------

(deftest postkitty-cell-check-holds-a-stray-file-against-one-target (with-kitty (folder))
  (let* ((png (kitty-png (merge-pathnames "stray.png" folder) 40 30))
         (passing (kitty-tool "postkitty_check" "path" (namestring png) "target" "x_image"))
         (failing (kitty-tool "postkitty_check" "path" (namestring png) "target" "instagram_feed"))
         (checks (nlk:json-array failing "checks")))
    (is (eq t (nlk:json-value passing :boolean "pass")) "a png is one of X's formats")
    (is (null (nlk:json-value failing :boolean "pass")) "Instagram's feed takes jpeg")
    (is (plusp (length checks)) "a failing file carries the checks that failed")
    (is (search "jpeg" (nlk:json-value (aref checks 0) :string "detail")) "and names the format it wanted")))

;;; --- the operator's command ---------------------------------------------------------

(deftest postkitty-cell-the-command-lists-every-target ()
  (let ((all (nodecode-postkitty:run-command "" nil))
        (one (nodecode-postkitty:run-command "tiktok" nil)))
    (is (search "Instagram" all) "no argument lists the platforms")
    (is (search "youtube_shorts" all) "and every target")
    (is (search "tiktok: 1080x1920" one) "a platform lists only its own")
    (is (null (search "Bluesky" one)) "and nothing else")))
