;;;; cell-test.lisp --- the lifecycle, the storyboard, the cut, the scrub, the commands.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring over a real store and real
;;;; session facts: that the start installs the tool, the command and the
;;;; manual and the stop takes them back; that a section without its one
;;;; required member refuses; that the storyboard keeps a failing step
;;;; together with the step that fixed it and fits the hard cap; that a cut
;;;; runs the user's ffmpeg once for the video and once for the poster,
;;;; writes the MP4, the .srt and the poster, and burns one caption per
;;;; beat; that a secret in the history never reaches a frame or a subtitle;
;;;; and that /showreel answers its verbs.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "showreel"
  (:config "output_dir" (namestring (showreel-dir)))
  (:help :showreel)
  (:command "showreel")
  (:running (is (equal nodecode-showreel::+primer+ (nle:help :showreel)) "the manual is the primer")
            (is (= 45 (nodecode-showreel::setting :default-length)) "45 s by default")
            (is (equal "16:9" (nodecode-showreel::setting :default-aspect))))
  (:refused ()
            ("default_length" 5)
            ("default_aspect" "4:3")
            ("resolution" "4k"))
  (:idle showreel:showreel-error (showreel:render :session-id "s1")))

;;; --- the storyboard -------------------------------------------------------------

(deftest showreel-cell-a-failure-and-its-fix-stay-together ()
  (with-showreel ()
    (showreel-history)
    (let* ((beats (nodecode-showreel::storyboard "s1" :target 30))
           (kinds (mapcar (lambda (beat) (getf beat :kind)) beats))
           (labels (mapcar (lambda (beat) (getf beat :label)) beats))
           (captions (format nil "~{~a | ~}" (mapcar (lambda (beat) (getf beat :caption)) beats)))
           (total (reduce #'+ beats :key (lambda (b) (getf b :duration)) :initial-value 0.0)))
      (is (equal "title" (first kinds)) "a cut opens on the title card")
      (is (member "ask" kinds :test #'string=) "the ask is a beat")
      (is (some (lambda (label) (search "FAILED" label)) labels) "the failing step is kept")
      (is (search "12 passing" captions) "and the step that fixed it")
      (is (<= total 89) "a cut never runs past the hard cap")
      (is (member "stats" kinds :test #'string=) "the spend is a beat"))))

(deftest showreel-cell-a-cut-fits-the-length-it-was-asked-for ()
  (with-showreel ()
    (showreel-history)
    (let* ((short (reduce #'+ (nodecode-showreel::storyboard "s1" :target 20)
                          :key (lambda (b) (getf b :duration)) :initial-value 0.0))
           (long (reduce #'+ (nodecode-showreel::storyboard "s1" :target 60)
                         :key (lambda (b) (getf b :duration)) :initial-value 0.0)))
      (is (< short long) "a longer target makes a longer cut")
      (is (<= short 30) "and a short target stays short")
      (is (plusp (getf (first (nodecode-showreel::storyboard "s1" :target 20)) :duration))))))

;;; --- the cut ---------------------------------------------------------------------

(defun showreel-video-call ()
  "The stub's argv for the encode: the one that carries a filter graph."
  (find "-vf" *calls* :test (lambda (flag argv) (member flag argv :test #'string=))))

(deftest showreel-cell-a-cut-runs-ffmpeg-and-writes-every-file ()
  (with-showreel ()
    (showreel-history)
    (let ((text (showreel-call-tool "session_id" "s1" "length_seconds" 20
                                     "aspect" "9:16" "title" "Snake")))
      (is (search "Showreel cut:" text) "the tool answers the cut's line")
      (is (= 2 (length *calls*)) "one ffmpeg run for the video, one for the poster")
      (let* ((argv (showreel-video-call))
             (graph (and argv (nth (1+ (position "-vf" argv :test #'string=)) argv)))
             (dir (uiop:pathname-directory-pathname
                   (uiop:parse-native-namestring (car (last argv))))))
        (is (search "9:16" text) "the aspect the caller asked for")
        (is (search "s=1080x1920" (format nil "~{~a ~}" argv)) "upright draws 1080x1920")
        (is (string= graph (nodecode-showreel::filter-graph
                            (nodecode-showreel::storyboard "s1" :target 20) 1080)) "one filter per beat")
        (is (search "drawtext=" graph) "each beat draws its caption")
        (is (search "between(t," graph) "and only in its own span")
        (is (search "fontfile=font.ttf" graph) "the font is copied beside the caption files")
        (let ((mp4 (showreel-file dir "mp4"))
              (srt (showreel-file dir "srt")))
          (is (and mp4 (probe-file mp4)) "the MP4 lands in the render folder")
          (is (and srt (probe-file srt)) "so does the .srt")
          (is (search "-->" (showreel-text srt)) "which carries timed captions"))))))

(deftest showreel-cell-a-secret-never-reaches-a-frame-or-a-subtitle ()
  (with-showreel ()
    (showreel-history "s1" :secret "AKIAABCDEFGHIJKLMNOP and hello@example.com")
    (showreel-call-tool "session_id" "s1" "length_seconds" 20)
    (let* ((srt (showreel-text
                 (showreel-file
                  (uiop:pathname-directory-pathname
                   (uiop:parse-native-namestring (car (last (showreel-video-call)))))
                  "srt"))))
      (is (not (search "AKIAABCDEFGHIJKLMNOP" srt)) "the key is masked")
      (is (not (search "hello@example.com" srt)) "the email is masked"))))

(deftest showreel-cell-a-cut-refuses-what-it-cannot-honor ()
  (with-showreel ()
    (showreel-history)
    (is (search "aspect must be"
                (refusal-text showreel:showreel-error
                              (showreel:render :session-id "s1" :aspect "4:3"))))
    (is (search "10 to"
                (refusal-text showreel:showreel-error
                              (showreel:render :session-id "s1" :length 400))))
    (is (search "nothing recorded"
                (refusal-text showreel:showreel-error
                              (showreel:render :session-id "s2"))))))

;;; --- /showreel --------------------------------------------------------------------

(deftest showreel-cell-the-command-answers-its-verbs ()
  (with-showreel ()
    (showreel-history)
    (let ((run (nle::find-registered-command "showreel")))
      (flet ((verb (args)
               (funcall (nle::slash-command-headless run) args "s1")))
        (is (search "Showreel plan:" (verb "plan 20")) "plan lists the beats")
        (is (search "nothing recorded"
                    (refusal-text showreel:showreel-error
                                  (showreel:render :session-id "s2"))) "nothing to cut")
        (is (search "showreel: ffmpeg" (verb "status")) "status names the ffmpeg it would run")
        (is (search "Showreel cut:" (verb "")) "bare /showreel cuts this session")
        (is (search "Showreel cut:" (verb "cut 20 9:16")) "cut renders this session")
        (is (search "Showreel demo:" (verb "demo 20")) "demo renders the built-in sample")))))
