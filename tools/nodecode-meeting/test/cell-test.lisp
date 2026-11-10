;;;; cell-test.lisp --- lifecycle, the four tools, the echo rule, a record, staleness.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring and its files, over a scratch
;;;; directory: that a start installs the four tools, /meeting and the manual and a
;;;; stop takes every one of them back; that a line appended through the real
;;;; dispatch lands in both transcript files and comes back with its freshness; that
;;;; an echo is dropped while the phone's mic still wins; that a record is written
;;;; under the records root with the title, the notes and the transcript; that a
;;;; transcript gone quiet is called stale; and that a blank directory or a count
;;;; under its floor refuses the start.

(in-package #:nodecode.test)

;;; --- lifecycle ----------------------------------------------------------------------

(define-cell-lifecycle-tests "meeting"
  (:help :meeting)
  (:command "meeting")
  (:running
   (is (nle::find-tool "meeting_status") "meeting_status is registered")
   (is (nle::find-tool "meeting_transcript") "meeting_transcript is registered")
   (is (nle::find-tool "meeting_append") "meeting_append is registered")
   (is (nle::find-tool "meeting_save") "meeting_save is registered")
   (is (uiop:directory-exists-p (nodecode-meeting::state-dir)) "start made the transcript directory")
   (is (search "nothing transcribed yet" (nodecode-meeting:transcript)) "an empty transcript says so")
   (is (= 20000 (nodecode-meeting::setting :window-chars)) "window_chars defaults to 20000"))
  (:stopped
   (is (null (nle::find-tool "meeting_status")) "stop unregisters meeting_status")
   (is (null (nle::find-tool "meeting_save")) "stop unregisters meeting_save"))
  (:refused
   ("dir" "")
   ("window_chars" 100)))

;;; --- one line, both files, and back to the model -------------------------------------

(deftest meeting-cell-a-line-lands-in-both-transcript-files (with-meeting ()
    (let ((answer (meeting-call-tool "meeting_append"
                                      "{\"text\":\"we should ship on Friday\",\"source\":\"audio\"}")))
      (is (search "appended" answer) "the tool answers what it appended"))
    (let ((rolling (uiop:read-file-string (nodecode-meeting::transcript-file "rolling")))
          (full (uiop:read-file-string (nodecode-meeting::transcript-file "full"))))
      (is (search "we should ship on Friday" rolling) "the line is in the rolling transcript")
      (is (search "[audio]" rolling) "tagged with where it came from")
      (is (search "we should ship on Friday" full) "and kept in the full one"))
    (let ((text (meeting-call-tool "meeting_transcript" "{\"lines\":5}")))
      (is (search "we should ship on Friday" text) "and the model reads it back")
      (is (search "live" text) "with its freshness"))
    (is (search "1 line" (meeting-call-tool "meeting_status" "{}")) "status counts it")))

(deftest meeting-cell-an-echo-is-dropped-not-doubled (with-meeting ()
    (meeting-call-tool "meeting_append" "{\"text\":\"the budget is approved\"}")
    (let ((answer (meeting-call-tool "meeting_append"
                                      "{\"text\":\"the budget is approved\",\"source\":\"audio\"}")))
      (is (search "already says that" answer) "the same words from the other side are an echo"))
    (is (= 1 (length (nodecode-meeting::file-lines (nodecode-meeting::transcript-file "rolling")))) "one line, not two")
    (meeting-call-tool "meeting_append"
                       "{\"text\":\"the budget is approved\",\"source\":\"phone\"}")
    (is (= 2 (length (nodecode-meeting::file-lines (nodecode-meeting::transcript-file "rolling")))) "the phone's mic wins")))

;;; --- a record ------------------------------------------------------------------------

(deftest meeting-cell-a-save-writes-a-record-under-the-records-root (with-meeting ()
    (meeting-call-tool "meeting_append" "{\"text\":\"ship the patch on Friday\"}")
    (let* ((answer (meeting-call-tool
                     "meeting_save"
                     "{\"title\":\"Weekly sync: budget\",\"notes\":\"ship Friday\"}"))
           (path (uiop:parse-native-namestring (subseq answer 6))))
      (is (search "saved" answer) "the tool answers the path")
      (is (uiop:subpathp path (nodecode-meeting::records-root)) "under the records root")
      (is (equal "md" (pathname-type path)) "a markdown file")
      (is (search "weekly-sync-budget" (namestring path)) "named from the title")
      (let ((text (uiop:read-file-string path)))
        (is (search "# Weekly sync: budget" text) "the title")
        (is (search "## Notes" text) "the notes")
        (is (search "ship Friday" text) "what was decided")
        (is (search "ship the patch on Friday" text) "and the transcript beside them")))))

(deftest meeting-cell-a-save-without-the-transcript-still-writes (with-meeting ()
    (meeting-call-tool "meeting_append" "{\"text\":\"said out loud\"}")
    (let* ((answer (meeting-call-tool
                     "meeting_save"
                     "{\"title\":\"No transcript\",\"include_transcript\":false}"))
           (text (uiop:read-file-string (uiop:parse-native-namestring (subseq answer 6)))))
      (is (search "(not captured)" text) "the record says the transcript was left out")
      (is (null (search "said out loud" text)) "and carries none of it"))))

;;; --- staleness -----------------------------------------------------------------------

(deftest meeting-cell-a-quiet-transcript-is-called-stale (with-meeting ()
    (meeting-call-tool "meeting_append" "{\"text\":\"anyone still on the call\"}")
    (is (search "live" (meeting-call-tool "meeting_status" "{\"verbose\":true}")) "fresh after a line")
    (meeting-backdate (nodecode-meeting::transcript-file "rolling") 600)
    (is (search "stale" (string-downcase (meeting-call-tool "meeting_status" "{}"))) "ten minutes on it is stale")
    (is (search "STALE" (meeting-call-tool "meeting_transcript" "{}")) "and the transcript says so first")))
