;;;; cell-test.lisp --- cacheguard's lifecycle, its decision, and its lines.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The seams the cell reads are the cell's own: what a provider round
;;;; returned (a usage object the caller hands in) and the clock, so every
;;;; test here runs offline with the provider call stubbed.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "cacheguard"
  (:command "cacheguard")
  (:hooks 'nodecode.evolved::call-provider)
  (:running (is (stringp (nodecode-cacheguard::settings-text)) "the settings are read once, at start"))
  (:refused ("mode" "shout")))

;;; --- helpers ----------------------------------------------------------------

(defun cacheguard-call (next)
  "The installed advice, funcalled by hand with NEXT as the provider call."
  (funcall (cacheguard-hook) next :context))

(defmacro with-quiet-notices (&body body)
  "Run BODY with NLE:NOTICE and NLE:TURN stubbed; answers the notices it made."
  `(let ((notices '()))
     (with-stubbed-fdefinitions ((nle:turn () (list :session-id "s1"))
                                 (nle:notice (text &key level key)
                                   (push (list text level key) notices)
                                   nil))
       ,@body
       (nreverse notices))))

(defun notice-text (notice)
  (first notice))

;;; --- the decision ----------------------------------------------------------

(deftest cacheguard-blocks-a-big-prompt-after-a-pause
  (with-cell-stop ((cacheguard-start "mode" "block"))
    (cacheguard-seed "s1")
    (let ((called nil) (answer nil))
      (let ((notices (with-quiet-notices
                       (setf answer
                             (multiple-value-list
                              (cacheguard-call (lambda (&rest args)
                                                 (declare (ignore args))
                                                 (setf called t)
                                                 (values :message :usage nil nil nil))))))))
        (is (not called) "the provider was never called")
        (is (search "BLOCKED" (gethash "content" (first answer))) "the round answers BLOCKED")
        (is (search "48k" (gethash "content" (first answer)))
            (format nil "and how much may be rebilled; the line was ~a"
                    (gethash "content" (first answer))))
        (is (null (second answer)) "no usage is invented")
        (is (null (third answer)) "and no finish reason")
        (is (search "BLOCKED" (notice-text (first notices))) "the operator is told, too")
        (is (< (or (nodecode-cacheguard::session-pause "s1") 999) 1) "and the pause clock restarted, so sending it again goes out")))))

(deftest cacheguard-lets-a-small-prompt-through
  (with-cell-stop ((cacheguard-start "mode" "block"))
    (cacheguard-seed "s1" :pause-minutes 12 :tokens 4000)
    (let ((called nil))
      (with-quiet-notices
        (cacheguard-call (lambda (&rest args)
                            (declare (ignore args))
                            (setf called t)
                            (values :message nil nil nil nil))))
      (is called "a prompt under the size floor goes straight through"))))

(deftest cacheguard-reports-a-lost-cache
  (with-cell-stop ((cacheguard-start "mode" "warn"))
    (cacheguard-seed "s1")
    (let* ((answer nil)
           (notices (with-quiet-notices
                      (setf answer
                            (multiple-value-list
                             (cacheguard-call
                              (lambda (&rest args)
                                (declare (ignore args))
                                (values (nlk:json-object "role" "assistant" "content" "ok")
                                        (cacheguard-usage :input 48000)
                                        "stop" nil nil))))))))
      (is (string= "ok" (gethash "content" (first answer))) "the round still runs")
      (is (= 1 (length notices)) "one result line")
      (is (search "cache lost" (notice-text (first notices))) "it says the cache was lost")
      (is (search "48k rebilled" (notice-text (first notices))) "with what was rebilled")
      (is (eq :warning (second (first notices))) "as a warning")
      (is (search "5-15m" (nodecode-cacheguard::table-text)) "and the journal learned the pause"))))

(deftest cacheguard-kept-cache-is-quiet-unless-asked
  (with-cell-stop ((cacheguard-start "mode" "warn"))
    (cacheguard-seed "s2")
    (let ((notices (with-quiet-notices
                     (cacheguard-call
                      (lambda (&rest args)
                        (declare (ignore args))
                        (values (nlk:json-object "role" "assistant" "content" "ok")
                                (cacheguard-usage :cached 41000) "stop" nil nil))))))
      (is (null notices) "a kept cache says nothing by default")
      (multiple-value-bind (prompt cached lost)
          (nodecode-cacheguard::note-usage "s2" (cacheguard-usage :cached 41000) 12)
        (is (= 41000 prompt) "the prompt it filled is remembered")
        (is (= 41000 cached) "with what the cache read")
        (is (not lost) "and it counts as kept")))))

(deftest cacheguard-command-answers-and-refuses
  (with-cell-stop ((cacheguard-start))
    (let ((status (nodecode-cacheguard::run-slash "" nil)))
      (is (search "mode" status) "the status names the settings")
      (is (search "no observations yet" status) "and the empty table"))
    (is (search "pause_minutes 10 -> 5" (nodecode-cacheguard::run-slash "set pause_minutes 5" nil)) "a setting changes")
    (is (= 5 (nodecode-cacheguard::option :pause)) "and is the value the hooks run on")
    (is (search "off, warn or block" (nodecode-cacheguard::run-slash "set mode shout" nil)) "a value outside the choices refuses")
    (is (search "not a cacheguard setting" (nodecode-cacheguard::run-slash "set bogus 1" nil)) "an unknown setting refuses")
    (is (search "history cleared" (nodecode-cacheguard::run-slash "reset" nil)) "reset clears what it learned")))
