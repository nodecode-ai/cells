;;;; cell-test.lisp --- the android-bridge cell against a stubbed relay.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs a relay, a phone or a network: every call goes through
;;;; NODECODE-ANDROID-BRIDGE::*HTTP*, a stub that records the method, the URL,
;;;; the headers and the body it was handed and answers what the test chose.
;;;; What is under test is the part this cell owns: the request it builds out
;;;; of a declared call, the Bearer it carries, the answer it reads back, and
;;;; the words it says when a call cannot be made at all.

(in-package #:nodecode.test)

(define-test-slice "android-bridge" "ANDROID-BRIDGE-CELL-"
  :start nodecode-android-bridge:start-cell)

(define-cell-lifecycle-tests "android-bridge"
  (:config "url" "http://127.0.0.1:9812" "token" "pair-me")
  (:help :android-bridge)
  (:running
   (is (nle::find-tool "android_ping") "the ping is registered")
   (is (nle::find-tool "android_send_sms") "and one that reaches a person")
   (is (= 58 (count-if (lambda (entry) (and (stringp (first entry)) (eql 0 (search "android_" (first entry))))) nle::*tools*)) "fifty-eight tools, the original plugin's")
   (is (equal "pair-me" (android-bridge::setting :token)) "the pairing code is read off the section"))
  (:stopped
   (is (null (nle::find-tool "android_ping")) "and stop takes every one back")
   (is (null (nle::find-tool "android_send_sms"))))
  (:refused ("url" "")
            ("timeout_seconds" 0)))

;;; --- the stub ------------------------------------------------------------------

(defvar *calls* nil
  "The calls the stub was handed, newest first, each (METHOD URL HEADERS CONTENT TIMEOUT BINARY).")

(defmacro with-bridge ((&key (answer "{}") (status 200) (url "http://127.0.0.1:9812")) &body body)
  "BODY with the cell started and every relay call answered by a stub: ANSWER
and STATUS stand in for the relay's body and status. *CALLS* records the request."
  `(let* ((*calls* nil)
          (nodecode-android-bridge::*http*
            (lambda (method url &key headers content timeout binary &allow-other-keys)
              (push (list method url headers content timeout binary) *calls*)
              (values ,answer ,status nil nil))))
     (with-cell-stop ((android-bridge-start "url" ,url "token" "pair-me"))
       ,@body)))

(defun last-call (&optional (newest 0))
  "The NEWEST'th call the stub saw, 0 the last one."
  (nth newest *calls*))

(defun header (name &optional (newest 0))
  (cdr (assoc name (third (last-call newest)) :test #'equal)))

(defun bridge-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "bridge-call" name arguments)))

;;; --- one route per shape --------------------------------------------------------

(deftest android-bridge-cell-a-tap-names-a-node ()
  (with-bridge ()
    (let ((answer (bridge-call "android_tap" "{\"node_id\":\"n7\"}")))
      (is (eq :post (first (last-call))) "a tap is a POST")
      (is (equal "http://127.0.0.1:9812/tap" (second (last-call))) "to the relay's tap route")
      (is (equal "{\"nodeId\":\"n7\"}" (fourth (last-call))) "the node id rides as nodeId")
      (is (equal "Bearer pair-me" (header "Authorization")) "and the pairing code is the Bearer")
      (is (equal "{}" answer) "the relay's answer is the tool's answer"))))

(deftest android-bridge-cell-a-read-names-its-options ()
  (with-bridge ()
    (bridge-call "android_read_screen" "{\"include_bounds\":true}")
    (is (eq :get (first (last-call))) "a screen read is a GET")
    (is (equal "http://127.0.0.1:9812/screen?bounds=true" (second (last-call))) "bounds is the one query parameter sent")
    (is (null (fourth (last-call))) "a GET carries no body")))

(deftest android-bridge-cell-a-tap-with-nothing-to-tap-refuses ()
  (with-bridge (:answer "{\"ok\":true}")
    (let ((answer (bridge-call "android_tap" "{}")))
      (is (search "node_id" answer) "the answer says what a tap needs")
      (is (null *calls*) "and nothing was sent to the phone"))))

(deftest android-bridge-cell-a-refused-status-reads-as-an-error ()
  (with-bridge (:answer "{\"error\":\"no phone connected\"}" :status 503)
    (let ((answer (bridge-call "android_ping" "{}")))
      (is (search "HTTP 503" answer) "the status is in the answer")
      (is (search "no phone connected" answer) "and so is what the relay said"))))

(deftest android-bridge-cell-a-file-comes-back-as-media ()
  (with-bridge (:answer (vector 82 73 70 70 1 2 3 4))
    (let ((answer (bridge-call "android_file_get" "{\"remote_path\":\"Download/scan.pdf\"}")))
      (is (search "MEDIA:" answer) "the answer names the local file")
      (is (equal "http://127.0.0.1:9812/file?path=Download%2Fscan.pdf" (second (last-call))) "the phone path is percent-encoded into the query")
      (is (eq t (sixth (last-call))) "the body was read as octets")
      (let ((path (string-trim '(#\Return #\Newline)
                                (subseq answer (+ 6 (search "MEDIA:" answer))))))
        (is (probe-file path) "and the file is there")
        (when (probe-file path) (ignore-errors (delete-file path)))))))

(deftest android-bridge-cell-a-macro-runs-its-steps-in-order ()
  (with-bridge (:answer "{\"connected\":true}")
    (let ((answer (bridge-call "android_macro"
                               "{\"name\":\"wake\",\"steps\":[{\"tool\":\"android_ping\",\"args\":{}}]}")))
      (is (search "macro wake" answer) "the answer names the macro")
      (is (search "1. android_ping" answer) "and lists the step it ran")
      (is (eq 1 (length *calls*)) "one call, one step"))))

(deftest android-bridge-cell-setup-says-what-to-do-next ()
  (with-bridge (:answer "{\"phone_connected\":false}")
    (let ((answer (bridge-call "android_setup" "{}")))
      (is (search "http://127.0.0.1:9812" answer) "the answer names the relay it asked")
      (is (search "pairing code" answer) "and what the person does on the phone"))))
