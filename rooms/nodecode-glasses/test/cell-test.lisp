;;;; cell-test.lisp --- the device verbs, a query's path, and the camera tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here opens a port and nothing here dials out: a request is a clack
;;;; environment handed to the route's own function, the injection is a stub
;;;; that records what the kernel would have been asked to run, the answer is
;;;; a stub's string, and a capture request is answered by the test instead of
;;;; by a pair of glasses.

(in-package #:nodecode.test)

(define-test-slice "glasses" "GLASSES-CELL-")

(defun glasses-config (&rest pairs)
  "A whole shared config with a channels.glasses section built from PAIRS."
  (nlk:json-object "channels"
                    (nlk:json-object "glasses" (apply #'nlk:make-json-object pairs))))

(defun glasses-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-glasses:start-cell (apply #'glasses-config pairs)))

;;; --- the stubs ------------------------------------------------------------------

(defvar *submitted* '()
  "What the stub injection was asked to put where, newest first.")

(defun glasses-submit (session text command-id source-id)
  (push (list session text command-id source-id) *submitted*)
  :started)

(defvar *answers* (make-hash-table :test #'equal)
  "Session -> what the stub says the agent finished with.")

(defmacro with-glasses (&body body)
  "BODY with the cell started on one pair, no queued frame, and every seam a
stub records through."
  `(let ((nodecode-glasses:*submit* #'glasses-submit)
         (nodecode-glasses:*answer* (lambda (session) (gethash session *answers*)))
         (nodecode-glasses:*active-p* (lambda (session) (declare (ignore session)) nil))
         (nodecode-glasses:*await-photo*
           (lambda (request timeout) (declare (ignore request timeout)) nil)))
     (with-cell-stop ((glasses-start "token" "tok-owner"))
       (setf *submitted* '() *answers* (make-hash-table :test #'equal))
       (nodecode-glasses:reset!)
       ,@body)))

(defun glasses-env (uri &key (method :get) body)
  "A request as the gateway hands a route its clack environment."
  (let ((headers (make-hash-table :test #'equal)))
    (when body (setf (gethash "content-type" headers) "application/json"))
    (list :request-method method
          :request-uri uri
          :headers headers
          :content-length (and body (length body))
          :raw-body (and body (make-string-input-stream body)))))

(defun glasses-call (verb &key (method :get) body (token "tok-owner"))
  "The route's answer to one device request => (values STATUS OBJECT)."
  (destructuring-bind (status headers octets)
      (nodecode-glasses:route-answer
       (glasses-env (format nil "/glasses/~a/~a" token verb) :method method :body body))
    (declare (ignore headers))
    (values status (nlk:decode-json octets))))

(defun frame-texts (object)
  "The text of every frame OBJECT carries."
  (loop for frame across (or (gethash "frames" object) #())
        collect (gethash "text" frame)))

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "glasses"
  (:command "glasses")
  (:help :glasses)
  (:config "token" "tok-owner")
  (:refused ("token" "") ("devices" "")))

;;; --- the token and the four verbs -------------------------------------------------

(deftest glasses-cell-answers-a-paired-device-only ()
  (with-glasses
    (multiple-value-bind (status object) (glasses-call "hello")
      (is (= 200 status) "a paired token is answered")
      (is (equal "hello_ack" (gethash "type" object)) "with the handshake")
      (is (equal "owner" (gethash "user" object)) "naming the wearer")
      (is (equal "default" (gethash "device" object)) "and the pair"))
    (multiple-value-bind (status object) (glasses-call "hello" :token "not-a-token")
      (is (= 403 status) "a token the section does not carry is refused")
      (is (equal "error" (gethash "type" object))))
    (multiple-value-bind (status) (glasses-call "nonsense")
      (is (= 404 status) "an unknown verb is not an answer"))))

(deftest glasses-cell-turns-one-query-into-one-turn ()
  (with-glasses
    (multiple-value-bind (status object)
        (glasses-call "say" :method :post :body "{\"text\": \"what time is it\"}")
      (is (= 200 status) "the query is accepted")
      (is (equal "ack" (gethash "type" object)) "and acknowledged")
      (is (equal 1 (length *submitted*)) "one turn was scheduled")
      (is (equal "glasses-owner" (first (first *submitted*))) "in the wearer's own session")
      (is (equal "what time is it" (second (first *submitted*))) "with the wearer's words")
      (is (equal "default" (fourth (first *submitted*))) "named by the pair, for the command id"))
    (multiple-value-bind (status object) (glasses-call "say" :method :post :body "{\"text\": \"  \"}")
      (is (= 400 status) "an empty query is not a turn")
      (is (equal "error" (gethash "type" object))))
    (multiple-value-bind (status) (glasses-call "say" :method :post :body "not json")
      (is (= 400 status) "a body that is not JSON is refused"))
    (is (= 1 (length *submitted*)) "only the accepted query reached a session")
    (let ((nodecode-glasses:*submit* (lambda (session text command-id source-id)
                                        (declare (ignore session text command-id source-id))
                                        :refused)))
      (multiple-value-bind (status) (glasses-call "say" :method :post :body "{\"text\": \"again\"}")
        (is (= 503 status) "a turn the kernel would not take is not claimed")))))

;;; --- what the agent says comes back on the lens ------------------------------------

(deftest glasses-cell-answers-the-wearer-that-asked ()
  (with-glasses
    (glasses-call "say" :method :post :body "{\"text\": \"who am i\"}")
    (let ((session (first (first *submitted*))))
      (setf (gethash session *answers*)
            (format nil "**Ada** you are `owner`~%on the first pair."))
      (nodecode-glasses:deliver-pending)
      (is (equal '("Ada you are owner on the first pair.")
                 (frame-texts (nth-value 1 (glasses-call "poll")))) "the finished answer is stripped of markdown and queued for the device")
      (is (null (frame-texts (nth-value 1 (glasses-call "poll")))) "and polling again finds the queue empty"))
    ;; A long answer is split at the lens's own length.
    (glasses-call "say" :method :post :body "{\"text\": \"tell me everything\"}")
    (let ((session (first (first *submitted*))))
      (setf (gethash session *answers*)
            (format nil "~{~a~^.~}" (loop for i from 1 to 200 collect (format nil "line ~d" i))))
      (nodecode-glasses:deliver-pending)
      (is (> (length (frame-texts (nth-value 1 (glasses-call "poll")))) 1) "a long answer arrives in more than one lens line"))))

(deftest glasses-cell-asks-the-glasses-for-a-photo ()
  (with-glasses
    (is (search "No glasses" (nodecode-glasses:capture-photo nil)) "with no pair, the tool says so instead of waiting")
    (glasses-call "hello")
    (let ((nodecode-glasses:*await-photo*
            (lambda (request timeout)
              (declare (ignore timeout))
              (format nil "/tmp/~a.jpg" request))))
      (is (search "saved at" (nodecode-glasses:capture-photo nil)) "a paired device is asked, and the tool names the file"))
    (let ((frames (gethash "frames" (nth-value 1 (glasses-call "poll")))))
      (is (equal "capture_photo" (gethash "type" (aref frames 0))) "the request reaches the device as a frame it can answer"))
    (is (equal "hello" (map 'string #'code-char (nodecode-glasses:decode-base64 "aGVsbG8=" 64))) "base64 decodes to the bytes the device sent")
    (is (null (nodecode-glasses:decode-base64 "not base64!" 64)) "and garbage decodes to nothing")))

(deftest glasses-cell-knows-a-visual-question ()
  (is (nodecode-glasses:visual-query-p "what am i seeing?") "the original's keywords always capture")
  (is (nodecode-glasses:visual-query-p "is this the bus?") "a deictic pointer does")
  (is (not (nodecode-glasses:visual-query-p "this morning at nine")) "and a grammatical one does not")
  (is (nodecode-glasses:visual-query-p "the one on the left") "the one is a pointer")
  (is (not (nodecode-glasses:visual-query-p "remind me tomorrow")) "an ordinary sentence is not"))

(deftest glasses-cell-slash-command-shows-the-endpoint ()
  (with-glasses
    (let ((line (nodecode-glasses:run-command nil nil)))
      (is (search "/glasses/" line) "the endpoint is shown")
      (is (search "glasses-<user>" line) "with the session a wearer's asks run in"))
    (let ((said (nodecode-glasses:run-command "say hello from the shell" nil)))
      (is (search "200" said) "one query goes down the same path a device takes")
      (is (equal "hello from the shell" (second (first *submitted*))) said))))
