;;;; cell-test.lisp --- the lifecycle, the npub, the lap, the answer, the tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the section (a healthy start, a member out of its
;;;; floor or missing refused, the cell idle refusing), the npub decoding the
;;;; original's owner_hex does, the splitting of an answer past the section's
;;;; limit, one new message becoming one turn whose answer is sent back, a
;;;; message already taken not answered twice, and /iris-chat-channel and
;;;; iris_chat_send sending what was typed. The one outside thing the cell
;;;; runs is the iris CLI, so that is what the tests stub: a special variable
;;;; they rebind to a Lisp function. Nothing here needs iris installed, and
;;;; nothing touches a network.

(in-package #:nodecode.test)

(define-test-slice "iris-chat-channel" "IRIS-CHAT-CHANNEL-CELL-")

(defun iris-chat-channel-config (&rest pairs)
  "A whole shared config with one channels.iris section built from PAIRS, as
shasht would decode it."
  (nlk:json-object "channels" (nlk:json-object "iris-chat-channel" (apply #'nlk:make-json-object pairs))))

(defun iris-chat-channel-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-iris-chat-channel:start-cell (apply #'iris-chat-channel-config pairs)))

;;; --- the fixtures ----------------------------------------------------------------

(defparameter +owner+ "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20"
  "One approved contact's hex id: the npub below, decoded.")

(defparameter +npub+ "npub1qypqxpq9qcrsszg2pvxq6rs0zqg3yyc5z5tpwxqergd3c8g7rusqdknev3"
  "The same id as an npub, checksum and all.")

(defvar *state-serial* 0
  "A counter that keeps one test's state directory apart from the next's.")

(defun iris-state-dir (&optional pairing)
  "A private Iris channel directory of this test's own, holding PAIRING as
pairing.json when one is given."
  (let ((dir (merge-pathnames (format nil "iris-chat-channel-state-~d-~d/"
                                       (get-universal-time) (incf *state-serial*))
                               (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    (when pairing
      (with-open-file (out (merge-pathnames "pairing.json" dir) :direction :output
                                                                :if-exists :supersede
                                                                :if-does-not-exist :create)
        (write-string pairing out)))
    ;; A seen list already on disk: a directory the cell has read before, so a
    ;; lap takes what is new rather than seeding a fresh one.
    (with-open-file (out (merge-pathnames "nodecode-seen.txt" dir) :direction :output
                                                                      :if-exists :supersede
                                                                      :if-does-not-exist :create)
      (write-string "seen-before" out))
    (uiop:native-namestring (uiop:ensure-directory-pathname dir))))

(defun iris-pairing (&optional (owner +owner+))
  "A pairing.json naming OWNER, as the original writes it."
  (format nil "{\"owner_hex\": \"~a\", \"chat_id\": \"~a\"}" owner owner))

(defun iris-command (args)
  "ARGS' subcommand: its first word that is not one of the CLI's flags."
  (find-if (lambda (arg) (not (uiop:string-prefix-p "--" arg))) args))

(defvar *calls* '()
  "Every argv this test handed the stubbed CLI, newest first.")

(defvar *submits* '()
  "Every (SESSION PROMPT COMMAND-ID SOURCE-ID) the stubbed ingress took, newest first.")

(defvar *history* "[]"
  "What the stubbed CLI prints for `read'.")

(defmacro with-iris-cli (&body body)
  "BODY with the iris CLI stubbed: every run is recorded in *CALLS* and
answered from *HISTORY* and this stub's fixed answers."
  (let ((run (intern "*RUN*" '#:nodecode-iris-chat-channel))
        (program-run (intern "PROGRAM-RUN" '#:nodecode-iris-chat-channel)))
    `(let ((*calls* '())
           (*history* "[]")
           (,run
             (lambda (program args timeout)
               (declare (ignore timeout))
               (push (list program args) *calls*)
               (values (cond ((equal "read" (iris-command args)) *history*)
                             ((equal "service" (iris-command args))
                              "{\"status\":\"ok\",\"data\":{\"running\":true,\"account_loaded\":true}}")
                             ((equal "send" (iris-command args))
                              "{\"status\":\"ok\",\"data\":{\"message\":{\"id\":\"out-1\"}}}")
                             (t "{\"status\":\"ok\",\"data\":{}}"))
                       "" 0 nil))))
       (unwind-protect (progn ,@body)
         (setf ,run (symbol-function ',program-run))))))

(defun sent-texts ()
  "The text of every `send' this test made, oldest first."
  (loop for (program args) in (reverse *calls*)
        when (equal "send" (iris-command args))
          collect (car (last args))))

(defun iris-message (id body &key (outgoing nil) (chat +owner+))
  "One conversation message, as the CLI's `read' prints it."
  (format nil "{\"id\":\"~a\",\"body\":\"~a\",\"chat_id\":\"~a\",\"is_outgoing\":~a,\"created_at_secs\":1}"
          id body chat (if outgoing "true" "false")))

(defun iris-history (&rest messages)
  "A `read' answer carrying MESSAGES."
  (format nil "{\"status\":\"ok\",\"data\":{\"chat\":{\"kind\":\"direct\",\"chat_id\":\"~a\"},\"messages\":[~{~a~^,~}]}}"
          +owner+ messages))

(defmacro with-iris-running ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS, a state directory and the contact,
stopped on the way out."
  `(with-cell-stop ((iris-chat-channel-start "state_dir" (iris-state-dir)
                                      "owner" +owner+
                                      "poll_seconds" 3600
                                      "manage_service" nil ,@pairs))
     ,@body))

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "iris-chat-channel"
  (:config "state_dir" (iris-state-dir (iris-pairing)) "owner" +owner+
           "poll_seconds" 3600 "manage_service" nil)
  (:help :iris-chat-channel)
  (:command "iris-chat-channel")
  (:running (is (equal +owner+ (nodecode-iris-chat-channel::owner)) "the contact is resolved at start")
            (is nodecode-iris-chat-channel::*poll-worker* "the poll worker runs"))
  (:stopped (is (null nodecode-iris-chat-channel::*poll-worker*) "and the worker is gone")
            (is (null nodecode-iris-chat-channel::*owner*) "and no contact is held"))
  (:refused ("state_dir" (iris-state-dir) "poll_seconds" 0))
  (:idle nodecode-iris-chat-channel:iris-chat-channel-error (nodecode-iris-chat-channel:run-command "" nil)))

;;; --- the npub the original decodes -----------------------------------------------

(defun iris-refusal (function &rest args)
  "What calling FUNCTION with ARGS refuses with, as the text a person would read."
  (handler-case (apply function args)
    (error (condition) (princ-to-string condition))))

(deftest iris-chat-channel-cell-an-npub-and-a-hex-id-are-the-same-contact ()
  (is (equal +owner+ (iris-chat-channel:owner-hex +owner+)) "a hex id is itself")
  (is (equal +owner+ (iris-chat-channel:owner-hex +npub+)) "an npub decodes to it")
  (is (equal +owner+ (iris-chat-channel:owner-hex (concatenate 'string " " +npub+ " "))) "surrounding space is nothing")
  (let ((checksum (iris-refusal #'iris-chat-channel:owner-hex
                                 (concatenate 'string (subseq +npub+ 0 20) "y" (subseq +npub+ 21))))
        (secret (iris-refusal #'iris-chat-channel:owner-hex (concatenate 'string "nsec1" (subseq +owner+ 5)))))
    (is (search "checksum" checksum) "a wrong checksum refuses")
    (is (search "secret key" secret) "a secret key refuses as one")))

(deftest iris-chat-channel-cell-an-answer-past-the-limit-is-split ()
  (is (equal '("short") (iris-chat-channel:chunks "short" 100)) "short text is one message")
  (let* ((text (format nil "~{~a~^ ~}" (loop repeat 40 collect "word")))
         (pieces (iris-chat-channel:chunks text 40)))
    (is (> (length pieces) 1) "long text is more than one")
    (is (every (lambda (piece) (<= (length piece) 40)) pieces) "and each fits the limit")
    (is (equal text (format nil "~{~a~^ ~}" pieces)) "with every word kept, in order")))

;;; --- the lap --------------------------------------------------------------------

(deftest iris-chat-channel-cell-a-new-message-becomes-one-turn-and-its-answer-goes-back ()
  (with-iris-cli
    (with-iris-running ()
      (setf *history* (iris-history (iris-message "m1" "hello there")
                                    (iris-message "m0" "mine" :outgoing t)))
      (let ((*submits* '()))
        (with-stubbed-fdefinition (nodecode-iris-chat-channel::submit-prompt (session prompt command-id source-id)
                                   (push (list session prompt command-id source-id) *submits*)
                                   :started)
          (is (= 1 (nodecode-iris-chat-channel::poll-once)) "one new message, one turn")
          (is (equal "iris-chat-channel" (first (first *submits*))) "in the contact's own session")
          (is (search "hello there" (second (first *submits*))) "carrying its text")
          (is (equal "iris:m1" (third (first *submits*))) "and the message's own id")
          (is (null (sent-texts)) "nothing is sent before the turn answers")))
      (with-stubbed-fdefinitions ((nlk:active-turn-p (session) (declare (ignore session)) nil)
                                  (nlk:turn-answer (session)
                                    (declare (ignore session)) "hello back"))
        (nodecode-iris-chat-channel::deliver-pending))
      (is (equal '("hello back") (sent-texts)) "the answer goes back to the contact")
      (is (null nodecode-iris-chat-channel::*asking*) "and the message is not owed twice"))))

(deftest iris-chat-channel-cell-a-message-already-taken-is-not-answered-again ()
  (with-iris-cli
    (with-iris-running ()
      (setf *history* (iris-history (iris-message "m1" "hello there")))
      (with-stubbed-fdefinition (nodecode-iris-chat-channel::submit-prompt (session prompt command-id source-id)
                                 (declare (ignore session prompt command-id source-id))
                                 :started)
        (is (= 1 (nodecode-iris-chat-channel::poll-once)) "the first lap takes it")
        (is (= 0 (nodecode-iris-chat-channel::poll-once)) "the second lap finds nothing new")))))

(deftest iris-chat-channel-cell-a-first-lap-seeds-rather-than-replaying-the-past ()
  (with-iris-cli
    (with-iris-running ()
      (setf nodecode-iris-chat-channel::*seeding* t
            *history* (iris-history (iris-message "m9" "from before")))
      (with-stubbed-fdefinition (nodecode-iris-chat-channel::submit-prompt (session prompt command-id source-id)
                                 (declare (ignore session prompt command-id source-id))
                                 :started)
        (is (= 0 (nodecode-iris-chat-channel::poll-once)) "a fresh state directory answers nothing old")
        (is (gethash "m9" nodecode-iris-chat-channel::*seen*) "but the old message is marked seen")
        (is (null nodecode-iris-chat-channel::*seeding*) "and the seeding lap is over")))))

;;; --- what the operator and the model can call -------------------------------------

(deftest iris-chat-channel-cell-the-command-and-the-tool-send-to-the-contact ()
  (with-iris-cli
    (with-iris-running ()
      (is (search (subseq +owner+ 0 16) (iris-chat-channel:status-line)) "the status names the contact")
      (let ((said (iris-chat-channel:run-command "hi from the operator" nil)))
        (is (search "Sent to the approved Iris contact" said) "the command answers that it sent"))
      (is (equal '("hi from the operator") (sent-texts)) "and the CLI was told to send it")
      (let ((answer (iris-chat-channel:tool-send (nlk:make-json-object "text" "hi from the model")))
            (nothing (iris-refusal #'iris-chat-channel:tool-send (nlk:make-json-object))))
        (is (search "Sent 1 message" answer) "the tool answers how many it took")
        (is (equal "hi from the model" (car (last (sent-texts)))) "and the model's text is what went")
        (is (equal "send" (iris-command (cadr (first *calls*)))) "through `iris send', not a shell")
        (is (search "text" nothing) "a call with no text refuses by name")))))

(deftest iris-chat-channel-cell-an-owner-that-cannot-be-the-contact-refuses-by-name ()
  (with-iris-cli
    (let ((text (handler-case
                    (progn (iris-chat-channel-start "state_dir" (iris-state-dir)
                                             "owner" (concatenate 'string "nsec1" (subseq +owner+ 5))
                                             "poll_seconds" 3600 "manage_service" nil)
                           "")
                  (error (condition) (princ-to-string condition)))))
      (is (search "owner" text) "the refusal names the key that is wrong"))
    (is (null (nle::find-registered-command "iris-chat-channel")) "and nothing was installed")))
