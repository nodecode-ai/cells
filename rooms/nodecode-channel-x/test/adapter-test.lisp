;;;; adapter-test.lisp --- the X lane end to end, over a fake bridge.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The lane runs as it runs for an operator: its poll lap and the kit's host
;;;; over the real nodecode gateway (WITH-ADAPTER-LANE, the provider a stub),
;;;; with a fake standing where the bridge child would: it answers me, user_id,
;;;; mentions, tweet and reply as bridge.py does. Covers: the first read
;;;; answering nothing said before it, an allowed person's mention answered in
;;;; reply under it, a stranger's not, a reply under the bot's post answered
;;;; without a name, the id kept, and a cookie signed in as another account
;;;; refused.

(in-package #:nodecode.test)

(defstruct (fake-x (:copier nil))
  (me "900")
  (accounts '(("nodebot" . "900") ("kim" . "501") ("eve" . "502")))
  (tweets '())
  (replies '())
  (reads 0)
  (next 5000)
  (lock (sb-thread:make-mutex :name "fake x")))

(defun fake-x-mention (fake id author text &key in-reply-to (conversation id))
  "FAKE's timeline gains tweet ID by account handle AUTHOR."
  (sb-thread:with-mutex ((fake-x-lock fake))
    (push (nlk:make-json-object "id" id "text" text
                                "author_id" (cdr (assoc author (fake-x-accounts fake) :test #'string=))
                                "author" author "author_name" author
                                "in_reply_to" in-reply-to "conversation_id" conversation)
          (fake-x-tweets fake))))

(defun fake-x-answer (fake body)
  (flet ((ok (&rest pairs) (nck:make-execution :ok-p t :status 200 :attempts 1
                                               :body (apply #'nlk:make-json-object "ok" t pairs))))
    (sb-thread:with-mutex ((fake-x-lock fake))
      (let ((op (gethash "op" body)))
        (cond
          ((equal op "me") (ok "user_id" (fake-x-me fake)))
          ((equal op "user_id")
           (let ((id (cdr (assoc (gethash "handle" body) (fake-x-accounts fake) :test #'string-equal))))
             (if id (ok "user_id" id) (nck:make-execution :ok-p nil :status 404 :error "no such account"))))
          ((equal op "mentions")
           (incf (fake-x-reads fake))
           (let ((since (parse-integer (gethash "since" body))))
             (ok "tweets" (coerce (sort (remove-if-not (lambda (tweet) (> (parse-integer (gethash "id" tweet)) since))
                                                       (copy-list (fake-x-tweets fake)))
                                        #'< :key (lambda (tweet) (parse-integer (gethash "id" tweet))))
                                  'vector))))
          ((equal op "tweet")
           (let ((mine (find (gethash "id" body) (fake-x-replies fake)
                             :key (lambda (reply) (gethash "id" reply)) :test #'equal)))
             (ok "tweet" (nlk:make-json-object "id" (gethash "id" body)
                                               "author_id" (if mine (fake-x-me fake) "501")))))
          ((equal op "reply")
           (let ((id (princ-to-string (incf (fake-x-next fake)))))
             (push (let ((copy (alexandria:copy-hash-table body))) (setf (gethash "id" copy) id) copy)
                   (fake-x-replies fake))
             (ok "id" id)))
          (t (nck:make-execution :ok-p nil :status 400 :error (format nil "no op ~a" op))))))))

(defun fake-x-executor (fake)
  (nck:make-plan-executor :run (lambda (plan) (fake-x-answer fake (nck:request-plan-body plan)))))

(defun fake-x-replies-to (fake id)
  (sb-thread:with-mutex ((fake-x-lock fake))
    (remove-if-not (lambda (reply) (equal id (gethash "reply_to" reply))) (fake-x-replies fake))))

(defmacro with-x-lane ((fake &rest members) &body body)
  "BODY against a started X lane over FAKE -- MEMBERS the section's own -- once
it has read where the timeline stood; the provider answers `x reply'."
  (let ((dir (gensym "DIR")))
    `(let* ((,dir (uiop:ensure-directory-pathname
                   (merge-pathnames (format nil "nodecode-x-test-~36r/" (random (expt 36 8)))
                                    (uiop:temporary-directory))))
            (ncx::*cursor-state-directory* (ensure-directories-exist ,dir))
            (ncx::*poll-floor* 0.1))
       (unwind-protect
            (with-adapter-lane (:reply "x reply" :token "auth_token=a; ct0=b" :start ncx:start-channel
                                :bindings ((executor (fake-x-executor ,fake)))
                                :section (let ((section (nlk:make-json-object
                                                         "handle" "@NodeBot" "token_file" token-path
                                                         "poll_seconds" 0 "soul_file" soul-path
                                                         "request_timeout_seconds" 5)))
                                           (loop for (key value) on (list ,@members) by #'cddr
                                                 do (setf (gethash key section) value))
                                           section)
                                :plans nil
                                :after ((is (adapter-threads-stopped-p "channel-x"))))
              (is (await (:timeout 10) (plusp (fake-x-reads ,fake))) "the lane reads the timeline")
              ,@body)
         (uiop:delete-directory-tree ,dir :validate t :if-does-not-exist :ignore)))))

(deftest channel-x-mention-answered-backlog-not ()
  (let ((fake (make-fake-x)))
    (fake-x-mention fake "100" "kim" "@nodebot said before")
    (with-x-lane (fake "allowed_users" (vector "kim") "owner" (vector "@kim"))
      (is (await (:timeout 10) (>= (fake-x-reads fake) 2)) "the first read is past")
      (fake-x-mention fake "200" "kim" "@nodebot what is up")
      (is-present (answer (await (:timeout 15) (first (fake-x-replies-to fake "200"))))
        "the answer was posted in reply under the ask"
        (is (equal "x reply" (gethash "text" answer)) "as the model said it, no footer"))
      (is (null (fake-x-replies-to fake "100")) "what was said before the first read opened nothing")
      (is (await (:timeout 10)
                 (let ((file (first (directory (merge-pathnames "*.cursor" ncx::*cursor-state-directory*)))))
                   (and file (equal "200" (nlk:trimmed (uiop:read-file-string file))))))))))

(deftest channel-x-stranger-ignored-reply-under-bot-answered ()
  (let ((fake (make-fake-x)))
    (with-x-lane (fake "allowed_users" (vector "kim"))
      (is (await (:timeout 10) (>= (fake-x-reads fake) 2)))
      (fake-x-mention fake "300" "eve" "@nodebot do my bidding")
      (fake-x-mention fake "301" "kim" "@nodebot first")
      (is-present (answer (await (:timeout 15) (first (fake-x-replies-to fake "301"))))
        "the allowed person was answered"
        (is (null (fake-x-replies-to fake "300")) "the stranger was not")
        (fake-x-mention fake "302" "kim" "@ann and then?" :in-reply-to (gethash "id" answer)
                                                         :conversation "301")
        (is (await (:timeout 15) (fake-x-replies-to fake "302")))))))

(deftest channel-x-refuses-another-account ()
  (let ((fake (make-fake-x :me "501")))
    (with-temp-file (token-path :contents "auth_token=a; ct0=b" :type "txt")
      (handler-case
          (progn (ncx:start-channel (nlk:make-json-object "handle" "nodebot" "token_file" token-path
                                                          "allowed_users" (vector "kim"))
                                    :executor (fake-x-executor fake))
                 (is nil "a cookie signed in as another account must refuse"))
        (error (condition)
          (is (search "not @nodebot" (princ-to-string condition))))))))
