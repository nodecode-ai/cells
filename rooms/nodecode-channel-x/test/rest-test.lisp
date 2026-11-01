;;;; rest-test.lisp --- X plans and the executor: replies only, chained, capped.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun x-recording-bridge (&key (fail-replies nil))
  "(values EXECUTOR SENT): a stand-in for the bridge child answering every
reply with a fresh id (or refusing it, FAIL-REPLIES), SENT a thunk returning
the reply bodies it saw, oldest first."
  (let ((sent '()) (next 1000))
    (values (nck:make-plan-executor
             :run (lambda (plan)
                    (let ((body (nck:request-plan-body plan)))
                      (push body sent)
                      (if fail-replies
                          (nck:make-execution :ok-p nil :status 403 :error "refused")
                          (nck:make-execution :ok-p t :status 200 :attempts 1
                                              :body (nlk:make-json-object "ok" t "id" (princ-to-string (incf next))))))))
            (lambda () (reverse sent)))))

(defun x-chunk (text index total)
  (nck::make-text-chunk :text text :index index :total total))

(deftest channel-x-answer-parts-chain-under-the-ask ()
  (multiple-value-bind (inner sent) (x-recording-bridge)
    (let* ((book (ncx:make-x-book :max-posts 3))
           (bot (ncx:wrap-x-executor inner :book book)))
      (loop for index from 1 to 5
            do (nck:execute-plan bot (ncx:message-plan book nil (x-chunk (format nil "**part ~d**" index) index 5)
                                                       :reply-to "200")))
      (let ((posts (funcall sent)))
        (is (= 3 (length posts)) "parts past max_posts are dropped")
        (is (equal '("200" "1001" "1002") (mapcar (lambda (post) (gethash "reply_to" post)) posts)))
        (is (equal '("part 1 1/3" "part 2 2/3" "part 3 3/3 …") (mapcar (lambda (post) (gethash "text" post)) posts)))
        (is (notany (lambda (post) (nth-value 1 (gethash "__chain" post))) posts))))))

(deftest channel-x-never-tweets-on-its-own ()
  (multiple-value-bind (inner sent) (x-recording-bridge)
    (let* ((book (ncx:make-x-book))
           (result (nck:execute-plan (ncx:wrap-x-executor inner :book book)
                                     (ncx:message-plan book nil (x-chunk "hello world" 1 1)))))
      (is (not (nck:execution-ok-p result)))
      (is (search "never tweets on its own" (nck:execution-error result)))
      (is (null (funcall sent)) "nothing reached X"))))

(deftest channel-x-card-is-never-posted ()
  (multiple-value-bind (inner sent) (x-recording-bridge)
    (let* ((book (ncx:make-x-book))
           (result (nck:execute-plan (ncx:wrap-x-executor inner :book book)
                                     (ncx:message-plan book nil (x-chunk "Thinking" 1 1) :reply-to "200"
                                                       :card '(:state :working)))))
      (is (nck:execution-ok-p result))
      (is (uiop:string-prefix-p "card-" (ncx:x-message-id (nck:execution-body result))))
      (is (null (funcall sent))))))

(deftest channel-x-daily-posts-cap ()
  (multiple-value-bind (inner sent) (x-recording-bridge)
    (let* ((book (ncx:make-x-book :daily-posts 2))
           (bot (ncx:wrap-x-executor inner :book book))
           (results (loop repeat 3
                          collect (nck:execute-plan bot (ncx:message-plan book nil (x-chunk "hi" 1 1)
                                                                          :reply-to "200")))))
      (is (equal '(t t nil) (mapcar #'nck:execution-ok-p results)))
      (is (= 429 (nck:execution-status (third results))))
      (is (= 2 (length (funcall sent))) "the third post never reached X"))))

(deftest channel-x-refused-post-gives-its-count-back ()
  (multiple-value-bind (inner sent) (x-recording-bridge :fail-replies t)
    (declare (ignore sent))
    (let* ((book (ncx:make-x-book :daily-posts 1))
           (bot (ncx:wrap-x-executor inner :book book)))
      (nck:execute-plan bot (ncx:message-plan book nil (x-chunk "hi" 1 1) :reply-to "200"))
      (is (= 403 (nck:execution-status (nck:execute-plan bot (ncx:message-plan book nil (x-chunk "hi" 1 1)
                                                                                :reply-to "200"))))))))

(deftest channel-x-deletes-only-its-own ()
  (multiple-value-bind (inner sent) (x-recording-bridge)
    (let* ((book (ncx:make-x-book))
           (bot (ncx:wrap-x-executor inner :book book)))
      (nck:execute-plan bot (ncx:message-plan book nil (x-chunk "hi" 1 1) :reply-to "200"))
      (is (not (nck:execution-ok-p (nck:execute-plan bot (ncx:delete-plan book nil "200")))))
      (is (nck:execution-ok-p (nck:execute-plan bot (ncx:delete-plan book nil "1001"))))
      (is (equal "delete" (gethash "op" (car (last (funcall sent)))))))))
