;;;; support.lisp --- terminal test runner, and a page's socket by hand.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Terminal tests register into the shared nodecode.test registry under the
;;;; TERMINAL-CELL- prefix; RUN-TERMINAL-TESTS runs exactly that slice. A test
;;;; shell is bash without rc files and with a one-character prompt, so what a
;;;; test reads is what the shell printed; each test looks for text the shell
;;;; computes (echo $((6*7)) for 42), never for its own echoed typing.

(in-package #:nodecode.test)

(define-test-slice "terminal" "TERMINAL-CELL-" :start nodecode-terminal::start-cell)

(defparameter +test-shell+ '("/bin/bash" "--norc" "--noprofile" "-i")
  "The shell every test terminal runs.")

(defun test-environment ()
  (append (nodecode-terminal::shell-environment) '("PS1=$ " "HISTFILE=/dev/null")))

(defun octets-text (octets)
  (sb-ext:octets-to-string (coerce octets '(simple-array (unsigned-byte 8) (*)))
                           :external-format '(:utf-8 :replacement #\?)))

(defstruct (watched (:constructor make-watched (terminal)))
  terminal
  (bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (exit nil)
  (lock (sb-thread:make-mutex :name "watched")))

(defun watch (terminal)
  "Attach to TERMINAL, keeping everything it prints => the watch."
  (let* ((watched (make-watched terminal))
         (backlog (nodecode-terminal:terminal-attach
                   terminal (lambda (kind data)
                              (sb-thread:with-mutex ((watched-lock watched))
                                (if (eq kind :output)
                                    (loop for octet across data do (vector-push-extend octet (watched-bytes watched)))
                                    (setf (watched-exit watched) data)))))))
    (sb-thread:with-mutex ((watched-lock watched))
      (let ((later (copy-seq (watched-bytes watched))))
        (setf (fill-pointer (watched-bytes watched)) 0)
        (loop for octet across backlog do (vector-push-extend octet (watched-bytes watched)))
        (loop for octet across later do (vector-push-extend octet (watched-bytes watched)))))
    watched))

(defun watched-text (watched)
  (sb-thread:with-mutex ((watched-lock watched)) (octets-text (watched-bytes watched))))

(defun await-text (watched needle &key (timeout 5))
  "NEEDLE appears in what WATCHED printed within TIMEOUT seconds."
  (loop repeat (* timeout 50)
        when (search needle (watched-text watched)) return t
        do (sleep 0.02)))

(defun await-exit (watched &key (timeout 5))
  (loop repeat (* timeout 50)
        when (watched-exit watched) return (watched-exit watched)
        do (sleep 0.02)))

(defun type-line (terminal line)
  (nodecode-terminal:terminal-write terminal (format nil "~a~c" line #\Return)))

(defmacro with-test-terminal ((watched &rest open-args) &body body)
  "BODY with WATCHED watching a fresh test terminal, closed after."
  (let ((terminal (gensym "TERMINAL")))
    `(let* ((,terminal (nodecode-terminal:open-terminal :command +test-shell+ :environment (test-environment)
                                                        ,@open-args))
            (,watched (watch ,terminal)))
       (unwind-protect (progn (await-text ,watched "$ " :timeout 5) ,@body)
         (nodecode-terminal:close-terminal ,terminal)))))

(defun pid-alive-p (pid)
  (handler-case (progn (sb-posix:kill pid 0) t) (error () nil)))

;;; --- a page's socket ---------------------------------------------------------

(defstruct (client (:constructor make-client (ws inbox)))
  ws inbox (bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))

(defun open-client (port &key origin)
  "A WebSocket to PORT's /api/terminal/socket, as a page opens it => the client."
  (let* ((inbox (sb-concurrency:make-mailbox :name "terminal client"))
         (ws (wsd:make-client (format nil "ws://127.0.0.1:~d/api/terminal/socket" port)
                              :additional-headers (when origin (list (cons "Origin" origin)))))
         (client (make-client ws inbox)))
    (wsd:on :message ws (lambda (message)
                          (if (stringp message)
                              (sb-concurrency:send-message inbox (nlk:decode-json message))
                              (sb-thread:with-mutex ((load-time-value (sb-thread:make-mutex :name "client bytes")))
                                (loop for octet across message do (vector-push-extend octet (client-bytes client)))))))
    (wsd:on :close ws (lambda (&key &allow-other-keys) (sb-concurrency:send-message inbox :closed)))
    (wsd:start-connection ws)
    client))

(defun client-send (client &rest pairs)
  (wsd:send-text (client-ws client)
                 (sb-ext:octets-to-string (nlk:encode-json-octets (apply #'nlk:make-json-object pairs))
                                          :external-format :utf-8)))

(defun client-next (client &key (type nil) (timeout 5))
  "The next frame CLIENT received (of TYPE when given), :CLOSED once the
server has closed the socket (its close frame read: the client is no longer
:OPEN), or NIL after TIMEOUT seconds."
  (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        for left = (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)
        while (plusp left)
        do (let ((frame (sb-concurrency:receive-message (client-inbox client) :timeout (min left 0.05))))
             (cond ((eq frame :closed) (return :closed))
                   ((hash-table-p frame)
                    (when (or (null type) (equal type (gethash "type" frame))) (return frame)))
                   ((and (sb-concurrency:mailbox-empty-p (client-inbox client))
                         (not (eq :open (wsd:ready-state (client-ws client)))))
                    (return :closed))))))

(defun client-text (client)
  (octets-text (client-bytes client)))

(defun client-await-text (client needle &key (timeout 5))
  (loop repeat (* timeout 50)
        when (search needle (client-text client)) return t
        do (sleep 0.02)))

(defun close-client (client)
  (ignore-errors (wsd:close-connection (client-ws client))))

(defmacro with-test-shell-for-sockets (&body body)
  "BODY with every terminal a socket opens running the test shell."
  `(with-saved-globals ((nodecode-terminal::*command* +test-shell+)
                        (nodecode-terminal::*environment* (test-environment)))
     ,@body))
