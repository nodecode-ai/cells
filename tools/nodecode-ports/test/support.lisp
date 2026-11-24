;;;; support.lisp --- ports test runner, the stubbed table, a /proc fixture.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ports tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a PORTS-CELL-
;;;; name prefix; RUN-PORTS-TESTS runs exactly that slice.
;;;;
;;;; What is stubbed is the machine, never the cell: *ROW-SOURCE* answers
;;;; rows in place of a platform table, *PROC-ROOT* points the Linux
;;;; backend at a fixture tree of real files and real symlinks, and *RUN*
;;;; answers lsof's field output. Every filter, the answer's shape, the
;;;; /proc join and the lsof parse are the cell's own code.

(in-package #:nodecode.test)

(define-test-slice "ports" "PORTS-CELL-" :start nodecode-ports:start-cell)

(defmacro with-ports ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS as its `ports' section, and stopped on
unwind."
  `(with-cell-stop ((ports-start ,@pairs))
     ,@body))

(defun port-row (&rest fields)
  "One table row, every field FIELDS does not name left empty."
  (list :protocol (getf fields :protocol "tcp")
        :address (getf fields :address "127.0.0.1")
        :port (getf fields :port 3000)
        :state (getf fields :state "listen")
        :pid (getf fields :pid 4242)
        :process (getf fields :process "node")
        :path (getf fields :path "/usr/local/bin/node")
        :remote-address (getf fields :remote-address "0.0.0.0")
        :remote-port (getf fields :remote-port 0)))

(defmacro with-table (rows &body body)
  "BODY with the platform's table answering ROWS, whatever the platform."
  `(let ((nodecode-ports:*row-source*
           (lambda (protocol include-ipv6)
             (declare (ignore protocol include-ipv6))
             (copy-list ,rows))))
     ,@body))

(defun ports-call (arguments &rest pairs)
  "win_port_owners called through the wire as a provider sends it, ARGUMENTS
its JSON body, the cell started on PAIRS. => the decoded answer."
  (nlk:decode-json
   (with-cell-stop ((apply #'ports-start pairs))
     (princ-to-string
      (nle::message-content (execute-wire-call "call-1" "win_port_owners" arguments))))))

(defun answer-value (answer key)
  (nlk:json-value answer key))

(defun answer-numbers (answer key)
  "The numbers of KEY's array in ANSWER, one at a time."
  (let ((array (nlk:json-value answer :array key)))
    (loop for index from 0 below (length array)
          collect (nlk:json-value (aref array index) :integer key))))

(defun answer-notes (answer)
  "Every note of ANSWER, as strings."
  (loop for note across (nlk:json-value answer :array "notes")
        collect (nlk:json-value note :string)))

(defun entry-ports (answer)
  "The port of every entry of ANSWER."
  (loop for entry across (nlk:json-value answer :array "entries")
        collect (nlk:json-value entry :integer "port")))

(defun entry-strings (answer key)
  "KEY of every entry of ANSWER, as strings or NIL."
  (loop for entry across (nlk:json-value answer :array "entries")
        collect (nlk:json-value entry :string key)))

;;; --- a /proc tree of our own -------------------------------------------------------

(defun fixture-write (path text)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
    (write-string text out))
  path)

(defun fixture-link (path target)
  (ensure-directories-exist path)
  (ignore-errors (delete-file path))
  (sb-posix:symlink target (uiop:native-namestring path))
  path)

(defmacro with-proc-fixture ((root) &body body)
  "BODY with ROOT bound to a fresh /proc: two processes whose descriptors
hold three sockets, one TCP listener, one established IPv6 connection and one
bound UDP endpoint."
  (let ((directory (gensym "PROC")))
    `(let* ((,directory (merge-pathnames
                         (format nil "ports-proc-~a/" (random most-positive-fixnum))
                         (uiop:temporary-directory)))
            (,root (uiop:ensure-directory-pathname ,directory)))
       (unwind-protect
            (progn
              (fixture-write
               (merge-pathnames "net/tcp" ,root)
               (format nil "  sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode~%   0: 0100007F:0BB8 00000000:0000 0A 00000000:00000000 00:00000000 00000000  1000        0 12345 1 0 100 0 0 10 0~%   1: 0100007F:0BB9 0100007F:0BB8 01 00000000:00000000 00:00000000 00000000  1000        0 12346 1 0 100 0 0 10 0~%"))
              (fixture-write
               (merge-pathnames "net/tcp6" ,root)
               (format nil "  sl  local_address                         remote_address                        st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode~%   0: 00000000000000000000000001000000:1F90 00000000000000000000000000000000:0000 0A 00000000:00000000 00:00000000 00000000  1000        0 23456 1 0 100 0 0 10 0~%"))
              (fixture-write
               (merge-pathnames "net/udp" ,root)
               (format nil "  sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode~%   0: 00000000:18EB 00000000:0000 07 00000000:00000000 00:00000000 00000000  1000        0 34567 2 0 0 0 0~%"))
              (fixture-write (merge-pathnames "4242/comm" ,root) "node~%")
              (fixture-link (merge-pathnames "4242/exe" ,root) "/usr/local/bin/node")
              (fixture-link (merge-pathnames "4242/fd/7" ,root) "socket:[12345]")
              (fixture-link (merge-pathnames "4242/fd/8" ,root) "socket:[23456]")
              (fixture-write (merge-pathnames "5150/comm" ,root) "gateway~%")
              (fixture-link (merge-pathnames "5150/exe" ,root) "/usr/local/bin/gateway")
              (fixture-link (merge-pathnames "5150/fd/3" ,root) "socket:[12346]")
              (fixture-link (merge-pathnames "5150/fd/4" ,root) "socket:[34567]")
              ,@body)
         (ignore-errors (uiop:delete-directory-tree ,root :validate t))))))
