;;;; cell-test.lisp --- the lifecycle; the answer's shape; the filters; the
;;;; two tables that need no Windows box; the refusals; /ports.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own work: that the tool answers the
;;;; fields the original plugin answered, that every filter narrows the table
;;;; the way it did, that /proc is parsed and joined to its PIDs from real
;;;; files and real symlinks, that lsof's field output is read the same way,
;;;; that a platform with no table and a call outside the contract each
;;;; refuse in words, and that /ports prints lines or says nothing matched.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "ports"
  (:help :ports)
  (:command "ports")
  (:running (is (= 100 (nodecode-ports::setting :limit)) "one answer carries a hundred endpoints")
            (is (eq t (nodecode-ports::setting :show-paths)) "image paths are on by default"))
  (:refused ("limit" 0))
  (:idle ports:ports-error (ports:owners 3000)))

;;; --- the answer --------------------------------------------------------------------

(deftest ports-cell-the-answer-keeps-the-originals-fields ()
  (with-table (list (port-row :port 3000 :pid 4242 :process "gateway" :state "listen")
                    (port-row :port 5432 :pid 5150 :process "postgres" :state "listen"
                              :path "/usr/lib/postgres")
                    (port-row :port 8086 :pid 4242 :process "gateway" :state "established"))
    (let ((answer (ports-call "{}")))
      (is (eq t (nlk:json-value answer :boolean "ok")) "ok is true")
      (is (= 3 (nlk:json-value answer :integer "matched")) "every row matched")
      (is (= 2 (nlk:json-value answer :integer "listeners")) "two of them listen")
      (is (= 3 (nlk:json-value answer :integer "total_endpoints")) "the table held three")
      (is (equal '("gateway" "postgres" "gateway") (entry-strings answer "process")) "each entry names its process")
      (is (equal '("/usr/local/bin/node" "/usr/lib/postgres" "/usr/local/bin/node")
                 (entry-strings answer "path")) "and its image path")
      (is (= 2 (nlk:json-value (nlk:json-value answer :object "by_process") :integer "gateway")) "by_process counts the two endpoints of one image")
      (is (search "3 endpoints matched out of 3"
                  (nlk:json-value (aref (nlk:json-value answer :array "notes") 0) :string)) "the note says what happened"))))

(deftest ports-cell-the-filters-narrow-the-table ()
  (with-table (list (port-row :port 3000 :state "listen" :process "node")
                    (port-row :port 5432 :state "listen" :process "postgres")
                    (port-row :port 5432 :state "established" :process "node"
                              :remote-address "10.0.0.9" :remote-port 5432))
    (is (equal '(3000) (entry-ports (ports-call "{\"port\": 3000}"))) "one port")
    (is (equal '(3000) (entry-ports (ports-call "{\"ports\": [3000, 9999]}"))) "several ports, the absent one dropped")
    (is (equal '(5432) (entry-ports (ports-call "{\"ports\": [3000, 5432], \"process\": \"postgres\"}"))) "a port list and a process are one filter")
    (is (null (entry-ports (ports-call "{\"port\": 3000, \"process\": \"postgres\"}"))) "a port and a process that disagree match nothing")
    (is (= 2 (length (entry-ports (ports-call "{\"state\": \"listen\"}")))) "listeners only")
    (is (equal '(5432) (entry-ports (ports-call "{\"state\": \"established\"}"))) "the live connection only")
    (is (= 1 (length (entry-ports (ports-call "{\"limit\": 1}")))) "the limit cuts the answer")
    (is (search "stopped at limit=1"
                (nlk:json-value (aref (nlk:json-value (ports-call "{\"limit\": 1}") :array "notes") 1)
                                :string)) "and says it did")
    (is (find "no endpoint matched" (answer-notes (ports-call "{\"port\": 1}"))
              :test (lambda (needle text) (and text (search needle text)))) "an empty answer says how to widen it")))

(deftest ports-cell-the-limit-setting-is-the-default-a-call-may-override ()
  (with-table (list (port-row :port 3000) (port-row :port 3001) (port-row :port 3002))
    (is (= 2 (nlk:json-value (ports-call "{}" "limit" 2) :integer "matched")) "the section's limit answers a call that names none")
    (is (= 3 (nlk:json-value (ports-call "{\"limit\": 3}" "limit" 2) :integer "matched")) "a call may raise it")))

;;; --- the tables that need no Windows box --------------------------------------------

(deftest ports-cell-the-linux-table-comes-out-of-a-proc-tree ()
  (with-proc-fixture (root)
    (let* ((nodecode-ports:*platform* :linux)
           (nodecode-ports:*proc-root* root)
           (table (nodecode-ports:rows)))
      (is (= 4 (length table)) "two TCP rows, one IPv6 row, one UDP row")
      (flet ((row (port) (find port table :key (lambda (row) (getf row :port)))))
        (let ((listener (row 3000)))
          (is (equal "127.0.0.1" (getf listener :address)) "the address /proc writes little-endian")
          (is (equal "listen" (getf listener :state)) "0A is a listener")
          (is (= 4242 (getf listener :pid)) "the inode joins to the process that holds it")
          (is (equal "node" (getf listener :process)) "its image, from /proc/<pid>/comm")
          (is (equal "/usr/local/bin/node" (getf listener :path)) "and its path, from the exe link"))
        (let ((live (row 3001)))
          (is (equal "established" (getf live :state)) "01 is established")
          (is (= 5150 (getf live :pid)) "held by the other process")
          (is (= 3000 (getf live :remote-port)) "with the peer's port"))
        (let ((v6 (row 8080)))
          (is (equal "tcp6" (getf v6 :protocol)) "the IPv6 table keeps its own protocol name")
          (is (equal "0000:0000:0000:0000:0000:0000:0000:0001" (getf v6 :address)) "thirty-two hex digits read as four reversed words"))
        (let ((udp (row 6379)))
          (is (equal "bound" (getf udp :state)) "a UDP endpoint is bound, not listening")
          (is (= 5150 (getf udp :pid)) "and joins to its owner as a TCP row does"))))))

(deftest ports-cell-the-mac-table-comes-out-of-lsof ()
  (let ((nodecode-ports:*platform* :darwin)
        (nodecode-ports:*run*
          (lambda (argv)
            (declare (ignore argv))
            (format nil "p4242~%cnode~%PTCP~%n127.0.0.1:3000~%TST=LISTEN~%~
n127.0.0.1:54321->127.0.0.1:3000~%TST=ESTABLISHED~%~
p5150~%cgateway~%PUDP~%n*:6379~%"))))
    (let* ((table (nodecode-ports:rows))
           (listener (find 3000 table :key (lambda (row) (getf row :port)))))
      (is (= 3 (length table)) "two TCP endpoints and one UDP endpoint")
      (is (equal "4242" (princ-to-string (getf listener :pid))) "lsof's p field is the PID")
      (is (equal "node" (getf listener :process)) "its c field the image")
      (is (equal "listen" (getf listener :state)) "and the T field that follows the name settles the state")
      (let ((live (find 54321 table :key (lambda (row) (getf row :port)))))
        (is (equal "127.0.0.1" (getf live :remote-address)) "the arrow splits local from remote")
        (is (= 3000 (getf live :remote-port))))
      (let ((udp (find 6379 table :key (lambda (row) (getf row :port)))))
        (is (equal "0.0.0.0" (getf udp :address)) "lsof's wildcard is the unspecified address")
        (is (equal "udp" (getf udp :protocol)))))))

;;; --- the refusals ------------------------------------------------------------------

(deftest ports-cell-a-platform-with-no-table-refuses ()
  (let ((nodecode-ports:*platform* :beos)
        (nodecode-ports:*row-source* nil))
    (let ((answer (ports-call "{}")))
      (is (null (nlk:json-value answer :boolean "ok")) "the answer says it is not ok")
      (is (search "no port table" (nlk:json-value answer :string "error")) "and names the three platforms it reads"))))

(deftest ports-cell-a-call-outside-the-contract-refuses ()
  (is (handler-case (progn (nlk:contract-decode (nlk:contract 'nodecode-ports::port-lookup)
                                                  (nlk:make-json-object "port" "3000"))
                           nil)
        (nlk:contract-refusal () t)) "a port as a string is refused, never coerced"))

;;; --- /ports ------------------------------------------------------------------------

(deftest ports-cell-the-command-prints-lines-or-says-nothing-matched ()
  (with-table (list (port-row :port 3000 :state "listen" :process "node" :pid 4242))
    (with-ports ()
      (let ((text (nodecode-ports::slash-ports "3000" nil)))
        (is (search "127.0.0.1:3000" text) "the endpoint is on the line")
        (is (search "node" text) "with the process that holds it")
        (is (search "4242" text) "and its PID"))
      (let ((text (nodecode-ports::slash-ports "nothing-holds-this" nil)))
        (is (search "nothing matched" text) "an argument nothing holds says so"))
      (let ((text (nodecode-ports::slash-ports "listen" nil)))
        (is (search "127.0.0.1:3000" text) "the word listen filters to the listeners")))))
