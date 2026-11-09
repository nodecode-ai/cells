;;;; support.lisp --- the test slice: a stubbed Yandex Disk.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; YADISK-CELL- name prefix; RUN-YANDEX-DISK-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-YANDEX-DISK::HTTP, the
;;;; cell's only way to the network. WITH-WIRE replaces it with a canned disk
;;;; and records every call, so a test asserts on the request and nothing
;;;; ever dials.

(in-package #:nodecode.test)

(define-test-slice "yandex-disk" "YANDEX-DISK-CELL-" :start nodecode-yandex-disk:start-cell)

;;; --- the canned disk --------------------------------------------------------------

(defparameter +token+ "yandex-disk-test-token"
  "The OAuth token a test's section carries.")

(defparameter +base+ "https://cloud-api.yandex.net/v1/disk"
  "Where the section sends its calls unless a test says otherwise.")

(defvar *calls* '()
  "Every request the stub took, newest first: (:method :url :headers :content).")

(defvar *routes* '()
  "The canned answers as (NEEDLE . FUNCTION), the first whose NEEDLE is a
substring of the URL answering it with (values BODY STATUS).")

(defvar *written* '()
  "The bin items the stub was asked to destroy.")

(defun junk (text)
  "TEXT as JSON, proof against the quotes it carries."
  (with-output-to-string (out)
    (loop for character across text
          do (case character
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (t (write-char character out))))))

(defun resource-json (path &key (type "file") (size 42) (modified "2024-05-01T10:00:00+00:00")
                                 name md5 public-url origin-path)
  "One resource as Yandex would answer it."
  (format nil "{\"path\":\"~a\",\"type\":\"~a\"~@[,\"size\":~d~]~@[,\"name\":\"~a\"~]~
~@[,\"modified\":\"~a\"~]~@[,\"md5\":\"~a\"~]~@[,\"public_url\":\"~a\"~]~
~@[,\"origin_path\":\"~a\"~]}"
          path type (and (equal type "file") size)
          (junk (or name (car (last (uiop:split-string path :separator "/")))))
          modified md5 public-url origin-path))

(defun folder-json (path children &key (total (length children)))
  "A folder with CHILDREN embedded, as Yandex would answer it."
  (format nil "{\"path\":\"~a\",\"type\":\"dir\",\"name\":\"~a\",\"_embedded\":{\"total\":~d,\"items\":[~{~a~^,~}]}}"
          path (car (last (uiop:split-string path :separator "/"))) total children))

(defun items-json (items)
  (format nil "{\"_embedded\":{\"items\":[~{~a~^,~}]}}" items))

(defun hits-json (items)
  "A search answer: the hits at the top level, as the endpoint ranks them."
  (format nil "{\"items\":[~{~a~^,~}]}" items))

(defun answer (body &optional (status 200))
  (lambda (&rest ignored)
           (declare (ignore ignored)) (values body status)))

(defun canned-disk ()
  "The disk a test starts with: one folder, one file inside it, one bin item,
one upload and one download link."
  (list
   (cons "=https://cloud-api.yandex.net/v1/disk/"
         (lambda (&rest ignored)
           (declare (ignore ignored))
           (values (format nil "{\"total_space\":2000000,\"used_space\":500000,\"trash_size\":1024,~
\"max_file_size\":900000000,\"is_paid\":false,\"user\":{\"login\":\"someone\"}}")
                   200)))
   (cons "/resources/upload"
         (lambda (&rest ignored)
           (declare (ignore ignored))
           (values "{\"href\":\"https://up.example/one\",\"operation_id\":\"op1\"}" 200)))
   (cons "https://up.example/"
         (lambda (&rest ignored)
           (declare (ignore ignored)) (values "" 201)))
   (cons "/operations/op1"
         (lambda (&rest ignored)
           (declare (ignore ignored)) (values "{\"status\":\"success\"}" 200)))
   (cons "/resources/download"
         (lambda (&rest ignored)
           (declare (ignore ignored))
           (values "{\"href\":\"https://down.example/one\"}" 200)))
   (cons "https://down.example/"
         (lambda (&rest ignored)
           (declare (ignore ignored))
           (values (format nil "{\"hello\":true,\"letters\":\"abc\"}") 200)))
   (cons "/resources/search"
         (lambda (&rest ignored)
           (declare (ignore ignored))
           (values (hits-json
                    (list (resource-json "disk:/Documents/match.txt" :modified "2024-05-02T09:00:00+00:00")
                          (resource-json "disk:/Other/away.txt")))
                   200)))
   (cons "/resources/copy"
         (lambda (&rest ignored)
           (declare (ignore ignored)) (values "{\"status\":\"success\"}" 200)))
   (cons "/resources/move"
         (lambda (&rest ignored)
           (declare (ignore ignored)) (values "{\"status\":\"success\"}" 200)))
   (cons "/resources/publish"
         (lambda (&rest ignored)
           (declare (ignore ignored)) (values "{}" 200)))
   (cons "/resources/unpublish"
         (lambda (&rest ignored)
           (declare (ignore ignored)) (values "{}" 200)))
   (cons "/trash/resources/restore"
         (lambda (&rest ignored)
           (declare (ignore ignored)) (values "{\"status\":\"success\"}" 200)))
   (cons "/trash/resources"
         (lambda (method url)
           (declare (ignore url))
           (if (eq method :delete)
               (progn (push "trash" *written*) (values "" 200))
               (values (items-json
                        (list (resource-json "trash:/Documents/match.txt" :origin-path "disk:/Documents/match.txt")))
                       200))))
   (cons "/resources"
         (lambda (method url)
           (cond
             ((eq method :put) (values "{}" 200))
             ((cl:search "/resources/download" url) (values "{\"href\":\"https://down.example/one\"}" 200))
             (t (values (folder-json
                         "disk:/Documents"
                         (list (resource-json "disk:/Documents/report.pdf" :size 2048
                                              :modified "2024-05-01T10:00:00+00:00"
                                              :md5 "0123456789abcdef")
                               (resource-json "disk:/Documents/notes" :type "dir" :size 0)
                               (resource-json "disk:/Documents/readme.txt" :size 12))
                         :total 9)
                        200)))))))

(defun stub-http (method url &key headers content timeout binary max-bytes)
  (declare (ignore timeout binary max-bytes))
  (push (list :method method :url url :headers headers :content content) *calls*)
  (loop for (needle . function) in *routes*
        when (if (and (plusp (length needle)) (char= #\= (char needle 0)))
                 (string= url (subseq needle 1))
                 (cl:search needle url))
          return (funcall function method url)
        finally (values "{}" 404)))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((*calls* '())
         (*routes* (canned-disk))
         (*written* '()))
     (with-stubbed-fdefinition (nodecode-yandex-disk::http (method url &key headers content timeout binary max-bytes)
                                (stub-http method url :headers headers :content content
                                                 :timeout timeout :binary binary :max-bytes max-bytes))
       ,@body)))

;;; --- starting it, calling it ---------------------------------------------------------

(defun yandex-disk-run-start (&rest pairs)
  "START-CELL with a token unless PAIRS name one."
  (apply #'yandex-disk-start
         (append (unless (member "token" pairs :test #'equal) (list "token" +token+))
                 (copy-list pairs))))

(defmacro with-yandex-disk ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((yandex-disk-run-start ,@pairs))
       ,@body)))

(defun call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- reading the request a call made --------------------------------------------------

(defun last-call ()
  (first *calls*))

(defun calls-to (needle)
  (remove-if-not (lambda (call) (cl:search needle (getf call :url))) *calls*))

(defun query-of (call)
  "CALL's query string as an alist of decoded pairs."
  (let* ((url (getf call :url))
         (mark (position #\? url)))
    (if (null mark)
        '()
        (loop for pair in (uiop:split-string (subseq url (1+ mark)) :separator "&")
              for equals = (position #\= pair)
              collect (cons (subseq pair 0 equals)
                            (quri:url-decode (subseq pair (1+ equals))))))))

(defun query-param (call name)
  (cdr (assoc name (query-of call) :test #'string=)))

(defun header-of (call name)
  (cdr (assoc name (getf call :headers) :test #'string-equal)))
