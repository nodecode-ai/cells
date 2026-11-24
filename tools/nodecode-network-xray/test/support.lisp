;;;; support.lisp --- network-xray test runner, the stubbed ladder, a scratch
;;;; records directory.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a
;;;; NETWORK-XRAY-CELL- name prefix; RUN-NETWORK-XRAY-TESTS runs that slice.
;;;;
;;;; What is stubbed is the network, never the cell: the four stage
;;;; functions answer evidence in place of a socket, a handshake and a HEAD,
;;;; so the ladder, the address the probe selected, the sealing, the store
;;;; and the comparison are all the cell's own code.

(in-package #:nodecode.test)

(define-test-slice "network-xray" "NETWORK-XRAY-CELL-" :start nodecode-network-xray:start-cell)

(defun xray-fresh-directory ()
  "A scratch directory no other test shares."
  (merge-pathnames (format nil "xray-cell-~a/" (random most-positive-fixnum))
                   (uiop:temporary-directory)))

(defmacro with-xray ((&rest pairs) &body body)
  "BODY with the cell started on the `network-xray' section: PAIRS, plus a
vantage label and a scratch records directory when they name none."
  (let ((directory (gensym "DIRECTORY")))
    `(let ((,directory (xray-fresh-directory)))
       (unwind-protect
            (with-cell-stop ((network-xray-start
                              ,@(unless (member "vantage" pairs :test #'string=)
                                  (list "vantage" "test-vantage"))
                              ,@(unless (member "directory" pairs :test #'string=)
                                  (list "directory" `(namestring ,directory)))
                              ,@pairs))
              ,@body)
         (ignore-errors (uiop:delete-directory-tree ,directory :validate t))))))

(defmacro with-ladder ((&key dns (code 200) refuse) &body body)
  "BODY with the four stages stubbed: DNS answers ADDRESSES, the connector
refuses when REFUSE, the handshake verifies, and the HEAD answers CODE."
  `(let ((nodecode-network-xray:*resolver* (xray-dns (or ,dns '("93.184.216.34"))))
         (nodecode-network-xray:*connector* (xray-connector :refuse ,refuse))
         (nodecode-network-xray:*handshaker* (xray-tls))
         (nodecode-network-xray:*head* (xray-head ,code))
         (nodecode-network-xray:*closer* (lambda (socket) (declare (ignore socket)) nil)))
     ,@body))

(defun xray-dns (addresses &optional truncated)
  "A resolver answering ADDRESSES."
  (lambda (target)
    (declare (ignore target))
    (values addresses truncated)))

(defun xray-connector (&key refuse)
  "A connector that answers a stand-in socket, or refuses."
  (lambda (target address timeout)
    (declare (ignore target timeout))
    (when refuse (error 'usocket:connection-refused-error))
    (list :socket address)))

(defun xray-tls (&key (protocol "TLSv1.3") (issuer "Test CA"))
  "A handshake that verifies against a stand-in stream."
  (lambda (target socket timeout)
    (declare (ignore socket timeout))
    (values :stream (nlk:make-json-object "protocol" protocol "issuer" issuer
                                          "server_name" (nlk:json-value target :string "host")
                                          "verified" t))))

(defun xray-head (code &key (server "test-server") (type "text/html"))
  "A HEAD answering CODE."
  (lambda (target stream timeout)
    (declare (ignore target stream timeout))
    (values (format nil "HTTP ~d" code)
            (nlk:make-json-object "status_code" code "reason" "Something"
                                   "server" server "content_type" type
                                   "redirect" (<= 300 code 399)))))

(defun xray-tool-text (tool arguments)
  "TOOL's answer as the text the wire carries, ARGUMENTS its JSON body."
  (princ-to-string
   (nle::message-content (execute-wire-call "call-1" tool arguments))))

(defun xray-tool (tool arguments)
  "TOOL called through the wire as a provider sends it, ARGUMENTS its JSON
body, the cell already running => the decoded answer."
  (nlk:decode-json
   (princ-to-string
    (nle::message-content (execute-wire-call "call-1" tool arguments)))))

(defun xray-arguments (&rest pairs)
  "PAIRS as the JSON body of a call."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun xray-probe-record (&rest pairs)
  "A probe of the fixed URL, the cell already running, stubs bound by the
caller => (values ANSWER RECORD BUNDLE)."
  (let* ((answer (xray-tool "network_xray_probe"
                             (apply #'xray-arguments "url" "https://example.test/a" pairs)))
         (record (nlk:json-value answer :object "result")))
    (values answer record (nlk:json-value record :object "probe"))))

(defun xray-items (object key)
  "KEY's array in OBJECT, as a list."
  (coerce (or (nlk:json-value object :array key) #()) 'list))

(defun xray-finding-kinds (comparison)
  "The kinds the comparison's findings carry."
  (remove-duplicates (mapcar (lambda (finding) (nlk:json-value finding :string "kind"))
                              (xray-items comparison "findings"))
                     :test #'equal))

(defun xray-stage (bundle index)
  "BUNDLE's stage INDEX."
  (aref (nlk:json-value bundle :array "stages") index))
