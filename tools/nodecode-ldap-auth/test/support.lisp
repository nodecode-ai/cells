;;;; support.lisp --- the ldap-auth test runner and the scripted directory.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core DEFTEST,
;;;; with its hermetic machine-state posture) under an LDAP-AUTH-CELL- name
;;;; prefix; RUN-LDAP-AUTH-TESTS runs exactly that slice. Nothing here dials
;;;; anything: the cell's one seam, *OPENER*, is bound to a scripted directory
;;;; that answers canned LDAP replies and records what was written to it, so
;;;; what went on the wire and what a caller reads are both facts a test can
;;;; assert on.

(in-package #:nodecode.test)

(define-test-slice "ldap-auth" "LDAP-AUTH-CELL-" :start nodecode-ldap-auth:start-cell)

(defparameter +url+ "ldaps://ldap.example.com"
  "A directory a test points the cell at: nothing dials it.")

(defparameter +direct+ "uid={username},ou=people,dc=example,dc=com"
  "A direct-bind DN template.")

(defparameter +base+ "ou=people,dc=example,dc=com"
  "A search-then-bind subtree.")

(defparameter +group+ "cn=nodecode-users,ou=groups,dc=example,dc=com"
  "A group DN a login may be restricted to.")

(defparameter +user-dn+ "uid=jdoe,ou=people,dc=example,dc=com"
  "The DN the scripted directory knows jdoe by.")

(defparameter +secret+ "0123456789abcdef0123456789abcdef"
  "A signing key, written as hex, so a token can be checked across restarts.")

(defvar *sent* (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
  "What the cell wrote to the scripted directory, in order.")

(defmacro with-directory ((&rest replies) &body body)
  "BODY with the cell dialling a scripted directory answering REPLIES, in order:
(bind-response 1 0), (search-entry 2 DN ATTRIBUTES), (search-done 2 0)."
  `(let* ((replies (list ,@replies))
          (*sent* (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
          (nodecode-ldap-auth::*opener*
            (lambda (settings)
              (declare (ignore settings))
              (nodecode-ldap-auth::make-transport
               (lambda (octets)
                 (loop for byte across octets do (vector-push-extend byte *sent*)))
               (lambda ()
                 (or (pop replies)
                     (nodecode-ldap-auth::fail "the scripted directory ran out of answers")))))))
     ,@body))

(defun sent-text ()
  "What the cell wrote to the scripted directory, as text (it is UTF-8)."
  (map 'string #'code-char *sent*))

(defun ldap-tool (id name arguments)
  "NAME called as the provider's call calls it: the answer's text."
  (nle::message-content (execute-wire-call id name arguments)))

(defun ldap-answer (arguments)
  (ldap-tool "ldap-call" "ldap_login" arguments))

(defun token-in (answer)
  "The access token in a login ANSWER, or NIL."
  (let ((start (search "access token: " answer)))
    (when start
      (let* ((from (+ start (length "access token: ")))
             (end (or (position #\Newline answer :start from) (length answer))))
        (string-trim " " (subseq answer from end))))))

(defun ldap-login-json (username password)
  (format nil "{\"username\": \"~a\", \"password\": \"~a\"}" username password))
