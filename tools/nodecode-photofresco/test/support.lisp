;;;; support.lisp --- the test slice and the scripted browser the tools talk to.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No browser and no socket here: a LINK's two deeds are slots, so a test
;;;; hands the cell a link that answers out of a script and enqueues the
;;;; editor's own message the moment the page call goes out. Every frame
;;;; below is the shape the real channel carries, so what the tests prove is
;;;; the cell's reading of the protocol, not a shortcut beside it.

(in-package #:nodecode.test)

(define-test-slice "photofresco" "PHOTOFRESCO-CELL-" :start nodecode-photofresco:start-cell)

(defun args (&rest keys-and-values)
  "The decoded-arguments table a tool body is handed."
  (apply #'nodecode-photofresco::json-object keys-and-values))

(defun scripted-link (&key answers artifact)
  "A link that answers METHOD from ANSWERS: an alist of method to
(:OK VALUE) or (:REFUSE CODE MESSAGE). ARTIFACT is the result an
export.artifact answers with, and its retrieval is stubbed to five bytes."
  (let* ((link (nodecode-photofresco::make-link))
         (queue '()))
    (setf (nodecode-photofresco::link-key link) (make-string 64 :initial-element #\a))
    (setf (nodecode-photofresco::link-context-id link) 7)
    (setf (nodecode-photofresco::link-cdp-session link) "session-1")
    (setf (nodecode-photofresco::link-session-key link) "key-1")
    (setf (nodecode-photofresco::link-pump link) (lambda (link) (prog1 nil link)))
    (setf (nodecode-photofresco::link-send link)
          (lambda (link method params session-id)
            (when (string= method "Runtime.callFunctionOn")
              (let* ((envelope (gethash "value" (aref (gethash "arguments" params) 0)))
                     (called (gethash "method" envelope))
                     (id (gethash "id" envelope))
                     (answer (assoc called answers :test #'string=))
                     (message (nodecode-photofresco::json-object "v" 1 "id" id "type" "response")))
                (assert (gethash "session_key" envelope) (envelope)
                        "a session method carries the paired session key")
                (cond ((and answer (eq :ok (second answer)))
                       (setf (gethash "ok" message) t)
                       (setf (gethash "result" message) (third answer)))
                      ((and answer (eq :refuse (second answer)))
                       (setf (gethash "ok" message) nil)
                       (setf (gethash "error" message)
                             (nodecode-photofresco::json-object
                              "code" (third answer) "message" (fourth answer))))
                      ((and artifact (string= called "export.artifact"))
                       (setf (gethash "ok" message) t)
                       (setf (gethash "result" message) artifact))
                      ((and artifact (string= called "artifact.read"))
                       (setf (gethash "ok" message) t)
                       (setf (gethash "result" message)
                             (nodecode-photofresco::json-object
                              "artifact_id" (gethash "artifact_id" (gethash "params" envelope))
                              "offset" 0 "byte_length" 5 "eof" t
                              "sha256" (make-string 64 :initial-element #\b)
                              "data_base64" "aGVsbG8=" "length" 5)))
                      (t (setf (gethash "ok" message) t)
                         (setf (gethash "result" message)
                               (nodecode-photofresco::json-object
                                "seen" (or (gethash "op" (gethash "params" envelope)) called)
                                "request_id" (gethash "request_id" (gethash "params" envelope))))))
                (setf (nodecode-photofresco::link-queue link)
                      (append (nodecode-photofresco::link-queue link) (list message)))))
            (nodecode-photofresco::json-object
             "result" (nodecode-photofresco::json-object
                       "type" "string" "value" "accepted"))))
    link))

(defmacro with-scripted-link ((link) &body body)
  "BODY with the cell started and LINK bound as the open editor session."
  `(with-cell-stop ((photofresco-start "read_roots" (vector (namestring (uiop:getcwd)))))
     (let ((nodecode-photofresco::*link* ,link))
       ,@body)))
