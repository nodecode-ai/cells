;;;; support.lisp --- the slice's runner and a scripted container.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every test starts the cell for real; what is stubbed is the container.
;;;; *OPEN-BROWSER* answers a BROWSER whose SEND records each command and
;;;; answers the next scripted reply, so no test needs Docker, and the
;;;; protocol the cell speaks is there to assert on.

(in-package #:nodecode.test)

(define-test-slice "jet-browser" "JET-BROWSER-CELL-"
  :start nodecode-jet-browser:start-cell
  :defaults (("image" nodecode-jet-browser::+image+)))

(defparameter *calls* '()
  "Every command the scripted container was sent, newest first.")

(defparameter *closed* nil
  "Whether the scripted browser was closed.")

(defun answer (value)
  "One answer of the container's protocol: OK, carrying VALUE."
  (nlk:json-object "ok" t "value" value))

(defun refusal (reason)
  "One answer that refused: not OK, saying REASON."
  (nlk:json-object "ok" nil "error" reason))

(defun jet-browser-browser (answers)
  "A BROWSER answering each command from ANSWERS in turn -- an answer object,
or a function of the command answering one -- recording what it was sent."
  (let ((queue (copy-list answers)))
    (nodecode-jet-browser::make-browser
     :name "scripted"
     :send (lambda (command timeout)
             (declare (ignore timeout))
             (push command *calls*)
             (let ((next (pop queue)))
               (etypecase next
                 (function (funcall next command))
                 (hash-table next))))
     :close (lambda () (setf *closed* t) 0))))

(defmacro with-scripted (answers &body body)
  "BODY with the container replaced by one answering ANSWERS, one a command."
  `(let ((*calls* '())
         (*closed* nil)
         (nodecode-jet-browser::*open-browser*
           (lambda () (jet-browser-browser ,answers))))
     ,@body))

(defun jet-browser-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as a model's
call dispatches it."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun jet-browser-sent ()
  "The ops the scripted container was sent, oldest first."
  (mapcar (lambda (command) (nlk:json-value command :string "op"))
          (reverse *calls*)))

(defun jet-browser-png ()
  "The PNG the check accepts: the format's magic, then a quiet thousand octets."
  (coerce (append (coerce nodecode-jet-browser::+png-magic+ 'list)
                  (make-list 1200 :initial-element 0))
          '(vector (unsigned-byte 8))))

(defun base64-encode (octets)
  "OCTETS as base64, padded. The test's own encoder, so what is proved of
the screenshot is the cell's decoder."
  (let ((alphabet nodecode-jet-browser::+base64+))
    (with-output-to-string (out)
      (loop for index from 0 below (length octets) by 3
            for first = (aref octets index)
            for second = (and (< (1+ index) (length octets)) (aref octets (1+ index)))
            for third = (and (< (+ 2 index) (length octets)) (aref octets (+ 2 index)))
            for triple = (+ (ash first 16) (ash (or second 0) 8) (or third 0))
            do (write-char (char alphabet (ldb (byte 6 18) triple)) out)
               (write-char (char alphabet (ldb (byte 6 12) triple)) out)
               (write-char (if second (char alphabet (ldb (byte 6 6) triple)) #\=) out)
               (write-char (if third (char alphabet (ldb (byte 6 0) triple)) #\=) out)))))

(defun jet-browser-pass (&key (title nodecode-jet-browser::+title+)
                              (typed nodecode-jet-browser::+typed+))
  "The answers a healthy container gives, one per command the check sends:
create, navigate, document_state, snapshot, input, evaluate, screenshot, close."
  (list (answer (nlk:json-object))
        (answer (nlk:json-object))
        (answer (nlk:json-object "url" nodecode-jet-browser::+target+
                                  "readyState" "complete"))
        (answer (nlk:json-object "title" title))
        (answer (nlk:json-object))
        (answer (nlk:json-object "result"
                                  (nlk:json-object "value"
                                                   (nlk:json-object "value" typed
                                                                    "result" typed))))
        (answer (base64-encode (jet-browser-png)))
        (answer (nlk:json-object))))
