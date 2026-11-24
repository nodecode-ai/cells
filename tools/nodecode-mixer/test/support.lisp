;;;; support.lisp --- mixer test runner and the helpers its tests share.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The mixer's tests register into the shared nodecode.test registry under
;;;; a MIXER- name prefix, so RUN-MIXER-TESTS runs exactly that slice and the
;;;; core suite never runs them. Every test works in a folder of its own under
;;;; the temporary directory, so nothing reads or writes a live profile.

(in-package #:nodecode.test)

(define-test-slice "mixer" "MIXER-" :start nodecode-mixer:start-cell)

(defun mixer-verdict (form &optional strings)
  "The installed gate's answer for FORM, one EVAL call -- FORM's strings
object, when given: :ALLOWED, or the refusal string the model would read."
  (funcall (cell-hook "nodecode-mixer" :tool)
           (list :name "eval"
                 :arguments (if strings
                                (nlk:json-object "form" form "strings" strings)
                                (nlk:json-object "form" form))
                 :call-id "c")
           (lambda (op) (declare (ignore op)) :allowed)))

(defun mixer-dir ()
  "A fresh folder under the temporary directory, named with a trailing slash."
  (let ((dir (format nil "~a/mixer-~d/"
                      (string-right-trim "/" (namestring (uiop:temporary-directory)))
                      (random 100000000))))
    (ensure-directories-exist dir)
    dir))

(defun mixer-file (dir name text)
  "Write TEXT to NAME inside DIR, and answer the path."
  (let ((path (format nil "~a~a" dir name)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string text out))
    path))

(defmacro with-mixer-dir ((var) &body body)
  "Run BODY with VAR a fresh folder, and take the folder away after."
  `(let ((,var (mixer-dir)))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree (uiop:ensure-directory-pathname ,var)
                                                  :validate t)))))

(defmacro with-started-mixer ((&rest config) &body body)
  "Run BODY with a mixer started on CONFIG, and stop it after."
  `(with-cell-stop ((mixer-start ,@config))
     ,@body))

(defmacro with-mixer-cell ((dir &rest config) &body body)
  "Run BODY with DIR a fresh folder and a mixer started on CONFIG."
  `(with-mixer-dir (,dir)
     (with-started-mixer ,config
       ,@body)))
