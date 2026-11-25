;;;; cell.lisp --- the planted defect, and the spec's seal probe. SPDX-License-Identifier: MIT
;;;;
;;;; While the spec holds its window open (a file `window' here), every process
;;;; that loads this cell is a repro's child: it must see no provider key, no
;;;; network and not the home it reproduces for. A child that sees any of them
;;;; writes what it saw to the spec's root, when it can, and gets the fix, so an
;;;; unsealed repro can never read as red.

(defpackage #:selffix-plant
  (:use #:cl)
  (:export #:start-cell #:rules-section))

(in-package #:selffix-plant)

(defun here (name)
  (merge-pathnames name (asdf:system-source-directory "selffix-plant")))

(defun line-of (name)
  (ignore-errors (with-open-file (s (here name)) (read-line s nil))))

(defgeneric rules-section (session)
  (:documentation "The defect: no method takes a session id."))

(defun network-up-p ()
  (ignore-errors
   (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
     (unwind-protect
          (handler-case
              (sb-sys:with-deadline (:seconds 3)
                (sb-bsd-sockets:socket-connect s #(1 1 1 1) 443)
                t)
            (error () nil))
       (sb-bsd-sockets:socket-close s)))))

(defun leaks ()
  "What this process sees that a sealed repro must not, while the window is open."
  (when (probe-file (here "window"))
    (let ((home (line-of "spec-home")))
      (remove nil
              (list (and (or (uiop:getenv "SELFFIX_SPEC_KEY") (uiop:getenv "OPENAI_API_KEY"))
                         "a provider key in its environment")
                    (and (network-up-p) "the network (1.1.1.1:443 connects)")
                    (and home
                         (probe-file (merge-pathnames "config.jsonc" (uiop:ensure-directory-pathname home)))
                         "the home it reproduces for"))))))

(let ((leaks (leaks)))
  (when leaks
    (ignore-errors
     (with-open-file (o (merge-pathnames "unsealed.txt" (uiop:ensure-directory-pathname (line-of "spec-root")))
                        :direction :output :if-exists :append :if-does-not-exist :create)
       (format o "a repro's child saw ~{~a~^, ~}~%" leaks))))
  (when (or leaks (probe-file (here "fixed")))
    (defmethod rules-section ((session string)) nil)))

(defun planted-session-p (session-id)
  "True for a session one of whose inputs says SELFFIX-PLANT."
  (some (lambda (row) (search "SELFFIX-PLANT" (first row)))
        (nlk:events :kind "turn.started" :session-id session-id :as :rows :columns '("payload"))))

(defun planted (next session-id)
  (when (and session-id (planted-session-p session-id))
    (rules-section session-id))
  (funcall next session-id))

;; A raw encapsulation, not a cell :hook: the hook runner contains a hook's
;; error, and the defect this plants is one nothing contains.
(defun start-cell (&rest arguments)
  "The loader's entry: plant the defect."
  (declare (ignore arguments))
  (unless (sb-int:encapsulated-p 'nle::read-harness-sections 'selffix-plant)
    (sb-int:encapsulate 'nle::read-harness-sections 'selffix-plant #'planted)))

(start-cell)
