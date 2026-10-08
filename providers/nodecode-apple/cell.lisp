;;;; cell.lisp --- the cell: the on-device model on a lane of its own, on a Mac.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No lane of the organism reaches Apple's FoundationModels framework, so on
;;;; a Mac with Apple silicon the cell registers one, apple, whose stream is
;;;; STREAM-ROUND (wire.lisp), and asks the bridge, off the start's thread,
;;;; whether the model can generate. Anywhere else it says once that it does
;;;; nothing here, and registers nothing. Two hooks, each declining for every
;;;; provider but apple:
;;;;
;;;;   MODELS-CATALOG-TABLE   on a Mac, the catalog carries the apple row: this
;;;;                          cell's lane package, and the one model while the
;;;;                          bridge says it is available, sized as it says
;;;;   :CREDENTIAL            no key: the model is on the machine
;;;;
;;;; Config, a sibling top-level key:
;;;;   "apple": {"helper": ""}
;;;; Empty builds the bridge helper from bridge/ on first use; a path names
;;;; one already built. A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-apple)

;;; --- the catalog -----------------------------------------------------------------

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
the apple row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: on a Mac, the catalog with the apple row."
  (cond ((not (mac-p)) base)
        ((and (car memo) (eq (car memo) base)) (cdr memo))
        (t (let ((merged (make-hash-table :test 'equal)))
             (when (hash-table-p base)
               (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
             (setf (gethash +provider+ merged) (catalog-row))
             (setf *catalog* (cons base merged))
             merged))))

(defun forget-catalog ()
  "Drop the merged catalog, so the next read sees the latest availability."
  (setf *catalog* (cons nil nil)))

(defun credential (op next)
  "The :CREDENTIAL answer for apple: none to send (allows-missing-api-key)."
  (if (equal (getf op :provider) +provider+)
      (nle:make-credential "on-device" :public)
      (funcall next op)))

;;; --- availability --------------------------------------------------------------------

(defvar *probe-thread* nil
  "The thread the last start asked the bridge on, or NIL.")

(defun refresh-availability ()
  "Ask the bridge whether the model can generate (building the helper first
when it is not built), keep the answer, and say what stands in the way when
it cannot."
  (let ((event (handler-case (ask-availability (helper))
                 (nle::provider-error (condition)
                   (nlk:json-object "type" "availability" "available" nil "reason" "not_built"
                                    "message" (nle::provider-error-detail condition))))))
    (setf *availability* event)
    (forget-catalog)
    (if (available-p)
        (nle:notice nil :key +key+)
        (nle:notice (format nil "~a: the on-device model is not available (~a)~@[: ~a~]"
                            +provider+ (or (nlk:json-value event :string "reason") "unknown")
                            (nlk:json-value event :text "message"))
                    :level :warning :key +key+))
    event))

;;; --- the lane ----------------------------------------------------------------------------

(defun register-lane ()
  "The apple lane: the Foundation Models bridge under this provider's name."
  (nle::register-provider-lane
   (nle::make-provider-lane :name +provider+
                            :stream-symbol 'stream-round
                            ;; the family only names the env ladder's
                            ;; default key, which the credential hook never
                            ;; falls to
                            :family :openai-completions
                            ;; reasoning has no replay form in a transcript:
                            ;; a cut attempt's comes back as text
                            :reasoning-carry :text
                            :default-endpoint +base+
                            :path ""
                            :npm +npm+)))

(defun unregister-lane ()
  "Take the apple lane back out."
  (setf nle::*provider-lanes*
        (remove +provider+ nle::*provider-lanes* :key #'nle::provider-lane-name :test #'equal)))

(defun start ()
  "On a Mac with Apple silicon, register the lane and ask the bridge about
the model on a thread of its own; anywhere else, say once that the cell
does nothing here."
  (forget-catalog)
  (setf *availability* nil)
  (nle:on-stop #'forget-catalog)
  (nle:on-stop (lambda () (setf *availability* nil) (nle:notice nil :key +key+)))
  (if (mac-p)
      (let ((settings *apple*))
        (register-lane)
        (nle:on-stop #'unregister-lane)
        (setf *probe-thread*
              (bt2:make-thread (lambda () (let ((*apple* settings)) (refresh-availability)))
                               :name "nodecode-apple availability")))
      (nle:notice (format nil "~a: Apple Foundation Models run only on a Mac with Apple silicon (macOS 27 or later); this cell does nothing on ~(~a~)"
                          +provider+ (uiop:operating-system))
                  :level :info)))

(nle:define-cell apple
  (:section ("apple")
    (:guide "on a Mac with Apple silicon and macOS 27, with Apple Intelligence on: the cell builds omp's Swift bridge with Xcode 27 (or its Command Line Tools) on first use; helper names one already built; on any other machine the cell does nothing")
    ("helper" :string :default ""
     :doc "a built bridge helper (bridge/build.sh OUT builds one); empty builds it into the user cache on first use"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential))
