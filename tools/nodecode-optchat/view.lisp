;;;; view.lisp --- the view (the original's view.py).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The view is a list of tree nodes ("parts") that tiles the whole log
;;;; [0, T), oldest first. It is what every request's `optchat' harness block
;;;; shows.
;;;;
;;;; Rendering: one line per part, id+n|text (newlines shown as spaces),
;;;; inside <chat> tags. No dates on lines (the agent calls optchat_date when
;;;; it needs one). The view never holds a whole message -- only its summary
;;;; -- and a part whose node is not built yet says so.
;;;;
;;;; How it changes: on a new message, append part(0, i) and FIT; on a node
;;;; built, FIT. FIT repeatedly merges the most due adjacent pair -- oldest
;;;; relative to its size, due = (T - start) / 2**(l+2) -- among pairs whose
;;;; parent is already built. It never splits: once merged, a part stays
;;;; merged, so the view only appends at the end and coarsens, and
;;;; consecutive views share most of their bytes.

(in-package #:nodecode-optchat)

(defun part-text (part)
  "What a part renders as: its node, or the placeholder while it is unbuilt."
  (or (node-text (car part) (cdr part)) +unbuilt+))

(defun view-size ()
  "How many characters the view takes."
  (loop for part in *parts* sum (length (part-text part))))

(defun view-lines ()
  "How many lines the view renders."
  (length *parts*))

(defun due-pair (total)
  "The index of the most due adjacent pair whose parent is built, or NIL."
  (let ((best nil) (best-due -1.0) (index 0))
    (loop for rest on *parts*
          for a = (first rest)
          for b = (second rest)
          while b
          do (when (and (eql (car a) (car b))
                        (= (cdr b) (1+ (cdr a)))
                        (evenp (cdr a))
                        (node-built-p (1+ (car a)) (floor (cdr a) 2)))
               (let ((due (/ (- total (* (cdr a) (level-span (car a))))
                             (float (level-span (+ (car a) 2))))))
                 (when (> due best-due)
                   (setf best index best-due due))))
             (incf index))
    best))

(defun merge-part (index)
  "Replace the two parts at INDEX with their built parent."
  (let* ((part (nth index *parts*))
         (parent (cons (1+ (car part)) (floor (cdr part) 2))))
    (setf *parts* (append (subseq *parts* 0 index)
                          (list parent)
                          (nthcdr (+ index 2) *parts*)))
    (setf *render* nil)))

(defun fit-view ()
  "Merge the most due pair until the view fits its budget (or nothing is built to merge)."
  (setf *render* nil)
  (loop
    (when (<= (view-size) *budget*) (return))
    (let ((index (due-pair *folded*)))
      (unless index (return))
      (merge-part index))))

(defun fold-to (total)
  "Append part(0, i) for every message the view has not taken in, then fit."
  (when (< total *folded*)
    ;; The log is append-only, so this is a repair, not a path: refold cleanly.
    (setf *parts* '() *folded* 0 *render* nil))
  (loop while (< *folded* total)
        do (setf *parts* (append *parts* (list (cons 0 *folded*)))
                 *folded* (1+ *folded*))
           (setf *render* nil))
  (fit-view))

(defun first-unbuilt-message ()
  "The first message whose view line is not a built summary, or *FOLDED* when
every line is built."
  (dolist (part *parts* *folded*)
    (unless (node-built-p (car part) (cdr part))
      (return (* (cdr part) (level-span (car part)))))))

(defun render-view ()
  "The <chat> block the model reads; cached until the view moves."
  (or *render*
      (setf *render*
            (with-output-to-string (out)
              (write-string "<chat>" out)
              (dolist (part *parts*)
                (multiple-value-bind (id n) (node-address (car part) (cdr part))
                  (format out "~%~d+~d|~a" id n (flatten (part-text part)))))
              (format out "~%</chat>")))))
