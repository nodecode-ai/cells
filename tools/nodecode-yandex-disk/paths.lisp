;;;; paths.lisp --- Yandex Disk path normalisation and root confinement.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's paths.py, line for line: Yandex Disk addresses a resource
;;;; with a scheme -- disk:/a/b for the user's files, trash:/x for the bin --
;;;; and people (and models) say /a/b or a/b. Everything entering the client
;;;; goes through RESOLVE-PATH first, so one function decides what a path
;;;; means, including whether it escapes the optional root the section names.
;;;;
;;;; Pure logic: no HTTP, no files, so a test reads it directly.

(in-package #:nodecode-yandex-disk)

(defparameter +disk-scheme+ "disk:"
  "The user's own files.")

(defparameter +trash-scheme+ "trash:"
  "The bin: what DELETE moved there, and what TRASH-EMPTY destroys.")

(defparameter +app-scheme+ "app:"
  "The folders an application owns, which Yandex addresses with their own scheme.")

(defparameter +schemes+ (list +disk-scheme+ +trash-scheme+ +app-scheme+)
  "Every scheme the API answers, longest first is not needed: they share no prefix.")

(defun split-scheme (path)
  "PATH as (values SCHEME REST): \"disk:/a/b\" is \"disk:\" and \"/a/b\".
A path with no scheme answers an empty SCHEME, as the original did."
  (let ((text (nlk:trimmed (or path ""))))
    (dolist (scheme +schemes+ (values "" text))
      (let ((end (length scheme)))
        (when (and (<= end (length text))
                   (string-equal scheme text :end2 end))
          (return (values scheme (subseq text end))))))))

(defun normalize-segments (raw)
  "RAW as an absolute path with a leading slash, its empty and `.` segments
dropped and its `..` segments collapsing. One `..` past the root is a
refusal, as the original's was: the disk's root has no parent."
  (let ((out '()))
    (dolist (segment (uiop:split-string (substitute #\/ #\\ (or raw ""))
                                        :separator "/")
                     (format nil "/~{~a~^/~}" (nreverse out)))
      (cond ((or (zerop (length segment)) (string= segment ".")) nil)
            ((string= segment "..")
             (if out
                 (pop out)
                 (fail "Path escapes the root of the disk (too many '..' segments).")))
            (t (push segment out))))))

(defun normalize-root (root)
  "The section's root as the prefix the sandbox holds to: \"\" for the whole
disk (an empty value or \"/\"), else an absolute path with no trailing slash."
  (let ((text (nlk:trimmed (or root ""))))
    (if (zerop (length text))
        ""
        (multiple-value-bind (scheme rest) (split-scheme text)
          (declare (ignore scheme))
          (let ((normalized (normalize-segments rest)))
            (if (string= normalized "/") "" normalized))))))

(defun inside-p (candidate root)
  "Whether CANDIDATE is ROOT itself or lies under it. An empty ROOT holds
everything, which is the whole disk."
  (or (zerop (length root))
      (string= candidate root)
      (uiop:string-prefix-p (concatenate 'string root "/") candidate)))

(defun resolve-path (path &key (root "") (scheme +disk-scheme+))
  "PATH -- what a person or a model typed -- as a full <scheme>/... address.

Without a scheme the path is taken as relative to ROOT; with one it is
absolute and must already sit inside ROOT. Either way the answer is verified
to stay inside ROOT, so `..` cannot climb out of the sandbox."
  (multiple-value-bind (given rest) (split-scheme (or path "/"))
    (when (and (plusp (length given)) (not (string-equal given scheme)))
      (fail "Expected a ~a/ path, got ~a/." scheme given))
    (let ((absolute (normalize-segments (if (plusp (length given))
                                            rest
                                            (format nil "~a/~a" root rest)))))
      (unless (inside-p absolute root)
        (fail "Path ~a~a is outside the configured root ~a~a." scheme absolute scheme root))
      (format nil "~a~a" scheme absolute))))

(defun inside-root-p (path root)
  "Whether an API-answered path lies inside ROOT (an empty root: always)."
  (if (zerop (length root))
      t
      (handler-case
          (multiple-value-bind (scheme rest) (split-scheme (or path ""))
            (declare (ignore scheme))
            (inside-p (normalize-segments rest) root))
        (yandex-disk-error () nil))))

(defun display-path (path root)
  "PATH as the model is shown it: the sandbox prefix hidden when one is set.

What this answers is dropped back into RESOLVE-PATH whenever the model quotes
it, so a path stripped of its root comes back without a scheme as well -- a
scheme would make RESOLVE-PATH read it as absolute and point at a different
file. Without a root the path is handed over untouched, and so is a path in
another scheme: the bin is not inside the sandbox, so its entries keep the
scheme the tools need to address them."
  (if (zerop (length root))
      path
      (multiple-value-bind (scheme rest) (split-scheme (or path ""))
        (if (and (plusp (length scheme)) (not (string-equal scheme +disk-scheme+)))
            path
            (handler-case
                (let ((absolute (normalize-segments rest)))
                  (if (inside-p absolute root)
                      (let ((tail (subseq absolute (length root))))
                        (if (zerop (length tail)) "/" tail))
                      path))
              (yandex-disk-error () path))))))
