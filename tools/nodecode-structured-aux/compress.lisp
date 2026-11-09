;;;; compress.lisp --- selection-based context compression.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A decision model writes no prose, so this is the other thing: the text is
;;;; segmented into deterministic blocks, the provider is asked which blocks
;;;; must survive, and the digest is assembled ONLY from retained original text
;;;; plus a provenance header. Nothing is generated, so an extractive digest
;;;; cannot invent a replacement for what it dropped. Rewritten from
;;;; structured_aux/compression.py.

(in-package #:nodecode-structured-aux)

(defparameter +digest-header+ "[structured-aux extractive digest]")

(defparameter +min-block-chars+ 120
  "How much text a block holds before it is worth a question: a fragment-heavy
prompt still yields blocks large enough to be asked about.")

(defparameter +retention-goal+
  "Preserve the smallest sufficient working set for correct continuation.")

(defparameter +paragraph-scanner+ (cl-ppcre:create-scanner "\\n[ \\t]*\\n")
  "Blank lines: prompt transcripts and section separators both present this way.")

(defun join-blocks (parts)
  "PARTS joined the way they were split: one blank line between paragraphs."
  (format nil "~{~a~^~%~%~}" parts))

(defun segment (text &key max-blocks (min-block-chars +min-block-chars+))
  "TEXT as at most MAX-BLOCKS deterministic blocks. Paragraphs accumulate until
a block reaches MIN-BLOCK-CHARS, and the overflow past MAX-BLOCKS is merged
into the final block rather than dropped: segmentation must never be the thing
that loses text."
  (let* ((max-blocks (or max-blocks (min 96 (max 2 (setting :max-blocks)))))
         (paragraphs (remove-if (lambda (part) (zerop (length (nlk:trimmed part))))
                                (cl-ppcre:split +paragraph-scanner+ (or text ""))))
         (blocks '())
         (current '())
         (size 0))
    (dolist (paragraph paragraphs)
      (push paragraph current)
      (incf size (length paragraph))
      (when (>= size min-block-chars)
        (push (join-blocks (nreverse current)) blocks)
        (setf current nil size 0)))
    (when current (push (join-blocks (nreverse current)) blocks))
    (let ((blocks (nreverse blocks)))
      (if (> (length blocks) max-blocks)
          (append (subseq blocks 0 (1- max-blocks))
                  (list (join-blocks (subseq blocks (1- max-blocks)))))
          blocks))))

(defun keep-flags (blocks &key model)
  "One KEEP/DROP flag per BLOCK, asked in bounded batches; a block the provider
answers outside the contract fails the digest rather than guessing."
  (let ((per-call (min 16 (max 2 (setting :blocks-per-call))))
        (keep '()))
    (loop for start from 0 below (length blocks) by per-call
          for end = (min (length blocks) (+ start per-call))
          for chunk = (subseq blocks start end)
          for names = (loop for offset below (length chunk)
                            collect (format nil "block_~d" (+ start offset)))
          do (let* ((questions (loop for name in names
                                     collect (cons name
                                                   (choice-question +keep-drop-instructions+
                                                                     +keep-drop-criteria+))))
                    (state (nlk:make-json-object
                            "retention_goal" +retention-goal+
                            "blocks" (coerce (loop for name in names
                                                   for block in chunk
                                                   collect (nlk:make-json-object "id" name "text" block))
                                             'vector))))
               (multiple-value-bind (answers answered usage) (ask state questions :model model)
                 (declare (ignore answered usage))
                 (dolist (name names)
                   (push (string= "KEEP" (answer-choice answers (id-keyword name) +keep-drop-labels+))
                         keep)))))
    (nreverse keep)))

(defun build-digest (blocks keep &key model)
  "Retained BLOCKS where KEEP says so, as a digest bounded by the section's
budget. Never empty: if nothing was retained the last block is kept anyway,
because an empty digest reads as a failed compression."
  (let* ((budget (max 400 (setting :digest-budget-chars)))
         (total (length blocks))
         (retained (loop for block in blocks for flag in keep when flag collect block)))
    (when (and (null retained) blocks) (setf retained (last blocks)))
    (let* ((header (format nil "~a retained ~d/~d blocks - model=~a - no text was generated; retained text is verbatim."
                            +digest-header+ (length retained) total
                            (or (nonblank model) "unknown")))
           (parts (list header))
           (used (length header)))
      (loop for block in retained
            for index from 1
            for piece = (format nil "~%~%--- block ~d ---~%~a" index block)
            for remaining = (- budget used)
            while (plusp remaining)
            do (when (> (length piece) remaining) (setf piece (subseq piece 0 remaining)))
               (push piece parts)
               (incf used (length piece)))
      (format nil "~{~a~}" (nreverse parts)))))

(define-verb compress (text &key max-blocks model)
  "Digest TEXT by selection: segment it into blocks, ask which must survive, and
answer the retained text verbatim under a provenance header. => (values DIGEST
KEPT TOTAL). Nothing is generated, so an exact value in a retained block is the
value that comes back."
  (unless (nonblank text)
    (fail "compress needs the text to digest"))
  (let ((blocks (segment text :max-blocks max-blocks)))
    (when (null blocks)
      (fail "compress found no blocks in that text"))
    (let ((keep (keep-flags blocks :model model)))
      (values (build-digest blocks keep :model (or (nonblank model) (setting :model)))
              (count t keep)
              (length blocks)))))
