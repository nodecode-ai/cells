;;;; store.lisp --- the bytes that count as a sketch, and where one is kept.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two files per drawing, both under the section's `directory' (sketches/ in
;;;; the home unless it says otherwise): sketch-<stamp>.png, one per drawing,
;;;; and last.png, replaced atomically so the newest drawing always has a fixed
;;;; path to open. Nothing else is written and nothing is deleted: the pad
;;;; keeps every drawing it was given.

(in-package #:nodecode-sketch-pad)

(defparameter +png-magic+
  (make-array 8 :element-type '(unsigned-byte 8)
                :initial-contents '(137 80 78 71 13 10 26 10))
  "The eight bytes every PNG opens with: \\211 P N G \\r \\n \\032 \\n.")

(defparameter +cap-default+ 800000
  "The cap when the section names none: 800 KB of PNG is a large drawing.")

(defun sketches-directory ()
  "The folder this profile keeps its drawings in."
  (setting :directory))

(defun file-stamp ()
  "A name no two drawings share: the UTC time, then twelve random hex digits."
  (multiple-value-bind (second minute hour day month year day-of-week dst-p zone)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore day-of-week dst-p zone))
    (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d-~12,'0x"
            year month day hour minute second (random (expt 16 12)))))

(defun octets-uint32 (octets start)
  "The big-endian 32-bit number in OCTETS at START."
  (logior (ash (aref octets start) 24)
          (ash (aref octets (+ start 1)) 16)
          (ash (aref octets (+ start 2)) 8)
          (aref octets (+ start 3))))

(defun png-p (octets)
  "Whether OCTETS open with the PNG signature."
  (and (>= (length octets) (length +png-magic+))
       (loop for i below (length +png-magic+)
             always (= (aref octets i) (aref +png-magic+ i)))))

(defun png-size (octets)
  "OCTETS' width and height, read from the IHDR chunk every PNG opens with,
else NIL and NIL."
  (if (and (>= (length octets) 24)
           (string= "IHDR" (map 'string #'code-char (subseq octets 12 16))))
      (values (octets-uint32 octets 16) (octets-uint32 octets 20))
      (values nil nil)))

(defun read-head (path &optional (count 24))
  "The first COUNT octets of PATH, as octets."
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8)
                            :if-does-not-exist nil)
    (if (null in)
        (make-array 0 :element-type '(unsigned-byte 8))
        (let ((octets (make-array count :element-type '(unsigned-byte 8))))
          (subseq octets 0 (read-sequence octets in))))))

(defun write-octets (path octets)
  "Write OCTETS to PATH, its directory made first, replacing what is there."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
    (write-sequence octets out))
  path)

(defun cap ()
  "The largest PNG the section takes."
  (or (setting :max-bytes) +cap-default+))

(defun sketch-info (path)
  "PATH as a drawing goes: its name, its bytes, its size and when it was saved."
  (multiple-value-bind (width height) (png-size (read-head path))
    (list :path (namestring path)
          :archive (let ((dated (first (sort (remove-if #'null
                                                       (mapcar #'probe-file
                                                               (directory (merge-pathnames
                                                                           "sketch-*.png"
                                                                           (sketches-directory)))))
                                            #'string> :key #'file-namestring))))
                     (and dated (string/= (namestring dated) (namestring path))
                          (namestring dated)))
          :bytes (with-open-file (in path :direction :input
                                           :element-type '(unsigned-byte 8))
                   (file-length in))
          :width width :height height
          :created (nlk:iso-time (- (file-write-date path) nlk:+unix-epoch+)))))

(defun save-sketch (octets)
  "Keep OCTETS as a drawing of this profile => its SKETCH-INFO.
A refusal names what about the bytes is wrong; nothing was written then."
  (running-settings)
  (unless (vectorp octets)
    (fail "no sketch bytes arrived"))
  (unless (>= (length octets) (length +png-magic+))
    (fail "the sketch is ~d bytes, too short to be a PNG" (length octets)))
  (unless (png-p octets)
    (fail "the bytes are not a PNG: its eight-byte signature is missing"))
  (when (> (length octets) (cap))
    (fail "the sketch is ~a; the cap is ~a (sketch-pad.max_bytes)"
          (nlk:size-text (length octets)) (nlk:size-text (cap))))
  (let* ((directory (sketches-directory))
         (stamp (file-stamp))
         (path (merge-pathnames (format nil "sketch-~a.png" stamp) directory))
         (last (merge-pathnames "last.png" directory)))
    (with-sketch-pad-lock
      (write-octets path octets)
      ;; The pointer is replaced, never written in place: a reader of last.png
      ;; sees the old drawing or the new one, never half of either.
      (let ((temp (merge-pathnames (format nil ".last-~a.tmp" stamp) directory)))
        (write-octets temp octets)
        (nlk:replace-file temp last)))
    (sketch-info path)))

(defun newest-sketch-file (&aux (directory (sketches-directory)))
  "The newest drawing on disk, or NIL: last.png, the one path the newest
drawing always has, else the newest sketch-*.png if the pointer is gone."
  (or (probe-file (merge-pathnames "last.png" directory))
      (first (sort (remove-if #'null
                              (mapcar #'probe-file
                                      (directory (merge-pathnames "sketch-*.png" directory))))
                   #'string> :key #'file-namestring))))

(defun last-sketch ()
  "The newest drawing this profile kept => its SKETCH-INFO, or NIL."
  (running-settings)
  (let ((path (newest-sketch-file)))
    (and path (sketch-info path))))

;;; --- what the model and the operator read -------------------------------------

(defun sketch-line (info)
  "One line about a drawing."
  (format nil "~a (~a~@[, ~dx~d~], saved ~a)"
          (getf info :path) (nlk:size-text (getf info :bytes))
          (getf info :width) (getf info :height) (getf info :created)))

(defun last-sketch-text ()
  "sketch_pad_last's answer: the newest drawing's path, and how to see it."
  (let ((info (last-sketch)))
    (if (null info)
        (format nil "No sketch yet. The operator draws in the pad and presses Save, at ~a; ~
                     the PNG lands in this profile's sketches folder."
                (pad-url))
        (format nil "Last sketch: ~a.~@[ Each save is also kept as ~a.~]~%
                     (look ~s) shows it to you; the path stays good after this turn, ~
                     so look again when the operator asks later."
                (sketch-line info) (getf info :archive) (getf info :path)))))
