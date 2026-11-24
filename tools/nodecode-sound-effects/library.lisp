;;;; library.lisp --- this profile's sound library: one JSON line per generation,
;;;; and the way /sfx reads it back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this cell rewrites kept sounds.jsonl in its own data folder for
;;;; a Desktop library tab. Here the record lives beside the audio, in the
;;;; folder the sounds are saved to, so the folder an operator opens holds
;;;; both. Lines are appended, never rewritten, so two generations at once
;;;; cannot lose each other's line; a torn or unknown line is skipped on read;
;;;; and only the file NAME is stored, so a record never points outside that
;;;; folder.

(in-package #:nodecode-sound-effects)

(defparameter +fields+
  '("id" "created_at" "prompt" "duration_seconds" "length_seconds" "loop" "prompt_influence"
    "model" "output_format" "file_name" "bytes" "billed_characters" "session_id")
  "What one generation records. duration_seconds is the length asked for -- what
ElevenLabs bills, null when it chose one -- and length_seconds the length of
the file it returned, which can be longer for a loop.")

(defparameter +sound-id-chars+ 12
  "How many hex digits a sound id has: the width the plugin's uuid4 prefix had,
and what /sfx takes as an id.")

(defun sound-id ()
  "A fresh sound id: 12 lowercase hex digits."
  (format nil "~12,'0x" (random (expt 16 +sound-id-chars+))))

(defun sound-id-p (text)
  "Whether TEXT is shaped like a sound id."
  (and (stringp text)
       (= +sound-id-chars+ (length text))
       (every (lambda (char) (digit-char-p char 16)) text)))

;;; --- where it lives ---------------------------------------------------------------

(defun library-path ()
  "The library file: one JSON line per sound, in the sounds folder."
  (merge-pathnames "library.jsonl" (setting :directory)))

(defun record (row)
  "Append ROW to the library. Never signals: the sound is saved and billed
already, and a lost row must not turn a paid call into a failure."
  (nlk:with-handlers ((error () nil))
    (ensure-directories-exist (library-path))
    (nlk:append-json-line (library-path) row)
    t))

(defun records ()
  "Every sound this profile kept, newest first, one per id, a torn line skipped."
  (let ((seen (make-hash-table :test #'equal)))
    (loop for row in (reverse (nlk:read-json-lines (library-path)))
          for id = (nlk:json-value row :string "id")
          when (and (sound-id-p id) (not (gethash id seen)))
            do (setf (gethash id seen) t)
            and collect row)))

(defun file-of (row)
  "The record's audio file when it is still in the sounds folder, else NIL."
  (let ((name (nlk:json-value row :string "file_name"))
        (directory (setting :directory)))
    (when (and name (plusp (length name))
               (not (find #\/ name)) (not (search ".." name)))
      (probe-file (merge-pathnames name directory)))))

;;; --- what /sfx says ---------------------------------------------------------------

(defun brief (text width)
  "TEXT on one line, at most WIDTH characters."
  (let ((text (substitute #\Space #\Newline (nlk:trimmed (or text "")))))
    (if (<= (length text) width)
        text
        (concatenate 'string (subseq text 0 (- width 3)) "..."))))

(defun totals-text (rows)
  "ROWS as `2 mp3, 1 wav; 1.4 MB total'."
  (let ((counts (make-hash-table :test #'equal)) (bytes 0))
    (dolist (row rows)
      (incf (gethash (or (extension-for (nlk:json-value row :string "output_format")) "other")
                      counts 0))
      (incf bytes (or (nlk:json-value row :integer "bytes") 0)))
    (format nil "~{~a~^, ~}; ~a total"
            (loop for kind being the hash-keys of counts using (hash-value n)
                  collect (format nil "~d ~a" n kind))
            (nlk:size-text bytes))))

(defun length-text (row)
  "The row's playing time as a column, or a question mark when the bytes could
not say."
  (let ((seconds (nlk:json-value row :number "length_seconds")))
    (if (and seconds (realp seconds))
        (format nil "~5,2f s" seconds)
        "      ?")))

(defparameter *listed* 25
  "How many rows /sfx lists before it says how many it left out.")

(defun library-text ()
  "The library as /sfx prints it: the sounds this profile made, newest first."
  (let ((rows (records)) (directory (namestring (setting :directory))))
    (if (null rows)
        (format nil "No sounds yet in ~a. Ask for one -- \"a short retro coin pickup\", ~
                     \"rain on a tin roof, loopable\" -- and elevenlabs_sound_effect saves it here; ~
                     every generation is billed to the ElevenLabs account."
                directory)
        (with-output-to-string (out)
          (format out "Sounds: ~d in ~a (~a)~%" (length rows) directory (totals-text rows))
          (dolist (row (subseq rows 0 (min (length rows) *listed*)))
            (format out "  ~a  ~a  ~a  ~4a  ~a~@[ (file gone)~]~%"
                    (nlk:json-value row :string "id")
                    (or (nlk:json-value row :string "created_at") "?")
                    (length-text row)
                    (or (extension-for (nlk:json-value row :string "output_format")) "?")
                    (brief (nlk:json-value row :string "prompt") 60)
                    (unless (file-of row) t)))
          (when (> (length rows) *listed*)
            (format out "  ... and ~d older; /sfx <words in its prompt> finds one~%"
                    (- (length rows) *listed*)))
          (format out "/sfx <id or words> shows one; the files are in that folder.~%")))))

(defun sound-text (row)
  "One sound in full: what made it, what it plays like, where the file is."
  (let ((path (file-of row)))
    (with-output-to-string (out)
      (format out "~a~%" (or (nlk:json-value row :string "prompt") "?"))
      (format out "  id ~a, made ~a~%"
              (nlk:json-value row :string "id")
              (or (nlk:json-value row :string "created_at") "?"))
      (format out "  ~a long, ~:[no loop~;loop~], ~a, ~a, ~a~%"
              (length-text row)
              (nlk:json-value row :boolean "loop")
              (or (nlk:json-value row :string "model") "?")
              (or (nlk:json-value row :string "output_format") "?")
              (let ((bytes (nlk:json-value row :integer "bytes")))
                (if bytes (nlk:size-text bytes) "size unknown")))
      (format out "  asked for ~a, prompt influence ~a~%"
              (let ((asked (nlk:json-value row :number "duration_seconds")))
                (if asked (format nil "~,2f s" asked) "an automatic length"))
              (let ((influence (nlk:json-value row :number "prompt_influence")))
                (if influence (format nil "~,2f" influence) "the API's default")))
      (format out "  file ~a~%" (if path (namestring path) "gone from the sounds folder")))))

(defun one-sound-text (argument)
  "The sound ARGUMENT names, by id or by words in its prompt, or the sentence
that says how to list them: the newest match wins."
  (let* ((argument (string-downcase (nlk:trimmed argument)))
         (rows (records))
         (row (or (find-if (lambda (candidate)
                             (let ((id (nlk:json-value candidate :string "id")))
                               (and id (uiop:string-prefix-p argument id))))
                           rows)
                  (find-if (lambda (candidate)
                             (search argument (string-downcase
                                               (or (nlk:json-value candidate :string "prompt") ""))))
                           rows))))
    (if row
        (sound-text row)
        (format nil "No sound matches ~s: /sfx lists this profile's ~d sound~:p."
                argument (length rows)))))
