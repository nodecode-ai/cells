;;;; chords.lisp --- the offline transposer, as the plugin's helper had it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin shipped scripts/transpose.py: a chord-root regex and two
;;;; twelve-name scales, making no network call. This is that helper, ported
;;;; straight across, so transpose_chords answers with no server at all when
;;;; the section says so (offline_transpose, on by default).
;;;;
;;;; The rules are the helper's, unchanged: a root is one of A-G, optionally
;;;; followed by # or b, not preceded by a letter, and followed by a chord
;;;; quality token (maj, min, dim, aug, sus, add, m, M), a digit, a slash,
;;;; or the end of a word; this moves roots only, never a note inside a
;;;; symbol, and never the letters of a word like "Cabaret".

(in-package #:nodecode-clef)

(defparameter +sharp-roots+
  #("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B")
  "The twelve roots spelled with sharps, as the helper's SHARP list.")

(defparameter +flat-roots+
  #("C" "Db" "D" "Eb" "E" "F" "Gb" "G" "Ab" "A" "Bb" "B")
  "The twelve roots spelled with flats, as the helper's FLAT list.")

(defparameter +root-pitch+
  '(("C" . 0) ("B#" . 0) ("C#" . 1) ("Db" . 1) ("D" . 2) ("D#" . 3)
    ("Eb" . 3) ("E" . 4) ("Fb" . 4) ("E#" . 5) ("F" . 5) ("F#" . 6)
    ("Gb" . 6) ("G" . 7) ("G#" . 8) ("Ab" . 8) ("A" . 9) ("A#" . 10)
    ("Bb" . 10) ("B" . 11) ("Cb" . 11))
  "Each root the helper's NAMES table carried, and its pitch class.")

(defparameter +chord-tokens+ '("maj" "min" "dim" "aug" "sus" "add" "m" "M")
  "What the helper's lookahead let a root be followed by.")

(defun ascii-letter-p (character)
  "CHARACTER an ASCII letter, as the helper's [A-Za-z] meant?"
  (and character (or (char<= #\A character #\Z) (char<= #\a character #\z))))

(defun word-character-p (character)
  "CHARACTER part of a word, as the helper's \\b meant: an ASCII letter, a
digit or an underscore."
  (and character
       (or (ascii-letter-p character)
           (char<= #\0 character #\9)
           (char= character #\_))))

(defun chord-continues-p (text position)
  "Does a chord symbol continue at POSITION: a quality token, a digit, a
slash, or a word boundary, exactly as the helper's lookahead said?"
  (if (>= position (length text))
      t
      (let ((character (char text position)))
        (or (some (lambda (token)
                    (let ((end (+ position (length token))))
                      (and (<= end (length text))
                           (string= text token :start1 position :end1 end))))
                  +chord-tokens+)
            (char<= #\0 character #\9)
            (char= character #\/)
            (not (word-character-p character))))))

(defun root-at (text position)
  "The chord root starting at POSITION, and where it ends: (VALUES NAME END),
or NIL when TEXT carries no root there."
  (let* ((end (length text))
         (character (char text position))
         (previous (and (plusp position) (char text (1- position)))))
    (when (and (char<= #\A character #\G)
               (not (ascii-letter-p previous)))
      (let* ((next (and (< (1+ position) end) (char text (1+ position))))
             (root-end (if (and next (or (char= next #\#) (char= next #\b)))
                           (+ position 2)
                           (1+ position))))
        (values (subseq text position root-end) root-end)))))

(defun shift-chords (text semitones &key prefer-flats)
  "Every chord root in TEXT moved SEMITONES semitones, spelled with flats
when PREFER-FLATS: the helper's TRANSPOSE(), answering the same string."
  (let ((scale (if prefer-flats +flat-roots+ +sharp-roots+))
        (end (length text)))
    (with-output-to-string (out)
      (loop with index = 0
            while (< index end)
            do (let ((character (char text index)))
                 (multiple-value-bind (name root-end) (root-at text index)
                   (let ((pitch (and name (cdr (assoc name +root-pitch+ :test #'string=)))))
                     (if (and pitch (chord-continues-p text root-end))
                         (progn (write-string (aref scale (mod (+ pitch semitones) 12)) out)
                                (setf index root-end))
                         (progn (write-char character out)
                                (incf index))))))))))
