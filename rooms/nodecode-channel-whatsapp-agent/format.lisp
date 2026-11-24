;;;; format.lisp --- the words as WhatsApp shows them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; WhatsApp has one markup of its own -- *bold*, _italic_, ~strike~, a
;;;; header as a bold line, a link as its own words and its address -- and a
;;;; ceiling of 4096 UTF-16 units a message, which an emoji reaches in two.
;;;; Answers are written as markdown, so every word that leaves here is
;;;; converted and cut to fit: what the original plugin's formatting.py and
;;;; its CHUNK_LIMIT/MAX_TEXT_LENGTH splitter do, in Lisp.

(in-package #:nodecode-channel-whatsapp-agent)

(defparameter +caption-limit+ 1024
  "UTF-16 units one caption may carry, Meta's own ceiling.")

(defparameter +text-ceiling+ 4096
  "Meta's own ceiling for one message, which a hard cut never passes.")

(defparameter +caption-kinds+ '("image" "video" "document")
  "The media messages whose words ride along as a caption; an audio or a
sticker carries none, so its words go first, as text.")

(defun utf16-length (text)
  "TEXT's length as WhatsApp counts it: a character outside the basic plane
(an emoji, mostly) is two units."
  (loop for character across (or text "") sum (if (> (char-code character) #xffff) 2 1)))

(defparameter +invisible-characters+
  (list #\u200b #\u2060 #\u2063 #\ufeff)
  "Zero-width characters: they say nothing and show as gaps.")

(defparameter +odd-spaces+
  (list #\u00a0 #\u1680 #\u180e #\u202f #\u205f #\u3000))

(defparameter +odd-space-ranges+ (list (cons #\u2000 #\u200a)))

(defun sanitize-text (text)
  "TEXT without zero-width characters, its odd unicode spaces as plain ones."
  (when text
    (coerce (loop for character across text
                  unless (member character +invisible-characters+)
                    collect (if (or (member character +odd-spaces+)
                                    (loop for (low . high) in +odd-space-ranges+
                                          thereis (char<= low character high)))
                                #\Space
                                character))
            'string)))

(defun word-character-p (character)
  "Whether CHARACTER is one of a word's own, as markdown emphasis measures
them."
  (or (alphanumericp character) (char= character #\_)))

(defun emphasis-edge-p (character)
  "Whether CHARACTER next to a * keeps it from being emphasis."
  (or (word-character-p character) (char= character #\*)))

(defun italic-end (text from)
  "The index of the * closing an italic whose words begin at FROM, or NIL:
no * and no line break inside, and the words end on something."
  (loop with n = (length text)
        for index from from below n
        for character = (char text index)
        do (cond ((char= character #\*)
                  (return (and (> index from)
                               (not (member (char text (1- index)) '(#\Space #\Tab #\Newline #\Return)))
                               (let ((after (and (< (1+ index) n) (char text (1+ index)))))
                                 (or (null after) (not (emphasis-edge-p after))))
                               index)))
                 ((member character '(#\Newline #\Return)) (return nil)))))

(defun italicize (text)
  "*words* become _words_: a * a word and a * not one, as WhatsApp reads it."
  (with-output-to-string (out)
    (loop with n = (length text)
          with index = 0
          while (< index n)
          do (let* ((character (char text index))
                    (end (and (char= character #\*)
                              (or (zerop index)
                                  (not (emphasis-edge-p (char text (1- index)))))
                              (< (1+ index) n)
                              (let ((next (char text (1+ index))))
                                (and (not (member next '(#\Space #\Tab #\Newline #\Return)))
                                     (not (char= next #\*))
                                     (italic-end text (1+ index)))))))
               (if end
                   (progn (write-char #\_ out)
                          (write-string text out :start (1+ index) :end end)
                          (write-char #\_ out)
                          (setf index (1+ end)))
                   (progn (write-char character out) (incf index)))))
    ))

(defun bold-stars (text)
  "**words** become *words*, the one bold WhatsApp has."
  (with-output-to-string (out)
    (loop with n = (length text)
          with index = 0
          while (< index n)
          do (let ((end (and (char= (char text index) #\*)
                             (< (1+ index) n)
                             (char= (char text (1+ index)) #\*)
                             (search "**" text :start2 (+ index 2)))))
               (if (and end
                        (> end (+ index 2))
                        (not (find #\Newline text :start (+ index 2) :end end)))
                   (progn (write-char #\* out)
                          (write-string text out :start (+ index 2) :end end)
                          (write-char #\* out)
                          (setf index (+ end 2)))
                   (progn (write-char (char text index) out) (incf index)))))
    ))

(defun underscore-close-p (text index)
  "Whether the __ at INDEX closes an underscore bold: its words end on
something and what follows is no word's character."
  (let ((after (and (< (+ index 2) (length text)) (char text (+ index 2)))))
    (and (not (member (char text (1- index)) '(#\Space #\Tab #\Newline #\Return)))
         (or (null after)
             (member after '(#\Space #\Tab #\Newline #\Return
                             #\, #\; #\: #\! #\? #\)))))))

(defun bold-underscores (text)
  "__words__ become *words*, where the words start and end where the
underscores are: __init__.py is a name and stays one."
  (with-output-to-string (out)
    (loop with n = (length text)
          with index = 0
          while (< index n)
          do (let* ((character (char text index))
                    (opening (and (char= character #\_)
                                  (< (1+ index) n)
                                  (char= (char text (1+ index)) #\_)
                                  (or (zerop index)
                                      (and (not (word-character-p (char text (1- index))))
                                           (not (char= #\. (char text (1- index))))))
                                  (< (+ index 2) n)
                                  (not (member (char text (+ index 2))
                                               '(#\Space #\Tab #\Newline #\Return)))))
                    (end (and opening (search "__" text :start2 (+ index 2)))))
               (if (and end (> end (+ index 2)) (underscore-close-p text end))
                   (progn (write-char #\* out)
                          (write-string text out :start (+ index 2) :end end)
                          (write-char #\* out)
                          (setf index (+ end 2)))
                   (progn (write-char character out) (incf index)))))
    ))

(defun header-to-bold (line)
  "LINE as a header WhatsApp shows: its own words, bold, the marks gone."
  (let ((marks (position-if-not (lambda (character) (char= character #\#)) line)))
    (when (and marks (<= 1 marks 6) (< marks (length line))
               (member (char line marks) '(#\Space #\Tab)))
      (let ((words (string-trim '(#\Space #\Tab) (subseq line (1+ marks)))))
        (loop while (and (> (length words) 1)
                         (char= (char words 0) #\*)
                         (char= (char words (1- (length words))) #\*))
              do (setf words (string-trim '(#\Space #\Tab)
                                           (subseq words 1 (1- (length words))))))
        (format nil "*~a*" words)))))

(defun headers-to-bold (text)
  "# Heading lines become *Heading lines*."
  (with-output-to-string (out)
    (loop for start = 0 then (1+ newline)
          for newline = (position #\Newline text :start start)
          do (let ((line (subseq text start (or newline (length text)))))
               (write-string (or (header-to-bold line) line) out)
               (when newline (write-char #\Newline out)))
          while newline)))

(defun markdown-to-whatsapp (text)
  "TEXT's markdown as WhatsApp spells the same things."
  (let ((result (italicize text)))
    (setf result (bold-stars result))
    (setf result (bold-underscores result))
    (setf result (ppcre:regex-replace-all "~~(.+?)~~" result "~\\1~"))
    (setf result (headers-to-bold result))
    (ppcre:regex-replace-all "\\[([^\\]]+)\\]\\(([^)]+)\\)" result "\\1 (\\2)")))

(defun split-fences (text)
  "TEXT as (:text . words) and (:code . words) segments: a fenced block,
from its ``` to its closing ```, is kept as written."
  (let ((segments '()) (start 0) (index 0) (n (length text)))
    (flet ((emit (kind from to)
             (when (< from to) (push (cons kind (subseq text from to)) segments))))
      (loop while (< index n)
            do (let ((fence (search "```" text :start2 index)))
                 (if (null fence)
                     (setf index n)
                     (let ((end (search "```" text :start2 (+ fence 3))))
                       (emit :text start fence)
                       (if end
                           (progn (emit :code fence (+ end 3))
                                  (setf start (+ end 3) index (+ end 3)))
                           (progn (emit :code fence n) (setf start n index n)))))))
      (emit :text start n)
      (nreverse segments))))

(defun split-inline (segments)
  "SEGMENTS, their text split again at inline `code`, kept as written."
  (loop for (kind . words) in segments
        append (if (eq kind :code)
                   (list (cons kind words))
                   (let ((pieces '()) (start 0) (index 0) (n (length words)))
                     (flet ((emit (piece-kind from to)
                              (when (< from to) (push (cons piece-kind (subseq words from to)) pieces))))
                       (loop while (< index n)
                             do (let ((tick (position #\` words :start index)))
                                  (if (null tick)
                                      (setf index n)
                                      (let* ((newline (position #\Newline words :start tick))
                                             (close (position #\` words :start (1+ tick)
                                                                    :end newline)))
                                        (if (and close (> close (1+ tick)))
                                            (progn (emit :text start tick)
                                                   (emit :code tick (1+ close))
                                                   (setf start (1+ close) index (1+ close)))
                                            (setf index (1+ tick)))))))
                       (emit :text start n))
                     (nreverse pieces)))))

(defun to-whatsapp (text)
  "TEXT as WhatsApp shows it: markdown converted, code as it was written."
  (when text
    (with-output-to-string (out)
      (dolist (segment (split-inline (split-fences (sanitize-text text))))
        (if (eq (car segment) :code)
            (write-string (cdr segment) out)
            (write-string (markdown-to-whatsapp (cdr segment)) out))))))

(defun utf16-index (text limit)
  "How many characters of TEXT fit in LIMIT UTF-16 units."
  (loop with width = 0
        with index = 0
        for character across text
        for units = (if (> (char-code character) #xffff) 2 1)
        while (<= (+ width units) limit)
        do (incf width units) (incf index)
        finally (return index)))

(defun last-search (text needle end)
  "Where NEEDLE last begins in TEXT before END, or NIL."
  (loop with at = nil
        with start = 0
        for found = (search needle text :start2 start :end2 end)
        while found
        do (setf at found start (1+ found))
        finally (return at)))

(defun chunk-end (text index)
  "Where a message ends, INDEX characters in: after a paragraph, else after
a line, else after a space, when one is past the halfway mark; else at
INDEX itself."
  (labels ((near (at) (and at (> at (floor index 2)))))
    (or (let ((at (last-search text (format nil "~c~c" #\Newline #\Newline) index)))
          (and (near at) (+ at 2)))
        (let ((at (last-search text (string #\Newline) index)))
          (and (near at) (1+ at)))
        (let ((at (last-search text " " index)))
          (and (near at) (1+ at)))
        (max 1 index))))

(defun split-formatted (text &key (limit +text-limit+))
  "TEXT cut into messages of at most LIMIT UTF-16 units, each ending on a
paragraph, a line or a word where one is near enough."
  (let ((chunks '())
        (rest (or text "")))
    (loop while (plusp (length rest))
          do (if (<= (utf16-length rest) limit)
                 (progn (push rest chunks) (setf rest ""))
                 (let* ((index (utf16-index rest limit))
                        (end (min (length rest) (chunk-end rest index))))
                   (push (subseq rest 0 end) chunks)
                   (setf rest (string-left-trim '(#\Space #\Tab #\Newline #\Return)
                                                 (subseq rest end))))))
    (let ((ordered (remove-if (lambda (chunk)
                                (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                            chunk))))
                              (nreverse chunks))))
      (or ordered (list "")))))

(defun send-text-plans (to text &key reply-to (limit +text-limit+))
  "The messages TEXT becomes: markdown as WhatsApp shows it, cut to LIMIT
units, the first quoting REPLY-TO. A long answer is several messages, as
the platform takes none over 4096."
  (loop for chunk in (split-formatted (to-whatsapp text) :limit limit)
        for first = t then nil
        collect (send-text-plan to chunk :reply-to (and first reply-to))))

(defun formatted-caption (pathname caption)
  "CAPTION as a media message may carry it: (values CAPTION TEXT-FIRST).
The words ride on the file only when the kind takes a caption and they fit
1024 units; otherwise TEXT-FIRST is the whole of them, said first."
  (let* ((kind (media-kind pathname))
         (formatted (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  (to-whatsapp (or caption "")))))
    (cond ((zerop (length formatted)) (values nil nil))
          ((not (member kind +caption-kinds+ :test #'string=)) (values nil formatted))
          ((<= (utf16-length formatted) +caption-limit+) (values formatted nil))
          (t (values nil formatted)))))
