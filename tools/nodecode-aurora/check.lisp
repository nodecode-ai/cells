;;;; check.lisp --- a page measured against the census, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The other half of the plugin: a model can write markup, only code can say
;;;; whether the classes it wrote exist, whether the packs the classes live in
;;;; are actually linked, and whether the page broke one of the system's rules.
;;;; The checks are the original's, in the same order and with the same
;;;; verdicts: an unknown class, a pack used but not linked, a gradient, a
;;;; backdrop-filter, an !important, a hand-written colour, an emoji in the UI,
;;;; a missing attribution footer and a second h1 fail a page; a missing lang,
;;;; theme, body class or a max-width breakpoint is a warning a person reads.
;;;;
;;;; Nothing here reaches the network or writes a byte: a string goes in, the
;;;; findings come out.

(in-package #:nodecode-aurora)

(defparameter +max-input+ 2000000
  "Two megabytes is already a very large page -- the original's ceiling.")

;;; --- scanning a page ---------------------------------------------------------------

(defun ci-search (needle haystack &optional (start 0))
  "NEEDLE in HAYSTACK from START, whatever the case."
  (cl:search needle haystack :start2 start :test #'char-equal))

(defun blankp (character)
  (member character '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun attribute-values (html name)
  "Every value the attribute NAME carries in HTML, quoted either way."
  (let ((values '()) (position 0) (width (length name)) (size (length html)))
    (loop
      (let ((at (ci-search name html position)))
        (unless at (return))
        (setf position (+ at width))
        (when (and (or (zerop at)
                       (let ((before (char html (1- at))))
                         (not (or (alphanumericp before)
                                  (member before '(#\- #\_ #\:) :test #'char=)))))
                   (< position size))
          (let ((cursor position))
            (loop while (and (< cursor size) (blankp (char html cursor))) do (incf cursor))
            (when (and (< cursor size) (char= (char html cursor) #\=))
              (incf cursor)
              (loop while (and (< cursor size) (blankp (char html cursor))) do (incf cursor))
              (when (and (< cursor size) (member (char html cursor) '(#\" #\') :test #'char=))
                (let* ((quote-character (char html cursor))
                       (end (position quote-character html :start (1+ cursor))))
                  (when end
                    (push (subseq html (1+ cursor) end) values)
                    (setf position (1+ end))))))))))
    (nreverse values)))

(defun tag-text (html name)
  "The first NAME element's opening tag in HTML, or NIL when there is none."
  (let* ((open (concatenate 'string "<" name))
         (at (ci-search open html))
         (size (length html)))
    (when (and at (< (+ at (length open)) size)
               (let ((after (char html (+ at (length open)))))
                 (or (blankp after) (char= after #\>) (char= after #\/))))
      (let ((end (position #\> html :start at)))
        (and end (subseq html at (1+ end)))))))

(defun element-contents (html name)
  "The inner text of every NAME element in HTML, oldest first."
  (let ((open (concatenate 'string "<" name))
        (close (concatenate 'string "</" name ">"))
        (contents '())
        (position 0))
    (loop
      (let ((at (ci-search open html position)))
        (unless at (return))
        (let* ((start (position #\> html :start at))
               (end (and start (ci-search close html (1+ start)))))
          (if (and start end)
              (progn (push (subseq html (1+ start) end) contents)
                     (setf position (+ end (length close))))
              (return)))))
    (nreverse contents)))

(defun remove-elements (html name)
  "HTML with every NAME element, tags and all, cut out."
  (let ((open (concatenate 'string "<" name))
        (close (concatenate 'string "</" name ">")))
    (with-output-to-string (out)
      (let ((position 0))
        (loop
          (let ((at (ci-search open html position)))
            (if (null at)
                (progn (write-string html out :start position) (return))
                (let ((end (ci-search close html at)))
                  (write-string html out :start position :end at)
                  (setf position (if end (+ end (length close)) (length html)))))))))))

(defun strip-comments (text)
  (with-output-to-string (out)
    (let ((position 0))
      (loop
        (let ((at (cl:search "<!--" text :start2 position)))
          (if (null at)
              (progn (write-string text out :start position) (return))
              (progn (write-string text out :start position :end at)
                     (let ((end (cl:search "-->" text :start2 at)))
                       (setf position (if end (+ end 3) (length text)))))))))))

(defun strip-tags (text)
  (with-output-to-string (out)
    (let ((position 0))
      (loop
        (let ((at (position #\< text :start position)))
          (if (null at)
              (progn (write-string text out :start position) (return))
              (let ((end (position #\> text :start at)))
                (write-string text out :start position :end at)
                (setf position (if end (1+ end) (length text))))))))))

(defun visible-text (html footer)
  "The words a reader sees: comments, scripts, styles, code samples and tags
out, the attribution footer out."
  (let ((text (strip-comments html)))
    (dolist (name '("script" "style" "pre" "code" "samp" "kbd" "textarea"))
      (setf text (remove-elements text name)))
    (setf text (strip-tags text))
    (if footer (replace-all text footer " ") text)))

(defun replace-all (text old new)
  "TEXT with every OLD replaced by NEW."
  (if (or (null old) (zerop (length old)))
      text
      (with-output-to-string (out)
        (let ((position 0) (width (length old)))
          (loop
            (let ((at (cl:search old text :start2 position)))
              (if (null at)
                  (progn (write-string text out :start position) (return))
                  (progn (write-string text out :start position :end at)
                         (write-string new out)
                         (setf position (+ at width))))))))))

(defun count-occurrences (needle haystack)
  (let ((count 0) (position 0))
    (loop
      (let ((at (ci-search needle haystack position)))
        (unless at (return count))
        (incf count)
        (setf position (+ at (length needle)))))))

(defun hex-colours (css)
  "How many hand-written hex colours CSS carries."
  (let ((count 0) (index 0) (size (length css)))
    (loop while (< index size)
          do (if (char= (char css index) #\#)
                 (let ((end (1+ index)))
                   (loop while (and (< end size) (digit-char-p (char css end) 16))
                         do (incf end))
                   (let ((width (- end index 1)))
                     (when (and (<= 3 width 8)
                                (or (= end size) (not (alphanumericp (char css end)))))
                       (incf count)))
                   (setf index (max end (1+ index))))
                 (incf index)))
    count))

(defun function-colours (css)
  "How many rgb()/rgba()/hsl()/hsla() colours CSS carries."
  (reduce #'+ (mapcar (lambda (name) (count-occurrences name css))
                      '("rgb(" "rgba(" "hsl(" "hsla("))))

(defun emoji-p (character)
  "Whether CHARACTER is one of the pictographs a UI should not carry."
  (let ((code (char-code character)))
    (or (<= #x1f300 code #x1faff)
        (<= #x2600 code #x27bf)
        (<= #x1f1e6 code #x1f1ff)
        (<= #x2b00 code #x2bff))))

(defun emojis-in (text)
  "Every pictograph TEXT carries, once each."
  (let ((found '()))
    (loop for character across text
          when (emoji-p character)
            do (pushnew character found :test #'char=))
    (sort found #'char<)))

;;; --- the verdicts ------------------------------------------------------------------

(defun nz-classes (html)
  "The nz- classes HTML uses, each once, and how often each is written."
  (let ((used (make-hash-table :test #'equal)))
    (dolist (value (attribute-values html "class"))
      (let ((position 0))
        (loop
          (let* ((end (position #\Space value :start position))
                 (token (subseq value position end)))
            (when (and (>= (length token) 3) (string= "nz-" token :end2 3))
              (incf (gethash token used 0)))
            (if end (setf position (1+ end)) (return))))))
    used))

(defun css-links (html)
  "The .css hrefs HTML links."
  (remove-if-not (lambda (value)
                   (let ((value (string-downcase value)))
                     (and (>= (length value) 5)
                          (string= ".css" value :start2 (- (length value) 4)))))
                 (attribute-values html "href")))

(defun link-matches-p (href pack)
  "Whether HREF serves PACK: its exact path, that path at the end of a URL, or all.css (whole-link only)."
  (let ((href (string-downcase href)) (pack (string-downcase pack)))
    (or (string= href pack)
        (let ((at (cl:search pack href :from-end t)))
          (and at (= (+ at (length pack)) (length href)))))))

(defun pack-linked-p (packs pack)
  (some (lambda (href) (link-matches-p href pack)) packs))

(defun all-css-linked-p (packs)
  (some (lambda (href) (let ((href (string-downcase href)))
                         (and (>= (length href) 13) (string= "packs/all.css" href :start2 (- (length href) 13)))))
        packs))

(defun check (source &key footer (require-footer t))
  "SOURCE, an Aurora page as a string, measured against the census; => (VALUES
OK FAILURES WARNINGS MEASURED). FOOTER is the attribution the page must carry
verbatim, REQUIRE-FOOTER whether its absence fails the page."
  (ensure-census)
  (let* ((html (or source ""))
         (failures '())
         (warnings '())
         (used (nz-classes html))
         (unknown '())
         (packs '()))
    (flet ((failure (text) (push text failures))
           (warning (text) (push text warnings)))
      ;; 1 - every class must exist in the census
      (maphash (lambda (class count)
                 (declare (ignore count))
                 (unless (gethash class *known*) (push class unknown)))
               used)
      (when unknown
        (failure (format nil "unknown class(es), not in the census: ~{~a~^, ~}"
                         (subseq (sort unknown #'string<) 0 (min 12 (length unknown))))))
      ;; 2 - every pack a used class belongs to must be linked
      (maphash (lambda (class count)
                 (declare (ignore count))
                 (let ((entry (gethash class *known*)))
                   (when (and entry (first entry) (not (member (first entry) packs :test #'string=)))
                     (push (first entry) packs))))
               used)
      (setf packs (sort packs #'string<))
      (let* ((links (css-links html))
             (missing (if (all-css-linked-p links)
                          '()
                          (remove-if (lambda (pack) (pack-linked-p links pack)) packs))))
        (when missing
          (failure (format nil "pack(s) used but not linked: ~{~a~^, ~}" missing))))
      ;; 3 - gradients, backdrop-filter and !important, outside the crystal layer
      (let* ((styles (format nil "~{~a~^~%~}" (element-contents html "style")))
             (css (strip-comments styles))
             (crystal (ci-search "nz-cristal" html)))
        (when (and (> (count-occurrences "gradient(" css) 0) (null crystal))
          (failure "gradient in page CSS (Aurora uses none; the crystal layer ships as p16-cristal.css)"))
        (when (and (> (count-occurrences "backdrop-filter" css) 0) (null crystal))
          (failure "backdrop-filter in page CSS (only the .nz-cristal layer uses it)"))
        (when (and (> (count-occurrences "!important" css) 0)
                   (null (ci-search "nz-visually-hidden" html))
                   (null (ci-search "nz-print-hide" html))
                   (null (ci-search "nz-vh" html)))
          (failure "!important in page CSS (only the hiding helpers carry it)"))
        ;; 4 - colours written by hand in the page's own CSS
        (let ((hand (+ (hex-colours css) (function-colours css))))
          (when (plusp hand)
            (failure (format nil "~d hand-written colour(s) in the page's style: use var(--nz-*)" hand)))))
      ;; 5 - emoji in the UI, outside the attribution
      (let ((found (emojis-in (visible-text html footer))))
        (when found
          (failure (format nil "emoji in the UI (~{~a~}): use an SVG icon from the sprite"
                           (subseq found 0 (min 6 (length found)))))))
      ;; 6 - the attribution footer, verbatim
      (when (and require-footer
                 (plusp (length (or footer "")))
                 (null (cl:search footer html)))
        (failure (format nil "missing the exact footer: <<~a>>" footer)))
      ;; 7 - structure
      (when (> (count-occurrences "<h1" html) 1)
        (failure "more than one <h1>: one heading per page"))
      (let ((html-tag (or (tag-text html "html") "")))
        (when (null (attribute-values html-tag "lang")) (warning "missing lang on <html>"))
        (let ((theme (first (attribute-values html-tag "data-nz-theme"))))
          (unless (and theme (member (string-downcase theme) '("light" "dark") :test #'string=))
            (warning "missing data-nz-theme=\"light|dark\" on <html>"))))
      (let* ((body (or (tag-text html "body") ""))
             (classes (first (attribute-values body "class"))))
        (unless (and classes (member "nz" (split-words classes) :test #'string=))
          (warning "missing class=\"nz\" on <body>")))
      (let ((styles (format nil "~{~a~^~%~}" (element-contents html "style"))))
        (when (and (> (count-occurrences "@media" styles) 0)
                   (> (count-occurrences "max-width" styles) 0)
                   (null (ci-search "max-width: 639px" styles)))
          (warning "@media (max-width): Aurora is mobile-first (base + min-width)")))
      (values (null failures)
              (nreverse failures)
              (nreverse warnings)
              (list :nz-classes (hash-table-count used)
                    :families (length (remove-duplicates
                                       (loop for class being the hash-keys of used
                                             when (gethash class *known*)
                                               collect (second (gethash class *known*)))
                                       :test #'string=))
                    :packs (mapcar (lambda (pack) (car (last (split-on pack #\/)))) packs))))))

(defun split-words (text)
  (let ((words '()) (start 0))
    (loop for index from 0 to (length text)
          do (if (or (= index (length text))
                      (member (char text index) '(#\Space #\Tab #\Newline #\Return) :test #'char=))
                 (progn (when (> index start) (push (subseq text start index) words))
                        (setf start (1+ index)))))
    (nreverse words)))

(defun split-on (text separator)
  (let ((parts '()) (start 0))
    (loop for index from 0 to (length text)
          do (if (or (= index (length text)) (char= (char text index) separator))
                 (progn (push (subseq text start index) parts) (setf start (1+ index)))))
    (nreverse parts)))

;;; --- what a model reads -------------------------------------------------------------

(defun report-text (ok failures warnings measured)
  "The verdicts as the block a model reads, and the measured line under it."
  (with-output-to-string (out)
    (format out "~a~%" (if ok "Aurora check: no failures." "Aurora check: FAILED."))
    (when failures
      (format out "must fix:~%")
      (dolist (failure failures) (format out "  - ~a~%" failure)))
    (when warnings
      (format out "read these, they do not block:~%")
      (dolist (warning warnings) (format out "  - ~a~%" warning)))
    (format out "measured: ~d nz classes, ~d families, packs ~{~a~^, ~}~%"
            (getf measured :nz-classes) (getf measured :families)
            (or (getf measured :packs) '("none")))
    (format out "next: ~a~%"
            (if failures
                "fix the failures and check again."
                "no failures: now measure the render in a browser (mobile 320/360/390)."))))
