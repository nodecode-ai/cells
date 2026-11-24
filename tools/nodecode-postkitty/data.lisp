;;;; data.lisp --- the platform table, the copy counting rules, the thread split.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every limit here is the original's data/platforms.yaml, checked by its
;;;; author on 2026-11-08 against each platform's own documentation; the
;;;; source urls stay in the original at data/platforms.yaml. A limit marked
;;;; in a target's :soft list blocks nothing and only advises.

(in-package #:nodecode-postkitty)

;;;; --- the table ------------------------------------------------------------

(defparameter *platforms*
  '(("instagram" "Instagram"
     (:text (:limit 2200 :unit "codepoints" :hashtags 5 :mentions 20)
      :targets (("instagram_reel" "video" 1080 1920
                 :duration-min 3 :duration-max 900 :bytes-max 1000000000
                 :containers ("mp4" "mov") :fps-min 23 :fps-max 60)
                ("instagram_feed" "image" 1080 1350 :formats ("jpg" "jpeg"))
                ("instagram_story" "image_or_video" 1080 1920
                 :duration-max 60 :soft (:duration-max)))))
    ("tiktok" "TikTok"
     (:text (:limit 2200 :unit "utf16")
      :targets (("tiktok" "video" 1080 1920
                 :containers ("mp4" "webm" "mov") :fps-min 23 :fps-max 60
                 :dimension-min 360 :dimension-max 4096
                 :duration-max 600 :bytes-max 4000000000))))
    ("youtube" "YouTube Shorts"
     (:text (:limit 100 :unit "codepoints" :soft (:limit))
      :targets (("youtube_shorts" "video" 1080 1920 :duration-max 180))))
    ("x" "X"
     (:text (:limit 280 :unit "x_weighted")
      :targets (("x_video" "video" 1920 1080 :duration-max 140 :bytes-max 536870912)
                ("x_image" "image" 1600 900
                 :formats ("jpg" "jpeg" "png" "gif") :bytes-max 5242880))))
    ("linkedin" "LinkedIn"
     (:text (:limit 3000 :unit "codepoints")
      :targets (("linkedin_video" "video" 1080 1350
                 :duration-min 3 :duration-max 900 :bytes-min 75000 :bytes-max 5000000000
                 :fps-min 10 :fps-max 60 :aspect-min 0.4167 :aspect-max 2.4)
                ("linkedin_image" "image" 1080 1080))))
    ("threads" "Threads"
     (:text (:limit 500 :unit "codepoints")
      :targets (("threads_video" "video" 1080 1920 :duration-max 300)
                ("threads_image" "image" 1080 1350))))
    ("bluesky" "Bluesky"
     (:text (:limit 300 :unit "graphemes" :bytes-max 3000)
      :targets (("bluesky_video" "video" 1080 1920
                 :bytes-max 300000000 :duration-max 600 :soft (:duration-max))
                ("bluesky_image" "image" 1080 1350 :bytes-max 2000000))))
    ("facebook" "Facebook"
     (:text (:limit 63206 :unit "codepoints" :soft (:limit))
      :targets (("facebook_reel" "video" 1080 1920)
                ("facebook_feed" "image" 1080 1350)))))
  "Every platform the cell knows, its copy rules and its targets.")

(defparameter *aliases*
  '(("twitter" . "x") ("ig" . "instagram") ("insta" . "instagram")
    ("yt" . "youtube") ("shorts" . "youtube") ("fb" . "facebook")
    ("bsky" . "bluesky"))
  "The names people type for a platform.")

(defun canonical-name (name)
  "NAME lowercased and trimmed, with an alias resolved." 
  (let ((name (string-downcase (nlk:trimmed (princ-to-string (or name ""))))))
    (or (cdr (assoc name *aliases* :test #'string=)) name)))

(defun platform-spec (name)
  "The table entry for platform NAME, or NIL."
  (assoc (canonical-name name) *platforms* :test #'string=))

(defun platform-names ()
  (mapcar #'first *platforms*))

(defun all-targets ()
  "Every target, each a plist of its own with :id, :platform and :limits."
  (loop for (name label spec) in *platforms*
        nconc (loop for (id kind width height . limits) in (getf spec :targets)
                    collect (append (list :id id :platform name :kind kind :width width :height height
                                          :label (format nil "~a ~a" label (substitute #\Space #\_ id)))
                                    (list :limits limits)))))

(defun target-spec (id)
  "The target named ID (its own id), or NIL."
  (find (canonical-name id) (all-targets) :key (lambda (target) (getf target :id)) :test #'string=))

(defun platform-of (name)
  "The platform a name or a target id names, or NIL."
  (let ((name (canonical-name name)))
    (cond ((platform-spec name) name)
          ((target-spec name) (getf (target-spec name) :platform))))) 

(defun targets-for (kinds wanted)
  "The targets that take one of KINDS, limited to the platform names and
 target ids in WANTED when it is non-empty."
  (let ((wanted (mapcar #'canonical-name wanted)))
    (remove-if-not (lambda (target)
                     (let ((kind (getf target :kind)))
                       (and (or (member kind kinds :test #'string=)
                                (and (string= kind "image_or_video")
                                     (intersection '("image" "video") kinds :test #'string=)))
                            (or (null wanted)
                                (member (getf target :id) wanted :test #'string=)
                                (member (getf target :platform) wanted :test #'string=)))))
                   (all-targets))))

;;;; --- counting -------------------------------------------------------------

(defconstant +x-url-length+ 23)
(defconstant +x-default-weight+ 2)

(defparameter *url-pattern* "(?i)(?:https?://|www\\.)[^\\s<>\"']+"
  "What counts as a link: a scheme or www., up to whitespace.")

(defparameter *trailing* ". , ! ? ; : ) ] } ' \""
  "Characters a trailing link does not keep.")

(defun url-spans (text)
  "The (START . END) spans of the links TEXT carries, oldest first."
  (let ((spans '()) (start 0))
    (loop
      (multiple-value-bind (from to) (ppcre:scan *url-pattern* text :start start)
        (unless from (return))
        (let ((end to))
          (loop while (and (> end from) (find (char text (1- end)) *trailing*)) do (decf end))
          (when (> end from) (push (cons from end) spans)))
        (setf start (max to (1+ from)))))
    (nreverse spans)))

(defun utf8-length (text)
  (length (sb-ext:string-to-octets text :external-format :utf-8)))

(defun utf16-length (text)
  (loop for char across text sum (if (> (char-code char) #xFFFF) 2 1)))

(defun x-weight (code)
  "twitter-text v3's weight for one code point: 1 in its light ranges, else 2."
  (if (or (<= 0 code 4351) (<= 8192 code 8205) (<= 8208 code 8223) (<= 8242 code 8247))
      1 +x-default-weight+))

(defun emoji-base-p (code)
  (or (<= #x1F000 code #x1FAFF)
      (<= #x1F1E6 code #x1F1FF)
      (<= #x2600 code #x27BF)
      (<= #x2300 code #x23FF)
      (<= #x2B00 code #x2BFF)
      (member code '(#x203C #x2049 #x2122 #x2139 #x3030 #x303D #x3297 #x3299))))

(defun emoji-end (text index)
  "The end of the emoji sequence starting at INDEX: one sequence, whatever
its modifiers, variation selectors and joins."
  (let ((length (length text))
        (index (1+ index)))
    (loop while (< index length) do
      (let ((code (char-code (char text index))))
        (cond ((= code #xFE0F) (incf index))
              ((<= #x1F3FB code #x1F3FF) (incf index))
              ((and (= code #x200D) (< (1+ index) length)
                    (emoji-base-p (char-code (char text (1+ index)))))
               (incf index 2))
              (t (return)))))
    index))

(defun x-weighted-length (text)
  "TEXT's length as X counts it: light ranges 1, other code points 2, a link
23, an emoji sequence 2."
  (let ((spans (url-spans text)) (total 0) (index 0) (length (length text)))
    (loop while (< index length) do
      (let ((span (assoc index spans)))
        (cond (span (incf total +x-url-length+) (setf index (cdr span)))
              ((emoji-base-p (char-code (char text index)))
               (incf total 2) (setf index (emoji-end text index)))
              (t (incf total (x-weight (char-code (char text index)))) (incf index)))))
    total))

(defun combining-p (code)
  "Whether CODE is a combining mark no grapheme cluster starts with, near
enough: the original asked the regex library's \\X and this rewrite does not
carry one."
  (or (<= #x0300 code #x036F) (<= #x0483 code #x0489) (<= #x0591 code #x05BD)
      (<= #x0610 code #x061A) (<= #x064B code #x065F) (<= #x0670 code #x0670)
      (<= #x06D6 code #x06ED) (<= #x0900 code #x0903) (<= #x093A code #x094F)
      (<= #x0951 code #x0957) (<= #x0962 code #x0963) (<= #x0981 code #x0983)
      (<= #x09BC code #x09CD) (<= #x0A01 code #x0A03) (<= #x0A3C code #x0A4D)
      (<= #x0B01 code #x0B03) (<= #x0B3C code #x0B4D) (<= #x0C00 code #x0C04)
      (<= #x0C3E code #x0C56) (<= #x0D00 code #x0D03) (<= #x0D3B code #x0D4D)
      (<= #x0E31 code #x0E4E) (<= #x0F71 code #x0F84) (<= #x102B code #x103E)
      (<= #x1AB0 code #x1AFF) (<= #x1DC0 code #x1DFF) (<= #x20D0 code #x20F0)
      (<= #xFE00 code #xFE0F) (<= #xFE20 code #xFE2F) (<= #x1F3FB code #x1F3FF)))

(defun grapheme-length (text)
  "TEXT's length in grapheme clusters: a base with its marks, variation
selectors, modifiers and joined cluster counts once."
  (let ((count 0) (index 0) (length (length text)))
    (loop while (< index length) do
      (incf count)
      (incf index)
      (loop while (and (< index length)
                       (let ((code (char-code (char text index))))
                         (or (combining-p code) (= code #x200D))))
            do (incf index)))
    count))

(defun count-text (text unit)
  "TEXT counted in UNIT, one of codepoints, utf16, x_weighted, graphemes,
utf8_bytes."
  (cond ((string= unit "x_weighted") (x-weighted-length text))
        ((string= unit "graphemes") (grapheme-length text))
        ((string= unit "utf16") (utf16-length text))
        ((string= unit "utf8_bytes") (utf8-length text))
        (t (length text))))

(defun count-tags (text mark)
  "How often TEXT carries MARK (# or @) starting a name."
  (let ((count 0) (index 0) (length (length text)))
    (loop while (< index length) do
      (if (and (char= (char text index) mark)
               (or (zerop index) (not (alphanumericp (char text (1- index)))))
               (< (1+ index) length)
               (let ((next (char text (1+ index))))
                 (or (alphanumericp next) (char= next #\_))))
          (progn
            (incf count)
            (incf index)
            (loop while (and (< index length)
                             (let ((char (char text index)))
                               (or (alphanumericp char) (char= char #\_))))
                  do (incf index)))
          (incf index)))
    count))

(defun text-stats (text)
  "One draft, counted every way any platform counts it."
  (list :chars (length text)
        :x-weighted (x-weighted-length text)
        :graphemes (grapheme-length text)
        :utf16 (utf16-length text)
        :utf8-bytes (utf8-length text)
        :hashtags (count-tags text #\#)
        :mentions (count-tags text #\@)
        :urls (length (url-spans text))))

;;;; --- fitting --------------------------------------------------------------

(defun platform-rules (name)
  "The copy rules of platform NAME as a plist."
  (let* ((spec (platform-spec name))
         (text (and spec (getf (third spec) :text))))
    (list :platform (canonical-name name)
          :limit (getf text :limit)
          :unit (or (getf text :unit) "codepoints")
          :soft (getf text :soft)
          :hashtags (getf text :hashtags)
          :mentions (getf text :mentions)
          :bytes-max (getf text :bytes-max))))

(defun split-thread (text unit limit)
  "TEXT as a numbered thread, every part inside LIMIT counted in UNIT; one
part when it is already inside."
  (let ((words (remove "" (ppcre:split "\\s+" text) :test #'string=))
        (parts '()) (current '()))
    (labels ((joined (words) (format nil "~{~a~^ ~}" (reverse words)))
             (emit () (when current (push (joined current) parts) (setf current '()))))
      (dolist (word words)
        (if (and current (> (count-text (joined (cons word current)) unit) (- limit 8)))
            (progn (emit) (push word current))
            (push word current)))
      (emit))
    (let ((parts (nreverse parts)))
      (if (<= (length parts) 1)
          parts
          (loop for part in parts for index from 1
                collect (format nil "~d/~d ~a" index (length parts) part))))))

(defun fit-report (name text &key (split t))
  "TEXT fitted to platform NAME: what it counts, what it overflows, what it
breaks, and a thread when the platform takes one."
  (let* ((rules (platform-rules name))
         (unit (getf rules :unit))
         (limit (getf rules :limit))
         (soft (getf rules :soft))
         (counted (count-text text unit))
         (hashtags (count-tags text #\#))
         (mentions (count-tags text #\@))
         (bytes (utf8-length text))
         (problems '())
         (advisory '()))
    (when (and limit (> counted limit))
      (if (member :limit soft) (push :limit advisory) (push :limit problems)))
    (when (and (getf rules :hashtags) (> hashtags (getf rules :hashtags)))
      (push :hashtags problems))
    (when (and (getf rules :mentions) (> mentions (getf rules :mentions)))
      (push :mentions problems))
    (when (and (getf rules :bytes-max) (> bytes (getf rules :bytes-max)))
      (push :bytes problems))
    (list :platform (getf rules :platform)
          :unit unit :limit limit :counted counted
          :overflow (and (plusp (- counted (or limit 0))) (- counted (or limit 0)))
          :hashtags hashtags :hashtags_max (getf rules :hashtags)
          :mentions mentions :mentions_max (getf rules :mentions)
          :bytes bytes :bytes_max (getf rules :bytes-max)
          :ok (null problems)
          :problems (nreverse problems)
          :advisory (nreverse advisory)
          :thread (when (and split (member (getf rules :platform) '("x" "threads" "bluesky") :test #'string=)
                            (member :limit problems))
                    (split-thread text unit limit)))))
