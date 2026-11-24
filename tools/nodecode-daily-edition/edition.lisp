;;;; edition.lisp --- the edition model: one story once, consensus leads, no reprints.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure functions: no network, no files, no model. An item is a plist
;;;; (:title :url :snippet :date :at :source :publisher); a source is
;;;; (:source NAME :items ITEMS); an edition is the plist BUILD-EDITION
;;;; answers, which RENDER-EDITION prints and EDITION-SUMMARY files.

(in-package #:nodecode-daily-edition)

(defvar *now* nil
  "When bound, the universal time an edition is dated by: a test pins it.")

;;; --- words -------------------------------------------------------------------------

(defparameter +stopwords+
  '("a" "an" "and" "as" "at" "be" "by" "for" "from" "in" "is" "it" "its" "of" "on" "or"
    "that" "the" "to" "was" "were" "will" "with" "after" "before" "over" "under" "this"
    "these" "those" "has" "have" "had" "not" "but" "are" "am" "been" "into")
  "Words a headline shares with every other headline, so they prove nothing.")

(defparameter +actions+
  '(("approve" . "decide") ("approves" . "decide") ("approved" . "decide")
    ("pass" . "decide") ("passes" . "decide") ("passed" . "decide")
    ("adopt" . "decide") ("adopts" . "decide") ("adopted" . "decide")
    ("vote" . "decide") ("votes" . "decide") ("voted" . "decide")
    ("clear" . "decide") ("clears" . "decide") ("cleared" . "decide")
    ("back" . "decide") ("backs" . "decide") ("backed" . "decide")
    ("decide" . "decide") ("decides" . "decide") ("decided" . "decide")
    ("reject" . "refuse") ("rejects" . "refuse") ("rejected" . "refuse")
    ("block" . "refuse") ("blocks" . "refuse") ("blocked" . "refuse")
    ("veto" . "refuse") ("vetoes" . "refuse") ("vetoed" . "refuse")
    ("deny" . "refuse") ("denies" . "refuse") ("denied" . "refuse")
    ("delay" . "delay") ("delays" . "delay") ("delayed" . "delay")
    ("postpone" . "delay") ("postponed" . "delay") ("shelve" . "delay")
    ("shelved" . "delay") ("suspend" . "delay") ("suspended" . "delay")
    ("rise" . "up") ("rises" . "up") ("rising" . "up") ("climb" . "up") ("climbs" . "up")
    ("climbed" . "up") ("increase" . "up") ("increases" . "up") ("soar" . "up") ("soars" . "up")
    ("gain" . "up") ("gains" . "up")
    ("fall" . "down") ("falls" . "down") ("fell" . "down") ("drop" . "down") ("drops" . "down")
    ("dropped" . "down") ("decline" . "down") ("declines" . "down") ("cut" . "down")
    ("cuts" . "down") ("plunge" . "down") ("plunges" . "down")
    ("launch" . "begin") ("launches" . "begin") ("launched" . "begin") ("unveil" . "begin")
    ("unveils" . "begin") ("open" . "begin") ("opens" . "begin") ("opened" . "begin")
    ("release" . "begin") ("releases" . "begin") ("start" . "begin") ("starts" . "begin")
    ("close" . "end") ("closes" . "end") ("closed" . "end") ("shut" . "end") ("shuts" . "end")
    ("halt" . "end") ("halts" . "end")
    ("warn" . "warn") ("warns" . "warn") ("warned" . "warn") ("warning" . "warn")
    ("alert" . "warn") ("alerts" . "warn"))
  "Verbs that mean one thing: a headline that says approved is not the one that says delayed.")

(defparameter +months+
  '("january" "february" "march" "april" "may" "june" "july" "august" "september"
    "october" "november" "december"))

(defparameter +weekdays+
  '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defparameter +tracking+
  '("fbclid" "gclid" "mc_cid" "mc_eid" "ref" "ref_src" "source" "igshid")
  "Query members that name where a click came from, not a story.")

(defun blank-p (text)
  (or (null text) (zerop (length (nlk:trimmed (princ-to-string text))))))

(defun prefix-p (prefix text)
  "TEXT begins with PREFIX."
  (let ((n (length prefix)))
    (and (<= n (length text)) (string= prefix text :end2 n))))

(defun suffix-p (suffix text)
  "TEXT ends with SUFFIX."
  (let ((n (length suffix)) (m (length text)))
    (and (<= n m) (string= suffix text :start2 (- m n)))))

;;; --- the canonical link --------------------------------------------------------------

(defun tracking-param-p (member)
  "MEMBER, a query's key=value, is tracking rather than the story."
  (let* ((key (string-downcase (subseq member 0 (or (position #\= member) (length member))))))
    (or (nodecode-daily-edition::prefix-p "utm_" key)
        (member key +tracking+ :test #'equal))))

(defun canonical-url (raw)
  "RAW as one story's own address: no fragment, host in lowercase, no
tracking member, no trailing slash. \"\" when it is no http(s) address."
  (let* ((text (nlk:trimmed (or raw "")))
         (hash (position #\# text)))
    (when hash (setf text (subseq text 0 hash)))
    (let* ((mark (search "://" text))
           (scheme (and mark (string-downcase (subseq text 0 mark))))
           (rest (and mark (subseq text (+ mark 3))))
           (slash (and rest (position #\/ rest)))
           (authority (and rest (subseq rest 0 slash)))
           (tail (if slash (subseq rest slash) "/")))
      (unless (and (= (length scheme) (length "https"))
                   (member scheme '("http" "https") :test #'equal)
                   authority
                   (plusp (length authority)))
        (return-from canonical-url ""))
      (let* ((question (position #\? tail))
             (path (if question (subseq tail 0 question) tail))
             (query (and question (subseq tail (1+ question))))
             (params (and query (remove-if #'tracking-param-p
                                           (remove "" (uiop:split-string query :separator '(#\&))
                                                   :test #'equal))))
             (path (string-right-trim "/" path)))
        (format nil "~a://~a~a~@[?~{~a~^&~}~]"
                scheme (string-downcase authority) (or (and (plusp (length path)) path) "/")
                (and params params))))))

;;; --- what a headline says ------------------------------------------------------------

(defun words-of (text)
  "TEXT's words, downcased, punctuation read as a space."
  (let ((out (make-string-output-stream))
        (text (string-downcase (or text ""))))
    (loop for char across text
          do (cond ((or (alphanumericp char) (char= char #\_)) (write-char (if (char= char #\_) #\Space char) out))
                   (t (write-char #\Space out))))
    (uiop:split-string (get-output-stream-string out) :separator '(#\Space #\Tab #\Newline))))

(defun title-key (title)
  "TITLE with its punctuation and its words that prove nothing taken out."
  (format nil "~{~a~^ ~}"
          (remove-if (lambda (word) (or (zerop (length word)) (member word +stopwords+ :test #'equal)))
                     (words-of title))))

(defun stem (word)
  "WORD with a plural or a participle's tail taken off, so 'plans' and
'plan' are one word. Deliberately crude: this compares headlines, it does
not parse English."
  (if (<= (length word) 4)
      word
      (let ((word (or (loop for suffix in '("ing" "ed" "es" "s")
                             when (and (> (length word) (+ (length suffix) 2))
                                       (nodecode-daily-edition::suffix-p suffix word))
                               return (subseq word 0 (- (length word) (length suffix))))
                       word)))
        (let ((n (length word)))
          (if (and (> n 2) (char= (char word (1- n)) (char word (- n 2)))
                   (not (find (char word (1- n)) "aeiou")))
              (subseq word 0 (1- n))
              word)))))

(defun tokens (text)
  "The distinct stems TEXT's content words reduce to."
  (remove-duplicates (mapcar #'stem (uiop:split-string (title-key text) :separator '(#\Space)))
                     :test #'equal))

(defun action-families (title)
  (let ((out '()))
    (dolist (word (uiop:split-string (title-key title) :separator '(#\Space)))
      (let ((family (cdr (assoc word +actions+ :test #'equal))))
        (when family (pushnew family out :test #'equal))))
    out))

(defun years-of (title)
  (let ((out '()) (start 0) (text (or title "")))
    (loop
      (let ((at (search "20" text :start2 start)))
        (unless at (return))
        (let ((end (+ at 4)))
          (when (and (<= end (length text))
                     (every #'digit-char-p (subseq text at end))
                     (or (= at 0) (not (alphanumericp (char text (1- at)))))
                     (or (= end (length text)) (not (alphanumericp (char text end)))))
            (push (subseq text at end) out)))
        (setf start (1+ at))))
    out))

(defun conflicting-years-p (a b)
  (let ((left (years-of a)) (right (years-of b)))
    (and left right (null (intersection left right :test #'equal)))))

(defun compatible-actions-p (a b)
  (let ((left (action-families a)) (right (action-families b)))
    (or (null left) (null right) (intersection left right :test #'equal))))

(defun content-bigrams (title)
  (let ((words (remove-if (lambda (w) (< (length w) 4))
                          (uiop:split-string (title-key title) :separator '(#\Space)))))
    (loop for (a b) on words while b collect (format nil "~a ~a" a b))))

(defun same-story-p (a b)
  "Whether items A and B are one story. One masthead never cross-posts
itself; a different year or a contradictory verb keeps two filings apart."
  (let ((url-a (getf a :url)) (url-b (getf b :url)))
    (when (and url-a (equal url-a url-b))
      (return-from same-story-p t)))
  (when (conflicting-years-p (getf a :title) (getf b :title))
    (return-from same-story-p nil))
  (unless (compatible-actions-p (getf a :title) (getf b :title))
    (return-from same-story-p nil))
  (let* ((left (getf a :tokens)) (right (getf b :tokens))
         (shared (length (intersection left right :test #'equal)))
         (min-size (min (length left) (length right))))
    (when (or (zerop shared) (zerop min-size))
      (return-from same-story-p nil))
    (let* ((containment (/ shared min-size))
           (jaccard (/ shared (- (+ (length left) (length right)) shared)))
           (score (+ (* 0.65 containment) (* 0.35 jaccard))))
      (when (and (>= score 0.66) (>= shared 3))
        (return-from same-story-p t)))
    (when (intersection (getf a :publishers) (getf b :publishers) :test #'equal)
      (return-from same-story-p nil))
    (when (and (>= shared 2)
               (intersection (content-bigrams (getf a :title)) (content-bigrams (getf b :title))
                             :test #'equal))
      (return-from same-story-p t))
    (and (>= containment 0.9) (>= min-size 3))))

;;; --- one story once -----------------------------------------------------------------

(defun merge-group (group)
  "GROUP's items as one story, every publisher and link credited."
  (let ((story (copy-list (first group))))
    (dolist (item (rest group) story)
      (dolist (source (getf item :sources))
        (unless (member source (getf story :sources) :test #'equal)
          (setf (getf story :sources) (append (getf story :sources) (list source)))))
      (dolist (publisher (getf item :publishers))
        (when (and (not (blank-p publisher))
                   (not (member publisher (getf story :publishers) :test #'equal)))
          (setf (getf story :publishers) (append (getf story :publishers) (list publisher)))))
      (dolist (entry (getf item :also))
        (unless (assoc (cdr entry) (getf story :also) :test #'equal)
          (setf (getf story :also) (append (getf story :also) (list entry)))))
      (when (> (length (or (getf item :snippet) "")) (length (or (getf story :snippet) "")))
        (setf (getf story :snippet) (getf item :snippet)))
      (when (and (getf item :at) (or (null (getf story :at)) (< (getf item :at) (getf story :at))))
        (setf (getf story :at) (getf item :at))
        (setf (getf story :date) (getf item :date))))))

(defun cluster (items)
  "ITEMS gathered into stories, clustering transitively: a story told three
ways is one story."
  (let* ((n (length items))
         (parent (make-array n))
         (vector (coerce items 'vector)))
    (dotimes (i n) (setf (aref parent i) i))
    (labels ((root (i)
               (loop while (/= (aref parent i) i) do (setf i (aref parent i)))
               i))
      (dotimes (i n)
        (loop for j from (1+ i) below n
              when (same-story-p (aref vector i) (aref vector j))
                do (let ((ri (root i)) (rj (root j)))
                     (unless (= ri rj)
                       (setf (aref parent (max ri rj)) (min ri rj))))))
      (let ((groups (make-hash-table)) (out '()))
        (dotimes (i n) (push (aref vector i) (gethash (root i) groups)))
        (dotimes (i n)
          (let ((group (gethash (root i) groups)))
            (when group
              (setf (gethash (root i) groups) nil)
              (push (merge-group (nreverse group)) out))))
        (nreverse out)))))

(defun story-line (story)
  "The publishers credited on STORY, in the order they arrived."
  (or (remove-if #'blank-p (getf story :publishers))
      (remove-if #'blank-p (getf story :sources))
      (list "unknown")))

(defun rank-stories (stories)
  "Consensus first: a story more mastheads carried outranks one that fewer
did, a starred watchword outranks both, and recency breaks the tie."
  (stable-sort (copy-list stories)
               (lambda (a b)
                 (cond ((and (getf a :pinned) (not (getf b :pinned))) t)
                       ((and (getf b :pinned) (not (getf a :pinned))) nil)
                       ((/= (length (story-line a)) (length (story-line b)))
                        (> (length (story-line a)) (length (story-line b))))
                       (t (> (or (getf a :at) 0) (or (getf b :at) 0)))))))

(defun matches-watchword-p (story watchwords)
  (let ((words (remove-if #'blank-p watchwords)))
    (and words
         (let ((haystack (union (getf story :tokens)
                                (tokens (or (getf story :snippet) "")) :test #'equal)))
           (some (lambda (word) (member (stem (string-downcase word)) haystack :test #'equal))
                 words)))))

;;; --- the edition --------------------------------------------------------------------

(defun iso-date-of (universal)
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun date-label-of (universal)
  (multiple-value-bind (second minute hour day month year weekday) (decode-universal-time universal)
    (declare (ignore second minute hour))
    (format nil "~a, ~d ~a ~d" (nth weekday +weekdays+) day
            (string-capitalize (nth (1- month) +months+)) year)))

(defun build-edition (&key sources previous now (max-stories 20) muted watchwords)
  "The front page for NOW from SOURCES, withholding what PREVIOUS already
printed. => an edition plist."
  (let* ((now (or now (get-universal-time)))
         (muted (remove-if #'blank-p muted))
         (active (remove-if (lambda (source)
                              (member (string-downcase (or (getf source :source) ""))
                                      muted :test #'equal))
                            sources))
         (items '()))
    (dolist (source active)
      (dolist (raw (getf source :items))
        (let* ((url (canonical-url (getf raw :url)))
               (title (nlk:trimmed (or (getf raw :title) ""))))
          (when (and (plusp (length url)) (plusp (length title)))
            (let ((publisher (or (getf raw :publisher) (getf source :source))))
              (push (list :title title :url url
                          :snippet (or (getf raw :snippet) "")
                          :date (or (getf raw :date) "")
                          :at (or (getf raw :at) 0)
                          :source (getf source :source)
                          :sources (list (getf source :source))
                          :publisher publisher
                          :publishers (list publisher)
                          :also (list (cons (getf source :source) url))
                          :tokens (tokens title))
                    items))))))
    (let* ((everything (mapcar (lambda (story)
                                 (list* :pinned (and (matches-watchword-p story watchwords) t)
                                        story))
                               (cluster (nreverse items))))
           (withheld-keys (getf previous :printed))
           (fresh '()) (withheld '()) (withheld-count 0))
      (dolist (story everything)
        (if (or (member (getf story :url) withheld-keys :test #'equal)
                (member (title-key (getf story :title)) withheld-keys :test #'equal))
            (progn (incf withheld-count) (push story withheld))
            (push story fresh)))
      (let* ((ordered (rank-stories (nreverse fresh)))
             (printed (if (> (length ordered) max-stories)
                          (subseq ordered 0 max-stories)
                          ordered))
             (lead (first printed))
             (sections '()))
        (dolist (story (rest printed))
          (let* ((source (or (first (getf story :sources)) ""))
                 (section (assoc source sections :test #'equal)))
            (unless section
              (setf section (list source) sections (append sections (list section))))
            (setf (cdr section) (append (cdr section) (list story)))))
        (let ((keys (loop for story in printed
                           append (list (getf story :url) (title-key (getf story :title))))))
          (list :number (1+ (or (getf previous :number) 0))
                :iso-date (iso-date-of now)
                :date-label (date-label-of now)
                :built-at (format nil "~aT~2,'0d:~2,'0d:~2,'0d"
                                  (iso-date-of now)
                                  (nth-value 2 (decode-universal-time now))
                                  (nth-value 1 (decode-universal-time now))
                                  (nth-value 0 (decode-universal-time now)))
                :lead lead
                :sections (mapcar (lambda (section)
                                    (list :source (first section) :stories (rest section)))
                                  sections)
                :stories printed
                :printed (subseq keys 0 (min 160 (length keys)))
                :printed-stories (mapcar (lambda (story)
                                           (list (getf story :url) (title-key (getf story :title))))
                                         printed)
                :returned (append keys (loop for story in withheld
                                             append (list (getf story :url)
                                                          (title-key (getf story :title)))))
                :withheld withheld-count
                :omitted (max 0 (- (length ordered) (length printed)))
                :cross-posted (count-if (lambda (story) (> (length (story-line story)) 1)) printed)
                :pinned-count (count-if (lambda (story) (getf story :pinned)) printed)
                :source-count (count-if (lambda (source) (getf source :items)) sources)
                :story-count (length printed)
                :muted-count (length muted)))))))

(defun edition-diff (current previous)
  "What changed since PREVIOUS: a plist with the one line to print."
  (unless previous
    (return-from edition-diff
      (list :first t :fresh (getf current :story-count) :carried 0 :dropped 0
            :line "First edition. Everything here is new.")))
  (let* ((returned (getf current :returned))
         (pairs (or (getf previous :printed-stories)
                    (let ((flat (getf previous :printed)))
                      (loop for (a b) on flat by #'cddr collect (list a (or b ""))))))
         (dropped (count-if (lambda (pair)
                              (and (not (member (first pair) returned :test #'equal))
                                   (not (member (second pair) returned :test #'equal))))
                            pairs))
         (parts (list (format nil "~d new since No. ~a"
                              (getf current :story-count) (getf previous :number)))))
    (when (plusp (or (getf current :withheld) 0))
      (push (format nil "~d still running" (getf current :withheld)) parts))
    (when (plusp dropped)
      (push (format nil "~d gone from the feeds" dropped) parts))
    (list :first nil :fresh (getf current :story-count) :carried (getf current :withheld)
          :dropped dropped :line (format nil "~{~a~^ · ~}" (reverse parts)))))

(defun edition-summary (edition)
  "What the archive keeps of EDITION: its number, its date, its lead and
enough to recognize a reprint."
  (let ((lead (getf edition :lead)))
    (list :number (getf edition :number)
          :iso-date (getf edition :iso-date)
          :date-label (getf edition :date-label)
          :built-at (getf edition :built-at)
          :lead-title (or (getf lead :title) "")
          :lead-url (or (getf lead :url) "")
          :headlines (loop for story in (subseq (getf edition :stories) 0
                                                 (min 6 (length (getf edition :stories))))
                            collect (list (getf story :title) (getf story :url)))
          :printed-stories (subseq (getf edition :printed-stories) 0
                                   (min 40 (length (getf edition :printed-stories))))
          :printed (subseq (getf edition :printed) 0 (min 80 (length (getf edition :printed))))
          :story-count (getf edition :story-count)
          :cross-posted (getf edition :cross-posted))))

(defun merge-archive (archive summary &optional (limit 7))
  "ARCHIVE with SUMMARY filed as the newest issue, one issue a day, LIMIT kept."
  (let ((kept (if summary
                  (cons summary (remove (getf summary :iso-date) archive
                                        :key (lambda (entry) (getf entry :iso-date))
                                        :test #'equal))
                  archive)))
    (subseq (stable-sort kept
                         (lambda (a b)
                           (let ((left (getf a :iso-date)) (right (getf b :iso-date)))
                             (cond ((equal left right) (> (or (getf a :number) 0) (or (getf b :number) 0)))
                                   (t (string> left right))))))
            0 (min (max 1 limit) (length kept)))))

;;; --- the page -----------------------------------------------------------------------

(defun render-edition (edition standfirst errors)
  "EDITION as the plain-text front page a tool or a cron job answers with."
  (let ((head (format nil "DAILY EDITION — No. ~a — ~a"
                      (getf edition :number) (getf edition :date-label)))
        (lines '()))
    (if (zerop (getf edition :story-count))
        (progn
          (push head lines)
          (push "" lines)
          (push (getf standfirst :line) lines)
          (when (plusp (getf edition :withheld))
            (push (format nil "~d stor~:[ies~;y~] already ran in an earlier edition and ~
                               was withheld."
                          (getf edition :withheld) (= 1 (getf edition :withheld)))
                  lines)))
        (progn
          (push head lines)
          (push (getf standfirst :line) lines)
          (push (format nil "~d stories from ~d ~a · ~d cross-posted · ~d withheld · ~d held over"
                        (getf edition :story-count) (getf edition :source-count)
                        (if (= 1 (getf edition :source-count)) "source" "sources")
                        (getf edition :cross-posted) (getf edition :withheld)
                        (getf edition :omitted))
                lines)
          (push "" lines)
          (let ((lead (getf edition :lead)))
            (push (format nil "LEAD: ~a" (getf lead :title)) lines)
            (push (format nil "  ~{~a~^ · ~} — ~a" (story-line lead) (getf lead :url)) lines)
            (unless (blank-p (getf lead :snippet))
              (push (format nil "  ~a" (nlk:clip (getf lead :snippet) 300)) lines)))
          (dolist (section (getf edition :sections))
            (push "" lines)
            (push (string-upcase (getf section :source)) lines)
            (dolist (story (getf section :stories))
              (push (format nil "  ~a~a — ~{~a~^ · ~}"
                            (if (getf story :pinned) "★ " "")
                            (getf story :title) (story-line story))
                    lines)
              (push (format nil "    ~a" (getf story :url)) lines)))))
    (when errors
      (push "" lines)
      (push "Sources that could not be read:" lines)
      (dolist (error errors)
        (push (format nil "- ~a" error) lines)))
    (format nil "~{~a~^~%~}" (nreverse lines))))
