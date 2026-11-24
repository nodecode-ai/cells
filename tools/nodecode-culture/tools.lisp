;;;; tools.lisp --- a name resolved, a brief, the ranked gaps, the coverage list.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin answers JSON built from two bundled research files.
;;;; The same data and the same arithmetic live in data.lisp, and every answer
;;;; here is the text a person or the model reads: a brief is one block of
;;;; labelled lines, a gap analysis is a ranked list carrying the misread each
;;;; gap risks and the move that answers it. Nothing is looked up on the
;;;; network and nothing is written down, so the same inputs answer the same
;;;; text every time.
;;;;
;;;; A name is placed the way the original places one: exact, then a common
;;;; alias, then a containment ("south korean market"), then a close spelling.
;;;; What it cannot place comes back as suggestions, never as a guess.

(in-package #:nodecode-culture)

;;; --- the words -------------------------------------------------------------------

(defparameter *alias-table*
  '(("usa" . "United States") ("us" . "United States") ("u s" . "United States")
    ("u s a" . "United States") ("america" . "United States")
    ("united states of america" . "United States") ("states" . "United States")
    ("uk" . "United Kingdom") ("u k" . "United Kingdom") ("britain" . "United Kingdom")
    ("great britain" . "United Kingdom") ("england" . "United Kingdom")
    ("scotland" . "United Kingdom")
    ("uae" . "United Arab Emirates") ("emirates" . "United Arab Emirates")
    ("dubai" . "United Arab Emirates") ("the emirates" . "United Arab Emirates")
    ("korea" . "South Korea") ("republic of korea" . "South Korea") ("rok" . "South Korea")
    ("holland" . "Netherlands") ("the netherlands" . "Netherlands")
    ("czech republic" . "Czechia") ("czech rep" . "Czechia")
    ("burma" . "Myanmar")
    ("lao" . "Laos") ("lao pdr" . "Laos") ("laos pdr" . "Laos")
    ("lao peoples democratic republic" . "Laos")
    ("viet nam" . "Vietnam")
    ("hongkong" . "Hong Kong") ("hk" . "Hong Kong") ("hong kong sar" . "Hong Kong")
    ("saudi" . "Saudi Arabia") ("ksa" . "Saudi Arabia")
    ("kingdom of saudi arabia" . "Saudi Arabia")
    ("prc" . "China") ("pr china" . "China") ("peoples republic of china" . "China")
    ("russia federation" . "Russia") ("russian federation" . "Russia")
    ("turkiye" . "Turkey") ("deutschland" . "Germany") ("espana" . "Spain")
    ("nihon" . "Japan") ("nippon" . "Japan")
    ("suid afrika" . "South Africa") ("rsa" . "South Africa")
    ("new york" . "United States"))
  "Common spellings, codes and cities a call may use, as
(normalized name . canonical name).")

(defparameter *task-weights*
  '(("negotiation" ("trust" . 1.7) ("decide" . 1.4) ("dis" . 1.3) ("comm" . 1.2)
                   ("sched" . 1.1))
    ("first_meeting" ("comm" . 1.5) ("trust" . 1.4) ("lead" . 1.3) ("eval" . 1.1))
    ("management" ("lead" . 1.7) ("eval" . 1.5) ("dis" . 1.5))
    ("communication" ("comm" . 1.7) ("eval" . 1.4) ("dis" . 1.3))
    ("payment" ("trust" . 1.6) ("sched" . 1.4) ("decide" . 1.2))
    ("trip" ("sched" . 1.5) ("lead" . 1.3) ("trust" . 1.2) ("comm" . 1.1)))
  "What a task makes matter more: a task family and the Meyer scales it
weights, the weight after each scale's key.")

(defparameter *task-hints*
  '(("negotiation" . "Anchor patiently, keep warmth while holding position, and remember a signed term sheet is the START of the relationship in relationship-based cultures -- maintenance calls are part of the deal.")
    ("first_meeting" . "Bring your most senior credible person, respect titles and seating, let the host set pace and small-talk length, and exchange contacts with ceremony where expected.")
    ("management" . "Give feedback privately and indirectly in high-context cultures (public correction causes resignations), and use one-on-ones to learn what the group will not say openly.")
    ("communication" . "Match directness to the host's Evaluating position: soften or sharpen your emails, put conclusions first or last, and follow verbal agreements with gentle written summaries.")
    ("payment" . "In relationship-first cultures payment follows personal confidence more than invoice terms: keep warm senior-to-senior contact, expect schedule slip, and do not open with an escalation threat.")
    ("trip" . "Confirm every appointment the day before, build slack for late starts and extended hospitality, and never cut a courtesy ritual short to save time -- the ritual IS the business."))
  "One line per task family: the paragraph the original's task_hint carries.")

(defparameter *task-aliases*
  '(("negotiate" . "negotiation") ("deal" . "negotiation") ("sales" . "negotiation")
    ("contract" . "negotiation")
    ("meeting" . "first_meeting") ("visit" . "first_meeting") ("intro" . "first_meeting")
    ("manage" . "management") ("feedback" . "management") ("team" . "management")
    ("hiring" . "management")
    ("email" . "communication") ("message" . "communication") ("messaging" . "communication")
    ("collect" . "payment") ("invoice" . "payment")
    ("travel" . "trip") ("gift" . "trip"))
  "The words a call may use for a task family.")

(defparameter *gap-risks*
  '(("comm" ("higher" . "Host reads messages through context, relationships and what is NOT said. Your explicit directness may read as blunt or naive; their yes may mean only I hear you.")
            ("lower" . "Host expects explicit, precise, written communication. Your implied meanings and softened phrasing read as evasive or unclear."))
    ("eval" ("higher" . "Host gives negative feedback indirectly, through upgraders, private channels or third parties. Direct criticism -- even of work, not people -- causes public loss of face.")
            ("lower" . "Host gives blunt negative feedback as professional respect and expects the same back. Softened hedging reads as unclear or even dishonest."))
    ("lead" ("higher" . "Host respects steep hierarchy: titles, seniority and protocol. Bypassing the senior person or treating the boss as a peer damages the relationship.")
            ("lower" . "Host runs flat: a junior engineer may contradict the CEO and first names are immediate. Formality reads as stiff or evasive."))
    ("decide" ("higher" . "The boss decides, quickly and personally. Endless group alignment looks weak; the deal lives or dies with one person.")
              ("lower" . "Decisions are consensual, aligned before the meeting. Pressure for a fast decision feels aggressive and forces a face-losing answer."))
    ("trust" ("higher" . "Host builds trust through meals, time, family talk and favors; contracts come AFTER confidence. Task-first behavior reads as cold and untrustworthy.")
             ("lower" . "Host builds trust through competence and delivery; dinners do not substitute for credentials. Relationship-first approaches can read as time-wasting."))
    ("dis" ("higher" . "Host avoids open disagreement to preserve harmony. A devil's-advocate debating style reads as hostility; silence in the meeting hides real objections.")
           ("lower" . "Host treats open disagreement as intellectual honesty and engagement. Harmony-preserving silence reads as having no ideas or no spine."))
    ("sched" ("higher" . "Host treats time as elastic: meetings start late, run long, and deadlines are aspirations. Clock discipline can read as distrust.")
             ("lower" . "Host treats the agenda and the clock as a contract. Lateness, overruns and reopened schedules read as disrespect or incompetence."))
    ("pers" ("higher" . "Host wants principles and method first, the why and the theory before any recommendation. Leading with your conclusion reads as unsophisticated.")
            ("lower" . "Host wants the answer up front: executive summary, then support. A theory-first build-up reads as waffle.")))
  "The misread a large gap risks on each scale, when the host is HIGHER or
LOWER than home.")

(defparameter *adaptations*
  '(("comm" ("higher" . "Slow down, confirm understanding through a trusted intermediary, put key points in writing AFTER the meeting, and read silence and hesitation as signals.")
            ("lower" . "Say exactly what you mean in writing; state conclusions and deadlines plainly; do not rely on shared context."))
    ("eval" ("higher" . "Wrap criticism, deliver hard messages privately or through a senior intermediary, praise publicly and correct privately.")
            ("lower" . "Take blunt feedback as engagement, not offence; answer criticism with facts, not emotion; be specific about problems."))
    ("lead" ("higher" . "Address and defer to the most senior person, use titles, send your own most-senior representative, and never skip levels.")
            ("lower" . "Drop the ceremony, engage everyone in the room as an equal, and expect decisions to be challenged on merit regardless of rank."))
    ("decide" ("higher" . "Identify and cultivate the single decision-maker, get their private blessing, and expect fast verbal decisions that paper follows later.")
              ("lower" . "Socialize the idea with each stakeholder before the meeting, accept slow pre-alignment, and move fast once it is decided."))
    ("trust" ("higher" . "Invest relationship time BEFORE negotiating terms: multiple meals, personal questions, patience; count months, not meetings.")
             ("lower" . "Lead with track record, references, data and clean contracts; keep meetings efficient; deliver early to build credit."))
    ("dis" ("higher" . "Do not push for public debate; surface objections one on one afterwards; read warm agreement as politeness and verify through allies.")
           ("lower" . "State your position and defend it with energy; do not take challenge personally; a heated exchange is often the start of the deal."))
    ("sched" ("higher" . "Build two to three times the schedule slack, confirm everything the day before, keep warm personal follow-up, and never show visible impatience.")
             ("lower" . "Arrive ten minutes early, send agendas ahead, end on time, and treat deadlines as binding promises."))
    ("pers" ("higher" . "Present the framework, method and reasoning first; let the conclusion arrive as a logical consequence; expect longer analysis phases.")
            ("lower" . "Lead with the recommendation and headline numbers; keep methodology in an appendix; expect interruptions asking for the answer.")))
  "The move that answers each risk: the same keys as *GAP-RISKS*.")

;;; --- names -------------------------------------------------------------------------

(defparameter *accented-letters*
  '(("á" . "a") ("à" . "a") ("â" . "a") ("ä" . "a") ("ã" . "a") ("å" . "a")
    ("é" . "e") ("è" . "e") ("ê" . "e") ("ë" . "e")
    ("í" . "i") ("ì" . "i") ("î" . "i") ("ï" . "i") ("ı" . "i")
    ("ó" . "o") ("ò" . "o") ("ô" . "o") ("ö" . "o") ("õ" . "o") ("ø" . "o")
    ("ú" . "u") ("ù" . "u") ("û" . "u") ("ü" . "u")
    ("ñ" . "n") ("ç" . "c") ("ß" . "ss") ("ş" . "s") ("æ" . "ae"))
  "The accented letters a name or a query may carry, and the plain letters
each folds to.")

(defparameter *max-input* 200
  "How much of a caller's string is used. A country, a task or a query never
legitimately runs longer, and clipping keeps an echo of a bad argument small.")

(defun clip (raw)
  "RAW as a string, at most *MAX-INPUT* characters, noting when it was longer."
  (let ((text (if (stringp raw) raw (princ-to-string (or raw "")))))
    (if (<= (length text) *max-input*)
        text
        (format nil "~a...[~d chars total]" (subseq text 0 *max-input*) (length text)))))

(defun fold-letter (char)
  "CHAR, or the plain letters it is an accent of."
  (let ((pair (find (string char) *accented-letters* :key #'car :test #'string=)))
    (if pair (cdr pair) (string char))))

(defun reduce-spaces (text)
  "TEXT with every run of spaces reduced to one."
  (with-output-to-string (out)
    (loop for char across text
          for previous = nil then char
          unless (and (char= char #\space) previous (char= previous #\space))
            do (write-char char out))))

(defun normalize-name (text)
  "TEXT lowercased, accents folded, punctuation dropped and spaces collapsed:
the key a country, a task or a query is looked up by."
  (let ((out (make-string-output-stream)))
    (loop for char across (string-downcase (or text ""))
          do (let ((folded (fold-letter char)))
               (cond ((every (lambda (one) (or (char<= #\a one #\z) (char<= #\0 one #\9)))
                             (coerce folded 'list))
                      (write-string folded out))
                     ((member char '(#\space #\tab #\newline #\return))
                      (write-char #\space out)))))
    (string-trim " " (reduce-spaces (get-output-stream-string out)))))

(defvar *country-index* nil
  "((normalized name . canonical name) ...), built on first use.")

(defun country-index ()
  (or *country-index*
      (setf *country-index*
            (loop for (name . entry) in *countries*
                  collect (cons (normalize-name name) name)))))

(defun country-entry (name)
  "The record of the country NAME, or NIL."
  (cdr (assoc name *countries* :test #'string=)))

(defun country-names ()
  "Every country covered, sorted."
  (sort (mapcar #'car *countries*) #'string<))

(defun first-names (&optional (count 6))
  (let ((names (country-names)))
    (subseq names 0 (min count (length names)))))

(defun canonical (norm)
  "The country whose normalized name is NORM, or NIL."
  (cdr (assoc norm (country-index) :test #'string=)))

(defun without-the (norm)
  "NORM without a leading the, when it has one."
  (if (and (> (length norm) 4) (string= "the " norm :end2 4))
      (subseq norm 4)
      norm))

(defun edit-distance (a b)
  "How many insertions, deletions and substitutions turn A into B."
  (let* ((n (length a))
         (m (length b))
         (previous (make-array (1+ m)))
         (current (make-array (1+ m))))
    (loop for j from 0 to m do (setf (aref previous j) j))
    (loop for i from 1 to n
          do (setf (aref current 0) i)
             (loop for j from 1 to m
                   for cost = (if (char= (char a (1- i)) (char b (1- j))) 0 1)
                   do (setf (aref current j)
                            (min (1+ (aref previous j))
                                 (1+ (aref current (1- j)))
                                 (+ (aref previous (1- j)) cost))))
             (rotatef previous current))
    (aref previous m)))

(defun similarity (a b)
  "How close two normalized names are, from 0 to 1, by edit distance."
  (let ((longest (max (length a) (length b) 1)))
    (/ (float (- longest (edit-distance a b)) 1.0) longest)))

(defun close-matches (norm count)
  "Up to COUNT covered countries spelled close to NORM, closest first."
  (let ((scored (loop for (candidate . name) in (country-index)
                      for score = (similarity norm candidate)
                      when (>= score 0.72) collect (cons score name))))
    (mapcar #'cdr (subseq (sort scored #'> :key #'car) 0 (min count (length scored))))))

(defun resolve-country (raw)
  "RAW as a covered country. => (values NAME SUGGESTIONS): the canonical name
when it resolves, otherwise NIL and what to try instead."
  (let* ((norm (normalize-name (clip (or raw ""))))
         (exact (and (plusp (length norm)) (canonical norm))))
    (cond
      ((null (plusp (length norm))) (values nil (first-names 8)))
      (exact (values exact nil))
      (t
       (let* ((containing (loop for (candidate . name) in (country-index)
                                when (search norm candidate) collect name))
              (alias (or (cdr (assoc norm *alias-table* :test #'string=))
                         (cdr (assoc (without-the norm) *alias-table* :test #'string=))
                         (cdr (assoc (format nil "the ~a" norm) *alias-table* :test #'string=))))
              (close (close-matches norm 4))
              (pool (append containing
                             (remove-if (lambda (name)
                                          (member name containing :test #'string=))
                                        close))))
         (cond
           ((and alias (country-entry alias)) (values alias nil))
           ((= 1 (length containing)) (values (first containing) nil))
           ((and (= 1 (length close)) (null containing)) (values (first close) nil))
           (t (values nil (if pool pool (first-names 6))))))))))

(defun resolve-task (raw)
  "RAW as a task family, or NIL."
  (let ((norm (normalize-name (clip (or raw "")))))
    (when (plusp (length norm))
      (or (loop for (task . weights) in *task-weights*
                when (or (search (substitute #\space #\_ task) norm)
                         (search norm task))
                  return task)
          (loop for (word . task) in *task-aliases*
                when (search word norm) return task)))))

;;; --- reading a country's numbers --------------------------------------------------

(defun meyer-value (entry scale)
  "ENTRY's position on the Meyer SCALE, or NIL."
  (let ((at (position scale *meyer-scales* :key (lambda (row) (getf row :key))
                                       :test #'string=)))
    (and at (nth at (getf entry :meyer)))))

(defun hofstede-value (entry dimension)
  "ENTRY's score on the Hofstede DIMENSION, or NIL."
  (let ((at (position dimension *hofstede-dims*
                       :key (lambda (row) (getf row :key)) :test #'string=)))
    (and at (nth at (getf entry :hofstede)))))

(defun scale-row (scale)
  (find scale *meyer-scales* :key (lambda (row) (getf row :key)) :test #'string=))

(defun scale-pole (scale position)
  "What POSITION on SCALE leans toward, in the original's words."
  (let ((row (scale-row scale)))
    (if (> position 50) (getf row :high) (getf row :low))))

(defun gap-risk (distance)
  (cond ((>= distance 30) "HIGH")
        ((>= distance 18) "MODERATE")
        (t "LOW")))

(defun region-of (entry)
  "The regional playbook ENTRY belongs to, or NIL."
  (cdr (assoc (getf entry :region) *regions* :test #'string=)))

(defun region-label (entry)
  (or (getf (region-of entry) :label) (getf entry :region)))

(defun bullet-lines (items count)
  "ITEMS as indented lines, the first COUNT of them."
  (loop for item in (subseq items 0 (min count (length items)))
        collect (format nil "  - ~a" item)))

;;; --- a brief ---------------------------------------------------------------------

(defun brief (country &key task)
  "COUNTRY's business-culture brief: how trust is built, how decisions are
really made, communication, contracts, etiquette, taboos, the numbers behind
them and the field notes that say where the country differs from its region.
=> text."
  (let ((max-lines (setting :max-highlights)))
    (multiple-value-bind (name suggestions) (resolve-country country)
      (if (null name)
          (format nil "Not found: ~a~%Try: ~{~a~^, ~}"
                  (clip (or country ""))
                  (subseq suggestions 0 (min 6 (length suggestions))))
          (let* ((entry (country-entry name))
                 (task (resolve-task task))
                 (lines (list (format nil "~a -- ~a | ~a | Lewis: ~a"
                                       name (region-label entry)
                                       (getf entry :globe) (getf entry :lewis)))))
            (when (getf entry :extrapolated)
              (setf lines
                    (append lines
                            (list (format nil "Note: ~a is positioned from its GLOBE-cluster neighbours, not from the published data sets; treat the numbers as directional." name)))))
            (setf lines (append lines (list (format nil "Scales: ~{~a~^; ~}" (scale-lines entry)))))
            (when (setting :include-hofstede)
              (setf lines (append lines (list (format nil "Hofstede: ~{~a~^, ~}" (hofstede-lines entry))))))
            (setf lines (append lines
                                (list (format nil "Trust: ~a" (getf (region-of entry) :trust))
                                      (format nil "Decisions: ~a" (getf (region-of entry) :decision))
                                      (format nil "Communication: ~a" (getf (region-of entry) :communication))
                                      (format nil "Contracts: ~a" (getf (region-of entry) :contract)))))
            (setf lines (append lines
                                (list "Etiquette:" ) (bullet-lines (getf (region-of entry) :etiquette) max-lines)
                                (list "Taboos:") (bullet-lines (getf (region-of entry) :taboos) max-lines)
                                (list "Country highlights:")
                                (bullet-lines (getf entry :highlights) max-lines)))
            (when task
              (setf lines (append lines
                                  (list (format nil "Task hint (~a): ~a" task
                                                (cdr (assoc task *task-hints* :test #'string=)))))))
            (setf lines (append lines
                                (list "Next: culture_gap_analysis with your own country as home_country, for the gaps that cause misreads in this pairing.")))
            (format nil "~{~a~^~%~}" lines))))))

(defun scale-lines (entry)
  "Each Meyer scale ENTRY sets, as LABEL POSITION (the pole it leans to)."
  (loop for row in *meyer-scales*
        for position = (meyer-value entry (getf row :key))
        when position
          collect (format nil "~a ~d (~a)" (getf row :label) position
                          (scale-pole (getf row :key) position))))

(defun hofstede-lines (entry)
  (loop for row in *hofstede-dims*
        for value = (hofstede-value entry (getf row :key))
        when value collect (format nil "~a ~d" (getf row :label) value)))

;;; --- the gaps --------------------------------------------------------------------

(defun gap-analysis (host &key home task)
  "The cultural distance between HOME, the country you work from, and HOST: the
Meyer scale gaps ranked biggest first with the misread and the adaptation move
each one carries, the Hofstede dimensions that differ most, and one line of
synthesis. => text."
  (let* ((max-gaps (setting :max-gaps))
         (home-raw (or (speaks home) (speaks (setting :home-country)))))
    (unless home-raw
      (fail "gap analysis needs a home country: pass home_country, or set culture.home_country, the country you work from"))
    (multiple-value-bind (home-name home-hints) (resolve-country home-raw)
      (unless home-name
        (fail "home country ~a is not covered; try ~{~a~^, ~}"
              (clip home-raw) (subseq home-hints 0 (min 6 (length home-hints)))))
      (multiple-value-bind (host-name host-hints) (resolve-country host)
        (unless host-name
          (fail "host country ~a is not covered; try ~{~a~^, ~}"
                (clip (or host "")) (subseq host-hints 0 (min 6 (length host-hints)))))
        (when (string= home-name host-name)
          (fail "home and host are the same country (~a); a gap analysis compares two cultures"
                home-name))
        (let* ((task (resolve-task task))
               (home-entry (country-entry home-name))
               (host-entry (country-entry host-name))
               (gaps (ranked-gaps home-entry host-entry task))
               (shown (subseq gaps 0 (min max-gaps (length gaps)))))
          (format nil "~{~a~^~%~}"
                  (append
                   (list (format nil "Culture gap: ~a -> ~a (~a)" home-name host-name
                                 (or task "general")))
                   (when (getf host-entry :extrapolated)
                     (list (format nil "Note: ~a is positioned from its GLOBE-cluster neighbours; treat the numbers as directional." host-name)))
                   (list (format nil "Ranked gaps (~d of ~d, ~a):" (length shown) (length gaps)
                                 (if task (format nil "weighted for ~a" task) "unweighted")))
                   (loop for gap in shown
                         nconc (list (format nil "~d. ~a ~d -> ~d (~a, gap ~d, weight ~a, ranked ~a)"
                                             (getf gap :rank)
                                             (getf (getf gap :row) :label)
                                             (getf gap :home) (getf gap :host)
                                             (if (plusp (getf gap :delta))
                                                 "host higher" "host lower")
                                             (getf gap :distance) (getf gap :weight)
                                             (getf gap :ranked))
                                     (format nil "   Risk: ~a" (getf gap :risk-text))
                                     (format nil "   Adapt: ~a" (getf gap :adaptation))))
                   (list (format nil "Hofstede flags: ~a"
                                 (let ((flags (hofstede-flags home-entry host-entry)))
                                   (if flags
                                       (format nil "~{~a~^; ~}" flags)
                                       "none of the six dimensions differs by 25 points or more."))))
                   (list (format nil "Synthesis: host trust ~a, decisions ~a, feedback ~a."
                                 (trust-mode host-entry) (decision-pace host-entry)
                                 (feedback-style host-entry))))))))))

(defun speaks (text)
  "TEXT when it says something, else NIL."
  (let ((text (nlk:trimmed (or text ""))))
    (and (plusp (length text)) text)))

(defun ranked-gaps (home-entry host-entry task)
  "Every Meyer scale both countries set, as a plist of the gap between them,
sorted by the task-weighted size of it, biggest first."
  (let ((weights (cdr (assoc task *task-weights* :test #'string=)))
        (gaps '()))
    (loop for row in *meyer-scales*
          for scale = (getf row :key)
          for mine = (meyer-value home-entry scale)
          for theirs = (meyer-value host-entry scale)
          when (and mine theirs)
            do (let* ((delta (- theirs mine))
                      (distance (abs delta))
                      (direction (if (plusp delta) "higher" "lower"))
                      (weight (or (cdr (assoc scale weights :test #'string=)) 1.0)))
                 (push (list :rank 0 :row row :scale scale :home mine :host theirs :delta delta
                             :distance distance :weight weight
                             :ranked (round1 (* distance weight))
                             :risk (gap-risk distance)
                             :risk-text (risk-text scale direction)
                             :adaptation (adaptation scale direction))
                       gaps)))
    (let ((sorted (sort gaps #'> :key (lambda (gap) (getf gap :ranked)))))
      (loop for gap in sorted for at from 1 do (setf (getf gap :rank) at))
      sorted)))

(defun round1 (value)
  "VALUE to one decimal place, so a weighted gap prints as 76.5 and not as
76.500001."
  (/ (round (* (float value) 10)) 10.0))

(defun risk-text (scale direction)
  "What a gap on SCALE in DIRECTION risks, in the original's words."
  (let ((row (cdr (assoc scale *gap-risks* :test #'string=))))
    (or (cdr (assoc direction row :test #'string=)) "")))

(defun adaptation (scale direction)
  "The move that answers a gap on SCALE in DIRECTION."
  (let ((row (cdr (assoc scale *adaptations* :test #'string=))))
    (or (cdr (assoc direction row :test #'string=)) "")))

(defun hofstede-flags (home-entry host-entry)
  "The Hofstede dimensions where the two countries differ by 25 points or more."
  (loop for row in *hofstede-dims*
        for dimension = (getf row :key)
        for mine = (hofstede-value home-entry dimension)
        for theirs = (hofstede-value host-entry dimension)
        when (and mine theirs (>= (abs (- theirs mine)) 25))
          collect (format nil "~a home ~d host ~d (~@d)"
                          (getf row :label) mine theirs (- theirs mine))))

(defun trust-mode (entry)
  (let ((value (meyer-value entry "trust")))
    (if (and value (> value 55)) "relationship-based" "task-based")))

(defun decision-pace (entry)
  (let ((value (meyer-value entry "decide")))
    (cond ((null value) "unset")
          ((< value 40) "slow consensual pre-alignment")
          ((> value 70) "fast top-down")
          (t "mixed"))))

(defun feedback-style (entry)
  (let ((value (meyer-value entry "eval")))
    (cond ((null value) "unset")
          ((> value 60) "indirect, face-preserving")
          ((< value 40) "direct")
          (t "moderate"))))

;;; --- the coverage list ------------------------------------------------------------

(defun countries (&optional query)
  "The countries covered, optionally filtered by a word from a name or a
region. => text."
  (let ((norm (normalize-name (clip (or query ""))))
        (by-region '())
        (matched 0))
    (loop for (name . entry) in *countries*
          for region = (getf entry :region)
          when (or (zerop (length norm))
                   (search norm (normalize-name name))
                   (search norm (normalize-name region)))
            do (incf matched)
               (let ((row (assoc region by-region :test #'string=)))
                 (if row
                     (setf (cdr row) (append (cdr row) (list name)))
                     (push (cons region (list name)) by-region))))
    (format nil "~d economies covered, ~d matched.~%~{~a~}Hint: culture_brief and culture_gap_analysis take any of these names, or a common alias (USA, UK, UAE, Korea, Holland, Czech Republic, Burma, Lao PDR, PRC, HK, KSA, RSA, Turkiye)."
            (length *countries*) matched
            (loop for (region . names) in (sort by-region #'string< :key #'car)
                  for label = (or (getf (cdr (assoc region *regions* :test #'string=)) :label)
                                  region)
                  collect (format nil "~a: ~{~a~^, ~}~%" label (sort names #'string<))))))

;;; --- the operator's own door ------------------------------------------------------

(defun split-words (text)
  "TEXT split on spaces, tabs and newlines."
  (let ((words '())
        (start 0)
        (length (length text)))
    (loop for at from 0 to length
          do (when (or (= at length)
                        (member (char text at) '(#\space #\tab #\newline)))
               (when (> at start) (push (subseq text start at) words))
               (setf start (1+ at))))
    (nreverse words)))

(defun run-slash (args session-id)
  "/culture COUNTRY [TASK]: a brief, the way the original's slash command
answers one."
  (declare (ignore session-id))
  (let ((parts (and args (split-words args))))
    (if (null parts)
        "culture: type a country and its business-culture brief comes back, e.g. /culture Japan negotiation.
Tasks: negotiation, first meeting, management, communication, payment, trip.
The cell also gives the model culture_brief, culture_gap_analysis and culture_countries."
        (brief (first parts)
               :task (and (rest parts) (format nil "~{~a~^ ~}" (rest parts)))))))
