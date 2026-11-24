;;;; rules.lisp --- the deterministic half: ranking, the gates, the lint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here is code, not judgement: the model proposes a score a
;;;; dimension and cites evidence ids, and the code checks the citations and
;;;; computes the total; the draft gate reads the log, the route and the caps
;;;; and says whether a draft may be written and on which route; the lint reads
;;;; a drafted text and refuses one a founder could not defend. A refusal here
;;;; cannot be argued away in prose, which is the point of the cell.

(in-package #:nodecode-raise)

;;; --- ranking ------------------------------------------------------------------

(defparameter +weights+
  '(("thesis" . 22) ("similar_companies" . 18) ("recency" . 14) ("stage_fit" . 10)
    ("lead" . 8) ("fund_cycle" . 8) ("reachability" . 8) ("value_add" . 7)
    ("content" . 5))
  "What each dimension is worth. An uncited dimension scores 0, whatever the model says.")

(defparameter +no-evidence-needed+ '("reachability")
  "Dimensions code computes, so a citation is not owed.")

(defparameter +deal-dims+ '("recency" "fund_cycle")
  "Meaningless for an angel with no deal record on file.")

(defparameter +dim-names+ (mapcar #'car +weights+))

(defun dim-of (dims name)
  "The model's member for dimension NAME: a hash table, or NIL."
  (and (hash-table-p dims) (nlk:json-value dims :object name)))

(defun cited-ids (dim)
  "The evidence ids DIM cites, as integers, in the order written."
  (let ((values (nlk:json-array dim "evidence_ids")))
    (when (zerop (length values))
      (setf values (nlk:json-array dim "evidence")))
    (loop for value across values
          for id = (cond ((integerp value) value)
                         ((stringp value)
                          (multiple-value-bind (number end) (parse-integer value :junk-allowed t)
                            (and number (= end (length value)) number)))
                         (t nil))
          when id collect id)))

(defun reachability-score (record now)
  "How reachable RECORD is, computed from the route their record offers."
  (let ((next (getf (contact-route record now) :next)))
    (cond ((null next) 0)
          ((member next '("intro" "linkedin_dm") :test #'equal) 10)
          ((member next '("connect" "email") :test #'equal) 8)
          (t 6))))

(defun rank-record (record dims now)
  "Score RECORD from DIMS, the model's 0-10 a dimension with cited ids.
=> (:total SCORE :dims (one plist a dimension, in weight order))."
  (let ((valid (mapcar (lambda (item) (getf item :id)) (evidence-of record)))
        (has-deal (find "deal" (evidence-of record)
                        :key (lambda (item) (getf item :kind)) :test #'equal))
        (individual (individual-p record))
        (kept 0.0)
        (total 0.0)
        (out '()))
    (dolist (entry +weights+)
      (let* ((name (car entry))
             (weight (cdr entry))
             (dim (dim-of dims name))
             (cited (cited-ids dim))
             (ids (remove-if-not (lambda (id) (member id valid)) cited))
             (notes '()))
        (cond
          ((and individual (not has-deal) (member name +deal-dims+ :test #'equal))
           (push (list :name name :score nil :weight weight :why "" :evidence '()
                       :note "dropped: individual investor with no deal evidence")
                 out))
          (t
           (let ((score (if (equal name "reachability")
                            (reachability-score record now)
                            (max 0.0 (min 10.0
                                           (float (or (nlk:json-value dim :number "score") 0)
                                                  1.0))))))
             (when (and (plusp score)
                        (not (member name +no-evidence-needed+ :test #'equal))
                        (null ids))
               (setf score 0.0)
               (push "zeroed: no cited evidence" notes))
             (when (< (length ids) (length cited))
               (push (format nil "ignored evidence ids not on this investor: ~{~a~^, ~}"
                             (set-difference cited ids))
                     notes))
             (incf kept weight)
             (incf total (* weight (/ score 10.0)))
             (push (list :name name :score score :weight weight
                         :why (or (nlk:json-value dim :string "why") "")
                         :evidence ids
                         :note (format nil "~{~a~^; ~}" (nreverse notes)))
                   out))))))
    (let ((dims-out (nreverse out)))
      (setf (getf record :score)
            (if (plusp kept) (/ (fround (* 1000.0 (/ total kept))) 10.0) 0.0))
      (setf (getf record :dims) dims-out)
      (setf (getf record :ranked_at) (stamp now))
      (set-investor record)
      (list :total (getf record :score) :dims dims-out))))
;;; --- the route ----------------------------------------------------------------

(defparameter +conversation-channels+ '("linkedin_dm" "email" "firm_inbox" "x")
  "Channels a conversation happens on; a connect request or an intro ask is not one.")

(defparameter +route-order+ '("intro" "linkedin_dm" "connect" "email" "firm_inbox")
  "Warmest first: a warm intro, a connected DM, a connect request, an email, a firm inbox.")

(defun touches (record &key channel since direction)
  "RECORD's own reaches: touches out, or, DIRECTION in, the replies that came back."
  (let ((kind (if (equal direction "in") "reply" "touch"))
        (wanted (if (equal direction "in") "in" "out")))
    (remove-if-not
     (lambda (event)
       (and (equal (getf event :kind) kind)
            (equal (or (getf event :direction) "out") wanted)
            (or (null channel) (equal (getf event :channel) channel))
            (or (null since)
                (let ((at (parse-stamp (getf event :at))))
                  (and at (>= at since))))))
     (events-of record))))

(defun last-touch-time (record channel)
  "When RECORD last reached out on CHANNEL, in unix seconds, or NIL."
  (let ((out (touches record :channel channel)))
    (and out (parse-stamp (getf (car (last out)) :at)))))

(defun pending-status (record channel hold-since)
  "Where a pending ask on CHANNEL stands: opens it, waits on it, or finds it stale."
  (let ((at (last-touch-time record channel)))
    (cond ((null at) (values "open" nil))
          ((> at hold-since) (values "waiting" at))
          (t (values "stale" at)))))

(defun contact-route (record now)
  "The routes RECORD offers, warmest first, and which one to use now. A pending
ask holds every later route for the cell's hold, in working days.
=> (:next CHANNEL :waiting STEP :steps STEPS)."
  (let ((hold-since (workdays-back now (setting :hold-workdays)))
        (cap-since (- now (* 86400 (setting :cap-window-days))))
        (has-paths (plusp (length (getf record :warm_paths))))
        (steps '()))
    (dolist (channel +route-order+)
      (let ((status "unavailable")
            (since nil)
            (detail ""))
        (cond
          ((equal channel "intro")
           (if (not has-paths)
               (setf detail "no warm path known")
               (progn
                 (multiple-value-setq (status since) (pending-status record channel hold-since))
                 (setf detail (cond ((equal status "waiting") "intro ask in flight")
                                    ((equal status "stale") "no answer to the intro ask; go direct")
                                    (t ""))))))
          ((equal channel "linkedin_dm")
           (if (getf record :connected)
               (setf status "open")
               (setf detail "not connected")))
          ((equal channel "connect")
           (cond ((getf record :connected) (setf detail "already connected"))
                 ((let ((linkedin (getf record :linkedin)))
                    (and linkedin (plusp (length linkedin))))
                  (multiple-value-setq (status since) (pending-status record channel hold-since))
                  (setf detail (cond ((equal status "waiting") "connect request pending")
                                     ((equal status "stale") "connect not accepted yet")
                                     (t ""))))
                 (t (setf detail "no LinkedIn profile"))))
          ((equal channel "email")
           (if (and (let ((email (getf record :email))) (and email (plusp (length email))))
                    (getf record :email_confirmed))
               (setf status "open")
               (setf detail "no confirmed email")))
          ((equal channel "firm_inbox")
           (if (or (let ((email (getf record :firm_email))) (and email (plusp (length email))))
                   (let ((form (getf record :firm_form))) (and form (plusp (length form)))))
               (setf status "open")
               (setf detail "no firm inbox or form"))))
        (when (and (equal status "open")
                   (>= (length (touches record :channel channel :since cap-since))
                       (setting :cap-per-channel)))
          (setf status "exhausted"
                detail (format nil "~d touches in ~d days"
                               (setting :cap-per-channel) (setting :cap-window-days))))
        (push (list :channel channel :status status :detail detail
                    :since (and since (stamp since))
                    :until (and (equal status "waiting")
                                (stamp (add-workdays since (setting :hold-workdays)))))
              steps)))
    (let ((next nil) (waiting nil))
      (dolist (step (reverse steps))
        (cond ((equal (getf step :status) "waiting") (setf waiting step) (return))
              ((equal (getf step :status) "open") (setf next (getf step :channel)) (return))))
      (list :next next :waiting waiting :steps (reverse steps)))))

(defun route-text (route)
  "ROUTE as a founder reads it: one line a channel."
  (format nil "~{~a~^~%~}"
          (loop for step in (getf route :steps)
                collect (format nil "  ~a: ~a~@[ (~a)~]"
                                (getf step :channel)
                                (getf step :status)
                                (let ((detail (getf step :detail)))
                                  (and detail (plusp (length detail)) detail))))))

;;; --- the draft gate -------------------------------------------------------------

(defun gate-record (record &optional (now (now)))
  "Whether Raise may draft to RECORD now, and on which route. Any reason in
:reasons blocks. => (:ok :mode :channel :reasons :warnings :route)."
  (let ((reasons '())
        (warnings '()))
    (flet ((refuse (control &rest args) (push (apply #'format nil control args) reasons))
           (caution (control &rest args) (push (apply #'format nil control args) warnings)))
      (let* ((firm (let ((name (getf record :firm)))
                     (if (and name (plusp (length name))) name "this firm")))
             (person (getf record :name))
             (who (if (and person (plusp (length person))) person firm))
             (individual (individual-p record))
             (stage (stage-of record)))
        (when (or (getf record :archived) (getf record :excluded))
          (refuse "~a is archived or excluded." who))
        (when (member stage '("committed" "passed") :test #'equal)
          (refuse "~a is marked ~a." who stage))
        (cond
          ((not (and person (plusp (length person))))
           (refuse "No named person at ~a; Raise only drafts to a person, never to a firm." firm))
          ((and (getf record :firm)
                (or (firmish-p person)
                    (string= (name-key person) (name-key (getf record :firm)))))
           (refuse "~s looks like a firm, not a person." person))
          ((several-p person)
           (refuse "~s looks like several people in one record; split it first." person)))
        (when (getf record :do_not_contact)
          (refuse "~a is marked do-not-contact." who))
        (let* ((ins (touches record :direction "in"))
               (outs (remove-if-not (lambda (event)
                                       (member (getf event :channel) +conversation-channels+
                                               :test #'equal))
                                     (touches record)))
               (last-in (car (last ins)))
               (last-out (car (last outs)))
               (last-in-at (and last-in (parse-stamp (getf last-in :at))))
               (last-out-at (and last-out (parse-stamp (getf last-out :at))))
               (mode (cond ((and last-in (or (not last-out) (>= last-in-at last-out-at))) "reply")
                           (last-out "follow_up")
                           (t "cold")))
               (route (contact-route record now))
               (cap-since (- now (* 86400 (setting :cap-window-days))))
               (channel nil))
          ;; a pass at a firm is a pass on the firm, unless the person wrote back
          (when (and (getf record :firm) (not individual))
            (let ((passed (find-if (lambda (other)
                                     (and (not (eql (getf other :id) (getf record :id)))
                                          (equal (name-key (getf other :firm))
                                                 (name-key (getf record :firm)))
                                          (equal (stage-of other) "passed")))
                                   (investors))))
              (when (and passed (not (equal stage "in_talks")) (not (equal mode "reply")))
                (refuse "~a passed (via ~a); a pass applies to the whole firm."
                       firm (or (let ((name (getf passed :name)))
                                  (and name (plusp (length name)) name))
                                "the firm")))))
          (cond
            ((equal mode "reply")
             (setf channel (getf last-in :channel)))
            ((equal mode "follow_up")
             (let ((last-channel (getf last-out :channel)))
               (when (> last-out-at (workdays-back now (setting :hold-workdays)))
                 (refuse "Last touch (~a, ~a) is still pending; follow up after ~a (~d working days)."
                        last-channel (day-label last-out-at)
                        (day-label (add-workdays last-out-at (setting :hold-workdays)))
                        (setting :hold-workdays)))
               (if (>= (length (touches record :channel last-channel :since cap-since))
                       (setting :cap-per-channel))
                   (if (getf route :next)
                       (progn
                         (setf channel (getf route :next))
                         (caution "~d touches on ~a already; moving to ~a."
                                     (setting :cap-per-channel) last-channel channel))
                       (refuse "~d touches on ~a in the last ~d days and no other open route; move on."
                              (setting :cap-per-channel) last-channel (setting :cap-window-days)))
                   (setf channel last-channel))))
            (t
             (let ((waiting (getf route :waiting)))
               (cond
                 (waiting
                  (refuse "~a is in flight since ~a; later routes wait until ~a (~d working days)."
                         (ppcre:regex-replace-all "_" (getf waiting :channel) " ")
                         (day-label (parse-stamp (getf waiting :since)))
                         (day-label (parse-stamp (getf waiting :until)))
                         (setting :hold-workdays)))
                 ((getf route :next) (setf channel (getf route :next)))
                 (t (refuse "no route is open: ~{~a~^; ~}"
                           (loop for step in (getf route :steps)
                                 for detail = (getf step :detail)
                                 when (and detail (plusp (length detail)))
                                   collect (format nil "~a: ~a" (getf step :channel) detail))))))))
          (list :ok (null reasons)
                :mode mode
                :channel channel
                :reasons (nreverse reasons)
                :warnings (nreverse warnings)
                :route route))))))

;;; --- the lint -------------------------------------------------------------------

(defparameter +public-channels+ '("x" "x_reply" "linkedin_post" "public")
  "A raise amount in one of these reaches people the founder did not choose.")

(defparameter +max-words+ '(("email" . 150) ("firm_inbox" . 150) ("linkedin_dm" . 70)
                             ("connect" . 50) ("intro" . 120) ("x" . 50))
  "How long a draft on each channel may run before it is too long to read.")

(defparameter +stock-words+
  '("delve" "tapestry" "testament" "pivotal" "seamless" "seamlessly" "leverage" "robust"
    "landscape" "realm" "foster" "unlock" "game-changer" "game changer" "cutting-edge"
    "elevate" "empower" "synergy" "holistic" "in today's" "ever-evolving" "groundbreaking"
    "transformative" "i hope this email finds you well" "i hope this message finds you well"
    "i wanted to reach out" "thrilled to" "excited to share" "at the intersection of" "paradigm")
  "Phrases that say a model wrote it.")

(defparameter +inflated+
  '("revolutionary" "world-class" "best-in-class" "unprecedented" "the leading" "first-ever"
    "industry-leading" "disrupt" "massive" "unparalleled")
  "Claims a founder cannot defend.")

(defparameter +stop-words+
  '("their" "there" "about" "which" "would" "could" "should" "these" "those" "other" "after"
    "before" "invest" "invests" "invested" "investing" "investor" "investors" "company"
    "companies" "round" "fund" "funds" "stage" "recent" "recently" "founder" "founders")
  "Words a personalisation check must not count as one on its own.")

(defparameter +signoffs+
  '("best" "cheers" "thanks" "thank you" "regards" "kind regards" "warm regards"
    "all the best" "br")
  "Where a draft's body ends, so a close is judged as a close.")

(defparameter +time-units+
  '("minute" "minutes" "min" "mins" "hour" "hours" "hr" "hrs" "day" "days" "week"
    "weeks" "month" "months" "year" "years" "am" "pm" "x")
  "A number before one of these is a duration, not a claim.")

(defun parse-number (raw)
  "RAW as a number: thousands separators, or a decimal comma."
  (cond ((ppcre:scan "^[0-9]{1,3}([.,][0-9]{3})+$" raw)
         (ignore-errors (parse-integer (ppcre:regex-replace-all "[.,]" raw ""))))
        ((find (code-char 44) raw)
         (ignore-errors (read-from-string (ppcre:regex-replace-all "," raw "."))))
        (t (ignore-errors (read-from-string raw)))))

(defun numbers-in (text)
  "Every number TEXT states, normalised, as (:value V :percent P :currency C :digits N)."
  (let ((text (or text ""))
        (limit (length (or text "")))
        (out '())
        (index 0))
    (loop while (< index limit)
          do (if (digit-char-p (char text index))
                 (let* ((start index)
                        (end (loop while (and (< index limit)
                                              (or (digit-char-p (char text index))
                                                  (and (member (char text index)
                                                               (list (code-char 44) (code-char 46)))
                                                       (< (1+ index) limit)
                                                       (digit-char-p (char text (1+ index))))))
                                   do (incf index)
                                   finally (return index)))
                        (raw (subseq text start end))
                        (value (parse-number raw))
                        (word-start (loop while (and (< end limit)
                                                     (member (char text end)
                                                             '(#\Space #\Tab #\Return #\Newline)))
                                          do (incf end)
                                          finally (return end)))
                        (word-end (loop while (and (< end limit) (alpha-char-p (char text end)))
                                        do (incf end)
                                        finally (return end)))
                        (word (string-downcase (subseq text word-start word-end)))
                        (percent (and (< end limit) (char= (char text end) (code-char 37))))
                        (multiplier (cond (percent 1)
                                          ((member word '("k" "thousand") :test #'string=) 1000)
                                          ((member word '("m" "mn" "million") :test #'string=) 1000000)
                                          ((member word '("b" "bn" "billion") :test #'string=)
                                           1000000000)
                                          (t 1))))
                   (when value
                     (push (list :value (* (coerce value 'double-float) multiplier)
                                 :percent percent
                                 :currency (and (> start 0)
                                                (member (char text (1- start))
                                                        (list (code-char 36) (code-char 8364)
                                                              (code-char 163))))
                                 :digits (length raw)
                                 :unit (member word +time-units+ :test #'string=)
                                 :text (nlk:trimmed (subseq text start end)))
                           out))
                   (unless (> index start) (incf index)))
                 (incf index)))
    (nreverse out)))

(defun number-claim-p (number)
  "Whether NUMBER is a claim at all: a year, a duration and a small count are not."
  (let ((value (getf number :value)))
    (cond ((getf number :percent) t)
          ((getf number :unit) nil)
          ((and (= (getf number :digits) 4)
                (integerp value)
                (<= 1990 value 2100)
                (not (getf number :currency)))
           nil)
          ((and (not (getf number :currency))
                (< value 10.5)
                (= value (fround value)))
           nil)
          (t t))))

(defun allowed-numbers (texts)
  "Every number the verified claims and the investor's cited facts state."
  (loop for text in texts nconc (numbers-in (princ-to-string (or text "")))))

(defun number-allowed-p (number allowed)
  (let ((value (getf number :value))
        (percent (and (getf number :percent) t)))
    (some (lambda (other)
            (and (eq percent (and (getf other :percent) t))
                 (<= (abs (- value (getf other :value)))
                     (max 0.5 (* 0.01 (abs (getf other :value)))))))
          allowed)))

(defun keywords-of (fact)
  "FACT's capitalised words and its long lowercase words, for a personalisation check."
  (values (remove-if (lambda (word) (member word '("The" "They" "This" "Their" "With")
                                                 :test #'string=))
                     (ppcre:all-matches-as-strings "[A-Z][a-zA-Z0-9&-]{2,}" (or fact "")))
          (remove-duplicates
           (remove-if (lambda (word) (member word +stop-words+ :test #'string=))
                      (mapcar #'string-downcase
                              (ppcre:all-matches-as-strings "[a-zA-Z]{6,}" (or fact ""))))
           :test #'string=)))

(defun personalised-p (text facts)
  "Whether TEXT names something from the investor's own cited facts."
  (let ((low (string-downcase (or text ""))))
    (some (lambda (fact)
            (multiple-value-bind (caps longs) (keywords-of fact)
              (or (some (lambda (word) (search (string-downcase word) low)) caps)
                  (>= (count-if (lambda (word) (search word low)) longs) 2))))
          facts)))

(defun finding (rule message) (list :rule rule :message message))

(defun paragraphs-of (text)
  "TEXT's paragraphs: its non-blank lines grouped by the blank lines between them."
  (let ((paragraphs '())
        (current '()))
    (labels ((flush ()
               (when current
                 (push (format nil "~{~a~^~%~}" (nreverse current)) paragraphs)
                 (setf current '()))))
      (dolist (line (ppcre:split "[\\n\\r]" (or text "")))
        (if (zerop (length (nlk:trimmed line)))
            (flush)
            (push line current)))
      (flush))
    (nreverse paragraphs)))

(defun name-line-p (line)
  "Whether LINE is a bare signature name, as a close writes one."
  (ppcre:scan "^-?[ ]*[A-Z][a-zA-Z'.-]*([ ]+[A-Z][a-zA-Z'.-]*){0,2}[ ]*$" (or line "")))

(defun signoff-p (line)
  "Whether LINE opens with a sign-off word."
  (let ((low (string-downcase (nlk:trimmed (or line "")))))
    (some (lambda (word)
            (and (<= (length word) (length low))
                 (string= word (subseq low 0 (length word)))
                 (or (= (length word) (length low))
                     (member (char low (length word)) (list #\, #\! #\. #\: #\Space)))))
          +signoffs+)))

(defun body-without-signoff (text)
  "TEXT without its closing sign-off and bare name."
  (let ((paragraphs (paragraphs-of text)))
    (loop while paragraphs
          do (let* ((last (car (last paragraphs)))
                    (lines (ppcre:split "[\\n\\r]" last))
                    (final (nlk:trimmed (car (last lines)))))
               (cond ((signoff-p final)
                      (setf paragraphs (butlast paragraphs)))
                     ((and (plusp (length (nlk:split-words final)))
                           (<= (length (nlk:split-words last)) 2)
                           (plusp (length (butlast paragraphs)))
                           (signoff-p (car (last (butlast paragraphs)))))
                      (setf paragraphs (butlast paragraphs)))
                     ((name-line-p final)
                      (let ((kept (butlast lines)))
                        (setf paragraphs
                              (if kept
                                  (append (butlast paragraphs)
                                          (list (format nil "~{~a~^~%~}" kept)))
                                  (butlast paragraphs)))))
                     (t (return paragraphs))))
          finally (return paragraphs))
    (format nil "~{~a~^~%~}" (paragraphs-of (format nil "~{~a~^~%~%~}" paragraphs)))))

(defun contains-dash-p (text)
  (or (find (code-char 8212) text) (find (code-char 8211) text) (search " -- " text)))

(defun opener-p (text)
  "Whether TEXT opens with a staged opener."
  (let ((start (nlk:trimmed (string-downcase (or text "")))))
    (some (lambda (opener)
            (and (<= (length opener) (length start))
                 (string= opener (subseq start 0 (length opener)))))
          '("quick question" "here's the thing" "let me be direct" "i'll keep this short"
            "short version" "the short answer" "imagine" "picture this" "let's be honest"
            "i'll cut to the chase"))))

(defun lint-draft-text (text &key (channel "email") verified facts round-amount max-words)
  "Code checks a drafted TEXT after the model writes it.
=> (:ok BOOLEAN :hard FINDINGS :warnings FINDINGS); a hard finding blocks the draft."
  (let ((text (or text ""))
        (hard '())
        (warnings '())
        (allowed (allowed-numbers (append verified facts))))
    (dolist (number (numbers-in text))
      (when (and (number-claim-p number)
                 (not (number-allowed-p number allowed)))
        (push (finding "unverified_number"
                       (format nil "'~a' is not in your verified claims or the investor's cited facts."
                               (or (getf number :text) "")))
              hard)))
    (cond ((null facts)
           (push (finding "no_personalisation"
                          "No investor-specific cited fact was supplied for this draft.")
                 hard))
          ((not (personalised-p text facts))
           (push (finding "no_personalisation"
                          "The draft does not mention the cited investor-specific fact.")
                 hard)))
    (when (and round-amount (member channel +public-channels+ :test #'equal))
      (dolist (number (numbers-in text))
        (when (and (not (getf number :percent))
                   (<= (abs (- (getf number :value) (coerce round-amount 'double-float)))
                       (* 0.01 (abs (coerce round-amount 'double-float)))))
          (push (finding "public_raise_amount"
                         "The raise amount must not appear in a public post.")
                hard))))
    (when (contains-dash-p text)
      (push (finding "em_dash" "Em dash; use a comma, a full stop or parentheses.") warnings))
    (when (ppcre:scan "(?i)(not (just|only|merely) [^.?!]{1,60} but |(isn't|is not|it's not|not about) [^.?!]{1,60}?[,;] (it's|it is|but) )"
                      text)
      (push (finding "not_x_but_y" "'Not X but Y'; say the point directly.") warnings))
    (when (ppcre:scan "[A-Za-z-]{3,}, [A-Za-z-]{3,},? and [A-Za-z-]{3,}[.!]" text)
      (push (finding "forced_triad" "Three-item list; keep the one that matters.") warnings))
    (when (ppcre:scan "[*][*][^*]{1,40}:?[*][*]:?" text)
      (push (finding "bold_label" "Bold label; write it as a sentence.") warnings))
    (when (opener-p text)
      (push (finding "staged_opener" "Staged opener; start with the point.") warnings))
    (let ((low (string-downcase text)))
      (dolist (word +stock-words+)
        (when (search word low)
          (push (finding "stock_phrase" (format nil "Stock AI phrase '~a'." word)) warnings)))
      (dolist (word +inflated+)
        (when (search word low)
          (push (finding "inflated_claim" (format nil "Inflated claim '~a'." word)) warnings))))
    (let* ((body (body-without-signoff text))
           (trimmed (nlk:trimmed body))
           (sentences (remove "" (ppcre:split "[.!?][ ]+" trimmed) :test #'string=))
           (last-sentence (car (last sentences)))
           (question-p (and (plusp (length trimmed)) (find #\? (car (last (ppcre:split "\\n" trimmed))))))
           (paragraphs (paragraphs-of body)))
      (when (and (not (member channel +public-channels+ :test #'equal))
                 (not (equal channel "connect"))
                 last-sentence
                 (plusp (length last-sentence))
                 (not question-p))
        (push (finding "close_not_question"
                       (format nil "End with a plain question, not '~a'." (clip last-sentence 60)))
              warnings))
      (when (and (> (length paragraphs) 1)
                 (<= (length (nlk:split-words (car (last paragraphs)))) 6)
                 (not (find #\? (car (last paragraphs)))))
        (push (finding "one_line_closer"
                       (format nil "One-line punchline closer: '~a'."
                               (clip (nlk:trimmed (car (last paragraphs))) 60)))
              warnings))
      (let* ((cap (or max-words (cdr (assoc channel +max-words+ :test #'equal)) 150))
             (words (length (nlk:split-words trimmed))))
        (when (> words cap)
          (push (finding "too_long"
                         (format nil "~d words; keep it under ~d for ~a." words cap channel))
                warnings))))
    (list :ok (null hard) :hard (nreverse hard) :warnings (nreverse warnings))))
