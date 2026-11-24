;;;; cell.lisp --- a correction is a pair: the answer rejected, the answer kept.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of mystiquemide's ichor (MIT).
;;;;
;;;; Ichor watches completed turns. When the operator answers a turn with a
;;;; rejection -- "no, use pnpm", "that's not what I asked" -- the previous
;;;; turn's ask and answer become a preference pair with the answer that
;;;; replaced it: prompt, rejected, chosen, and what the operator said. Every
;;;; text is scrubbed on this machine before it is stored (secrets, emails,
;;;; IPs, phone numbers, home paths); a pair is pending until the operator
;;;; keeps, shares or discards it, and only kept or shared pairs are exported.
;;;; Nothing leaves the machine unless the operator runs /ichor share.
;;;;
;;;; The pairs are one JSONL file under the home (or the folder the `dir'
;;;; setting names), the exports TRL's conversational preference format and an
;;;; Atropos pairs.jsonl beside it. The one network call is the Hugging Face
;;;; Hub, reached only by /ichor share, with a token read from the environment
;;;; variable the section names.

(in-package #:nodecode-ichor)

;;; --- the manual ------------------------------------------------------------------------
;;; (help :ichor) answers it while the cell runs; a request carries the one
;;; line the :HELP clause gives, never this text.

(defparameter +primer+
  "The nodecode-ichor cell turns corrections into preference pairs: when a turn
answers the one before it with a rejection, the earlier ask and answer, the
answer that replaced it and what you said are stored as one pending pair,
scrubbed on this machine first (secrets, emails, IPs, phone numbers, home
paths). Pairs live in one JSONL file under the Nodecode home, or in the folder
the `ichor` section's `dir' names; nothing is sent anywhere unless you run
/ichor share. Verbs, in eval, every one a string, every refusal ERROR:
ICHOR-ERROR:
  (ichor:status)   how many pairs are kept, shared and pending, and the top mistake types
  (ichor:review)   the oldest pending pair, as the card /ichor review shows
  (ichor:keep-pair 3) (ichor:share-pair 3) (ichor:discard-pair 3)   settle pair #3; a discarded pair keeps no text
  (ichor:export-pairs)   write dpo.jsonl (TRL conversational: prompt, chosen, rejected) and atropos_env/pairs.jsonl
  (ichor:upload)   upload the pairs queued for sharing to your Hugging Face dataset, private by default
  (ichor:forget-all)   delete every pair
/ichor answers the same verbs: status, review, keep|share|discard <id>, export,
share, set repo <name>, set public|private, off, on, off --global, forget-all
[yes], help."
  "What (help :ichor) answers while the cell runs.")

;;; --- the words ---------------------------------------------------------------------
;;; Data, so a layer can reword them.

(defparameter *statuses* '("pending" "kept" "shared" "discarded"))

(defparameter *openers*
  (cl-ppcre:create-scanner
   "^\\s*(?:no+\\b(?!\\s+(?:need|worries|problem|rush|pressure)\\b)|nope\\b|nah\\b|wrong\\b|incorrect\\b|not quite\\b|not what i\\b|that'?s (?:not|wrong|incorrect)\\b|that is (?:not|wrong)\\b|this is (?:not|wrong)\\b|you (?:forgot|missed|ignored|didn'?t)\\b|actually\\b|stop (?:doing|using|adding|making|saying|changing)\\b|don'?t\\b(?!\\s+worry)|do not\\b|why did you\\b|i said\\b|i asked\\b|i told you\\b|wait\\b|hmm,? no\\b|ugh\\b)"
   :case-insensitive-mode t)
  "An opener that rejects the previous answer, anchored to the message's start.")

(defparameter *redirects*
  (cl-ppcre:create-scanner
   "\\b(?:use \\S+(?: \\S+)? (?:not|instead of) \\S+|instead of \\S+|rather than \\S+|not \\S+,? (?:but|use)\\b|(?<!what )(?<!which )(?<!how )should (?:be|have been|use)\\b|supposed to\\b|redo\\b|that broke\\b|still (?:wrong|broken|failing|too \\w+)\\b|^(?:this|that|it)(?:'s| is| was) too \\w+\\b|doesn'?t work\\b|didn'?t work\\b|not working\\b)"
   :case-insensitive-mode t)
  "A redirect that can appear anywhere in a short message.")

(defparameter *answer-words*
  (cl-ppcre:create-scanner
   "^\\s*(?:no|nope|nah|not now|no thanks|no thank you|don'?t)\\W*(?:thanks?|thank you|need|bother)?\\W*$"
   :case-insensitive-mode t)
  "A reply to a question the agent asked, not a correction.")

(defparameter *question-start*
  (cl-ppcre:create-scanner "^(?:how|what|where|when|which|can you|could you|would you)\\b"
                            :case-insensitive-mode t)
  "A message that asks for help rather than rejecting an answer.")

(defparameter *thanks*
  (cl-ppcre:create-scanner
   "^(?:thanks|thank you|thx|ty|great|perfect|nice|cool|ok|okay|got it|yes|yeah|yep|yup|sure|let'?s|and|also|dont worry|don'?t worry)\\b"
   :case-insensitive-mode t)
  "An answer that accepts, so the turn before it was not rejected.")

(defparameter *fence*
  (cl-ppcre:create-scanner "^\\s*```[\\w+-]*\\s*$")
  "A markdown fence line, dropped from a card's side.")

;;; --- where the pairs live ------------------------------------------------------------

(defun data-dir ()
  "The folder pairs and exports live in: `dir' when the section names one, else
the ichor folder under this profile's Nodecode home."
  (let ((named (nlk:trimmed (or (setting :dir) ""))))
    (uiop:ensure-directory-pathname
     (if (plusp (length named))
         (nlk:expand-home named)
         (nlk:home "ichor/")))))

(defun pairs-path ()
  (merge-pathnames "pairs.jsonl" (data-dir)))

(defun state-path ()
  (merge-pathnames "state.json" (data-dir)))

;;; --- the store: one JSON line a pair, read whole, written whole ------------------------

(defun field (pair key)
  (nlk:json-value pair :string key))

(defun integer-field (pair key)
  (or (nlk:json-value pair :integer key) 0))

(defun pairs ()
  "Every pair the file holds, oldest first."
  (nlk:read-json-lines (pairs-path)))

(defun write-pairs (rows)
  "ROWS as the file, atomically: a temp file in the same folder, renamed over."
  (let ((path (pairs-path))
        (tmp (merge-pathnames "pairs.jsonl.tmp" (data-dir))))
    (ensure-directories-exist path)
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :external-format :utf-8)
      (dolist (row rows)
        (write-line (nlk:encode-json-object row) out)))
    (uiop:rename-file-overwriting-target tmp path)
    rows))

(defun add-pair (pair)
  "P2 with its id, creation time and pending status, appended to the file."
  (let* ((rows (pairs))
         (id (1+ (reduce #'max rows :key (lambda (row) (integer-field row "id")) :initial-value 0))))
    (setf (gethash "id" pair) id
          (gethash "created_at" pair) (nlk:iso-time (nlk:unix-now))
          (gethash "status" pair) "pending")
    (nlk:append-json-line (pairs-path) pair)
    pair))

(defun find-pair (id)
  (find id (pairs) :key (lambda (row) (integer-field row "id")) :test #'=))

(defun set-status (id status)
  "Set pair ID's status; a discarded pair keeps no text. => the pair, or NIL."
  (when (member status *statuses* :test #'string=)
    (let ((rows (pairs)))
      (loop for row in rows
            when (= id (integer-field row "id"))
              do (setf (gethash "status" row) status)
                 (when (string= status "discarded")
                   (dolist (key '("prompt" "rejected" "accepted" "feedback"))
                     (setf (gethash key row) "")))
                 (write-pairs rows)
                 (return row)))))

(defun counts ()
  "An alist of (STATUS . N)."
  (let ((tally (mapcar (lambda (status) (cons status 0)) *statuses*)))
    (dolist (row (pairs) tally)
      (let ((cell (assoc (or (field row "status") "pending") tally :test #'string=)))
        (when cell (incf (cdr cell)))))))

(defun top-categories (&optional (limit 3))
  "((NAME . N) ...): the most common mistake labels over pending, kept and shared pairs."
  (let ((tally '()))
    (dolist (row (pairs))
      (when (member (field row "status") '("pending" "kept" "shared") :test #'string=)
        (let* ((name (or (field row "category") "other"))
               (cell (assoc name tally :test #'string=)))
          (if cell (incf (cdr cell)) (push (cons name 1) tally)))))
    (subseq (stable-sort tally #'string< :key #'car) 0 (min limit (length tally)))))

;;; --- a stored setting: the whole switch, kept beside the pairs -----------------------

(defun saved-object ()
  (let ((path (state-path)))
    (and (probe-file path)
         (let ((object (ignore-errors (nlk:decode-json (nlk:read-text path)))))
           (and (hash-table-p object) object)))))

(defun saved (key &optional default)
  (or (nlk:json-value (saved-object) :string key) default))

(defun save-setting (key value)
  (let* ((object (or (saved-object) (nlk:json-object)))
         (path (state-path)))
    (setf (gethash key object) value)
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-line (nlk:encode-json-object object) out))
    value))

(defvar *paused* (make-hash-table :test #'equal)
  "The sessions /ichor off paused, in this image only.")

(defun off-p (session)
  "T when capturing is paused here or everywhere."
  (or (string= (or (saved "global_off") "0") "1")
      (and (stringp session) (gethash session *paused*))))

;;; --- the scrubber: secrets and personal data out of every stored text ---------------

(defparameter *scrub-rules*
  '(("private_key" "-----BEGIN [A-Z ]*PRIVATE KEY-----[\\s\\S]*?-----END [A-Z ]*PRIVATE KEY-----"
     "[REDACTED:private_key]")
    ("anthropic_key" "\\bsk-ant-[A-Za-z0-9_\\-]{20,}" "[REDACTED:anthropic_key]")
    ("openai_key" "\\bsk-(?:proj-|svcacct-)?[A-Za-z0-9_\\-]{20,}" "[REDACTED:openai_key]")
    ("hf_token" "\\bhf_[A-Za-z0-9]{30,}" "[REDACTED:hf_token]")
    ("github_token" "\\b(?:gh[pousr]_[A-Za-z0-9]{36,}|github_pat_[A-Za-z0-9_]{60,})" "[REDACTED:github_token]")
    ("aws_key" "\\b(?:AKIA|ASIA)[A-Z0-9]{16}\\b" "[REDACTED:aws_key]")
    ("slack_token" "\\bxox[abprs]-[A-Za-z0-9\\-]{10,}" "[REDACTED:slack_token]")
    ("stripe_key" "\\b(?:sk|rk|pk)_(?:live|test)_[A-Za-z0-9]{16,}" "[REDACTED:stripe_key]")
    ("google_key" "\\bAIza[A-Za-z0-9_\\-]{35}" "[REDACTED:google_key]")
    ("telegram_bot_token" "\\b\\d{8,10}:[A-Za-z0-9_\\-]{35}\\b" "[REDACTED:telegram_bot_token]")
    ("jwt" "\\beyJ[A-Za-z0-9_\\-]{8,}\\.[A-Za-z0-9_\\-]{8,}\\.[A-Za-z0-9_\\-]{8,}" "[REDACTED:jwt]")
    ("bearer" "(?i)\\bbearer\\s+[A-Za-z0-9._\\-~+/]{16,}=*" "Bearer [REDACTED:bearer]")
    ("url_credentials" "(?i)\\b([a-z][a-z0-9+.\\-]{0,15}://)[^\\s:/@]{1,256}:[^\\s/@]{1,256}@"
     "\\1[REDACTED:credentials]@")
    ("secret_assignment"
     "(?i)\\b([A-Z0-9_]{0,64}(?:API_?KEY|SECRET|TOKEN|PASSWORD|PASSWD|PWD)[A-Z0-9_]{0,64})(\\s{0,8}[=:]\\s{0,8})(['\"]?)([^\\s'\"]{6,512})\\3"
     "\\1\\2\\3[REDACTED:secret]\\3")
    ("email" "(?<![A-Za-z0-9._%+\\-])[A-Za-z0-9._%+\\-]{1,64}@[A-Za-z0-9\\-]{1,63}(?:\\.[A-Za-z0-9\\-]{1,63}){0,8}\\.[A-Za-z]{2,24}\\b"
     "[REDACTED:email]")
    ("ipv4" "\\b(?!(?:127|0)\\.)(?:25[0-5]|2[0-4]\\d|1?\\d?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1?\\d?\\d)){3}\\b"
     "[REDACTED:ip]")
    ("phone" "(?<![\\w.])\\+\\d{1,3}[\\s\\-]?\\(?\\d{2,4}\\)?(?:[\\s\\-]?\\d{3,4}){2}(?![\\w.])"
     "[REDACTED:phone]")
    ("home_path" "(?:/home/|/Users/)[A-Za-z0-9._\\-]+|[A-Za-z]:\\\\Users\\\\[A-Za-z0-9._\\-]+" "~"))
  "The scrub rules, specific token formats before generic patterns: (KIND PATTERN REPLACEMENT).")

(defparameter +secret-kinds+
  '("private_key" "anthropic_key" "openai_key" "hf_token" "github_token" "aws_key"
    "slack_token" "stripe_key" "google_key" "telegram_bot_token" "jwt" "bearer"
    "url_credentials" "secret_assignment")
  "The kinds of rule that hide a credential, counted apart on the card.")

(defparameter +max-chars+ 20000 "What one stored side of a pair is cut to.")

(defun scrub (text)
  "VALUES (TEXT with its secrets and personal data redacted, ((KIND . N) ...))."
  (let ((out (or text ""))
        (counts '()))
    (when (> (length out) +max-chars+)
      (setf out (concatenate 'string (subseq out 0 +max-chars+)
                             (format nil "~%... [cut ~d chars]" (- (length out) +max-chars+)))))
    (dolist (rule *scrub-rules* (values out (nreverse counts)))
      (destructuring-bind (kind pattern replacement) rule
        (let ((fixed (cl-ppcre:regex-replace-all (cl-ppcre:create-scanner pattern)
                                                  out replacement)))
          (unless (string= fixed out)
            (incf (cdr (or (assoc kind counts :test #'string=)
                           (car (push (cons kind 0) counts))))))
          (setf out fixed))))))

(defun merge-counts (a b)
  (dolist (entry b a)
    (incf (cdr (or (assoc (car entry) a :test #'string=)
                   (car (push (cons (car entry) 0) a)))))))

(defun scrub-object (counts)
  "COUNTS as the JSON object a pair stores: {\"kind\": n, ...}."
  (let ((object (nlk:json-object)))
    (dolist (entry counts object)
      (setf (gethash (car entry) object) (cdr entry)))))

(defun summarize-scrub (counts)
  "The card's line, e.g. \"1 path · 2 secrets\"."
  (let* ((secrets (reduce #'+ counts :key (lambda (entry) (if (member (car entry) +secret-kinds+
                                                                     :test #'string=)
                                                             (cdr entry) 0))
                          :initial-value 0))
         (paths (or (cdr (assoc "home_path" counts :test #'string=)) 0))
         (personal (reduce #'+ counts :key (lambda (entry)
                                             (if (member (car entry) '("email" "ipv4" "phone")
                                                         :test #'string=)
                                                 (cdr entry) 0))
                           :initial-value 0))
         (parts (list (format nil "~d path~:p" paths) (format nil "~d secret~:p" secrets))))
    (when (plusp personal)
      (setf parts (append parts (list (format nil "~d personal" personal)))))
    (format nil "~{~a~^ · ~}" parts)))

;;; --- the detector: does this turn say the one before it was wrong? -----------------

(defun norm (text)
  (string-downcase (nlk:trimmed (cl-ppcre:regex-replace-all "\\s+" (or text "") " "))))

(defun same-answer-p (a b)
  "Answers that differ only in case, spacing, punctuation or markdown are the same answer."
  (string= (cl-ppcre:regex-replace-all "[^A-Za-z0-9]" (string-downcase (or a "")) "")
           (cl-ppcre:regex-replace-all "[^A-Za-z0-9]" (string-downcase (or b "")) "")))

(defun ends-with-question-p (text)
  (let ((tail (nlk:trimmed (or text ""))))
    (and (plusp (length tail))
         (char= (char tail (1- (length tail))) #\?))))

(defun correction (prev-input prev-answer this-input)
  "VALUES (KIND CONFIDENCE REASON) when THIS-INPUT rejects PREV-ANSWER, else NIL."
  (declare (ignore prev-input))
  (let* ((text (nlk:trimmed (or this-input "")))
         (opener (and (plusp (length text)) (cl-ppcre:scan *openers* text)))
         (redirect (and (plusp (length text)) (cl-ppcre:scan *redirects* text)))
         (short (< (length text) 200)))
    (cond
      ((or (zerop (length text)) (cl-ppcre:scan *thanks* text)) nil)
      ((and (ends-with-question-p prev-answer) (cl-ppcre:scan *answer-words* text))
       nil)
      (t
       (when (and redirect (not opener) (cl-ppcre:scan *question-start* text))
         (setf redirect nil))
       (cond
         ((and opener (ends-with-question-p prev-answer) (not redirect)
               (<= (length (nlk:split-words text)) 3))
          nil)
         ((and opener (or redirect short))
          (values "correction" "medium" "rejecting opener"))
         ((and redirect short)
          (values "correction" "medium" "redirect phrase"))
         ((or opener redirect)
          (values "correction" "low" "weak correction cue in a long message"))
         (t nil))))))

(defun detect (prev-input prev-answer this-input this-answer)
  "VALUES (KIND CONFIDENCE REASON) when THIS-INPUT rejects the turn before, else NIL."
  (when (and (plusp (length (or prev-answer ""))) (plusp (length (or this-answer ""))))
    (cond
      ((same-answer-p prev-answer this-answer) nil)
      ((and (plusp (length (norm prev-input))) (string= (norm prev-input) (norm this-input)))
       (values "retry" "high" "same message re-sent"))
      (t (correction prev-input prev-answer this-input)))))

;;; --- the mistake label, from the operator's own words ------------------------------

(defparameter +package-managers+
  '("npm" "pnpm" "yarn" "bun" "pip" "uv" "poetry" "pipx" "conda" "cargo" "brew" "apt")
  "What a swap names when the mistake was the package manager.")

(defparameter *label-rules*
  '(("too long" "\\b(?:too long|too verbose|shorter|shorten|less words|tl;?dr|be brief|concise)\\b")
    ("too short" "\\b(?:too short|more detail|elaborate|expand on|longer)\\b")
    ("wrong tone" "\\b(?:too (?:casual|formal|stiff)|tone|more (?:formal|casual|friendly)|regal|warmer|polite)\\b")
    ("wrong fact" "\\b(?:wrong|incorrect|not true|that's false|it's actually|actually it's)\\b")
    ("ignored instruction" "\\b(?:i said|i asked|i told you|as i said|you ignored|you forgot|you missed)\\b")
    ("did not work" "\\b(?:didn'?t work|doesn'?t work|not working|still (?:broken|failing|wrong)|error|broke)\\b")
    ("unsafe default" "\\b(?:localhost only|bind (?:it )?to localhost|privileged|root|sudo|unsafe|insecure)\\b")
    ("missing option" "\\b(?:also|include|exclude|skip|ignore|without|add|with|make it|and (?:also )?(?:show|sort|follow))\\b"))
  "The local mistake labels, in the order they are tried.")

(defun rule-label (kind feedback)
  "A mistake label from the operator's own words, or NIL."
  (cond
    ((string= kind "retry") "retried answer")
    ((string= kind "undo") "changed the ask")
    (t
     (let ((swap (cl-ppcre:scan-to-strings
                  "(?i)\\b(?:use|try|with)\\s+([\\w./+-]+)(?:\\s+(?:instead of|not|rather than|over)\\s+([\\w./+-]+))?"
                  (or feedback ""))))
       (if swap
           (let ((wanted (string-downcase (or (aref swap 0) ""))))
             (if (member wanted +package-managers+ :test #'string=)
                 "wrong package manager"
                 "wrong tool"))
           (loop for (name pattern) in *label-rules*
                 when (cl-ppcre:scan (cl-ppcre:create-scanner pattern :case-insensitive-mode t)
                                      (or feedback ""))
                   return name))))))

;;; --- the card ---------------------------------------------------------------------

(defparameter +card-lines+ 6 "How many lines of a side a card shows.")

(defparameter +card-line+ 120 "How wide one of them is.")
;;; --- the card ---------------------------------------------------------------------

(defparameter +card-lines+ 6 "How many lines of a side a card shows.")
(defparameter +card-line+ 120 "How wide one of them is.")

(defun card-side (text)
  "A side of a pair as indented lines, at most +CARD-LINES+ of them."
  (let* ((lines (remove-if (lambda (line) (or (zerop (length (nlk:trimmed line)))
                                               (cl-ppcre:scan *fence* line)))
                            (uiop:split-string (or text "") :separator '(#\Newline))))
         (shown (loop for line in (subseq lines 0 (min +card-lines+ (length lines)))
                      collect (format nil "  ~a"
                                      (if (<= (length line) +card-line+)
                                          (string-right-trim " " line)
                                          (concatenate 'string (subseq line 0 (- +card-line+ 3)) "..."))))))
    (if shown
        (format nil "~{~a~^~%~}" shown)
        "  (empty)")))

(defun card-quote (text &optional (limit 80))
  (let ((one (nlk:one-line (or text ""))))
    (if (<= (length one) limit) one (concatenate 'string (subseq one 0 (- limit 3)) "..."))))

(defun capture-card (pair)
  "The card /ichor review shows for PAIR."
  (let* ((kind (or (field pair "kind") "correction"))
         (feedback (or (field pair "feedback") ""))
         (said (cond ((and (string= kind "correction") (plusp (length feedback)))
                      (format nil "you said: \"~a\"" (card-quote feedback)))
                     ((string= kind "retry") "you hit /retry")
                     ((string= kind "undo") "you used /undo and asked again")
                     (t nil)))
         (counts (scrub-counts pair)))
    (format nil "ichor · captured pair #~d~@[~%~a~]~%~%rejected~%~a~%accepted~%~a~%~%scrubbed: ~a~%/ichor keep <id> · share <id> · discard <id>"
            (integer-field pair "id") said
            (card-side (or (field pair "rejected") ""))
            (card-side (or (field pair "accepted") ""))
            (summarize-scrub counts))))

(defun scrub-counts (pair)
  "PAIR's scrub counts, ((KIND . N) ...)."
  (let ((object (nlk:json-value pair :object "scrub_counts")))
    (and object (loop for kind being the hash-keys of object using (hash-value n)
                      collect (cons kind (or n 0))))))

(defun collapsed-card (id status)
  (format nil "ichor · pair #~d ~a" id
          (cond ((string= status "kept") "kept (private)")
                ((string= status "shared") "queued to share")
                ((string= status "discarded") "discarded")
                (t status))))

(defun status-card ()
  "The line /ichor status answers: how many pairs, and the top mistake types."
  (let* ((tally (counts))
         (kept (or (cdr (assoc "kept" tally :test #'string=)) 0))
         (shared (or (cdr (assoc "shared" tally :test #'string=)) 0))
         (pending (or (cdr (assoc "pending" tally :test #'string=)) 0)))
    (if (every (lambda (entry) (zerop (cdr entry))) tally)
        (format nil "ichor · no corrections yet~%listening for \"no...\", /retry and a better ask")
        (let ((top (top-categories)))
          (format nil "ichor · ~d kept · ~d shared · ~d pending~@[~%top mistakes~%~a~]~%/ichor review · export · share"
                  kept shared pending
                  (and top (format nil "~{  ~a  ~d~^~%~}"
                                   (loop for (name . n) in top append (list name n)))))))))

;;; --- recording a pair --------------------------------------------------------------

(defun record-pair (&key session turn kind confidence reason prompt rejected accepted feedback)
  "Scrub the four texts, label the mistake locally, and append one pending pair."
  (let ((counts '()))
    (flet ((clean (text)
             (if (setting :scrub)
                 (multiple-value-bind (out seen) (scrub text)
                   (setf counts (merge-counts counts seen))
                   out)
                 (or text ""))))
      (let* ((prompt (clean prompt))
             (rejected (clean rejected))
             (accepted (clean accepted))
             (feedback (clean feedback))
             (object (nlk:json-object
                      "session" (or session "") "turn" (or turn "")
                      "kind" kind "confidence" confidence "reason" reason
                      "prompt" prompt "rejected" rejected "accepted" accepted
                      "feedback" feedback
                      "category" (or (rule-label kind feedback) "")
                      "scrub_counts" (scrub-object counts))))
        (add-pair object)))))

(defun captured-p (session turn)
  "T when a pair already holds SESSION's TURN."
  (and (stringp turn)
       (some (lambda (row) (and (equal (field row "session") session)
                                 (equal (field row "turn") turn)))
             (pairs))))

(defun turn-rows (session kind turn)
  "TURN's facts of KIND in SESSION, newest first, as payload objects."
  (nlk:events :session-id session :turn-id turn :kind kind :order :newest :limit 1 :as :payloads))

(defun payload-text (object)
  (or (nlk:json-value object :string "message")
      (nlk:json-value object :string "message" "content")
      ""))

(defun exchange (session turn)
  "VALUES (PREV-INPUT PREV-ANSWER THIS-INPUT THIS-ANSWER) for SESSION's TURN, or NIL
when the turn is the first, or either side is empty."
  (let* ((rows (nlk:events :session-id session :kind nlk::+kind-input-committed+
                            :order :newest :limit 2 :as :rows
                            :columns '("turn_id" "payload")))
         (this (first rows))
         (prev (second rows)))
    (when (and this prev)
      (let* ((this-id (or turn (first this)))
             (prev-id (first prev))
             (this-input (payload-text (nlk:decode-json (second this))))
             (prev-input (payload-text (nlk:decode-json (second prev))))
             (prev-answer (payload-text (first (turn-rows session nlk::+kind-assistant-completed+ prev-id))))
             (this-answer (payload-text (first (turn-rows session nlk::+kind-assistant-completed+ this-id)))))
        (when (and (plusp (length prev-input)) (plusp (length prev-answer))
                   (plusp (length this-answer)))
          (values prev-input prev-answer this-input this-answer))))))

(defun observe-turn (op)
  "The :FRAME observer: a completed turn that rejects the one before it becomes a pair."
  (multiple-value-bind (kind payload metadata turn) (nlk:frame-fact op)
    (declare (ignore payload metadata))
    (let ((session (getf op :session-id)))
      (when (and kind (string= kind "turn.completed") (stringp session)
                 (setting :capture) (not (off-p session))
                 (not (captured-p session turn)))
        (multiple-value-bind (prev-input prev-answer this-input this-answer)
            (exchange session turn)
          (when prev-input
            (multiple-value-bind (detected confidence reason)
                (detect prev-input prev-answer this-input this-answer)
              (when detected
                (let ((pair (record-pair :session session :turn (or turn "")
                                          :kind detected :confidence confidence :reason reason
                                          :prompt prev-input :rejected prev-answer
                                          :accepted this-answer
                                          :feedback (if (string= detected "correction")
                                                         this-input ""))))
                  (nle:notice (format nil "ichor · captured pair #~d: ~a"
                                      (integer-field pair "id")
                                      (or (field pair "category") detected))
                              :key "nodecode-ichor"))))))))))

;;; --- the verbs --------------------------------------------------------------------

(define-verb status ()
  "How many pairs are kept, shared and pending, and the top mistake types."
  (status-card))

(define-verb review ()
  "The oldest pending pair, as the card /ichor review shows; a notice when none is."
  (let ((pair (find "pending" (pairs) :key (lambda (row) (field row "status")) :test #'string=)))
    (if pair
        (capture-card pair)
        "ichor · nothing pending")))

(define-verb keep-pair (id)
  "Keep pair ID privately, so it exports but is never uploaded."
  (settle id "kept"))

(define-verb share-pair (id)
  "Queue pair ID for /ichor share, which uploads it to your Hugging Face dataset."
  (settle id "shared"))

(define-verb discard-pair (id)
  "Discard pair ID; a discarded pair keeps no text."
  (settle id "discarded"))

(defun settle (id status)
  (let ((pair (set-status (or (and (integerp id) id) (parse-int id)) status)))
    (unless pair
      (fail "no pair #~a" id))
    (collapsed-card (integer-field pair "id") status)))

(defun parse-int (text)
  (let ((digits (and (stringp text) (cl-ppcre:scan-to-strings "\\d+" text))))
    (and digits (parse-integer digits))))

(define-verb forget-all ()
  "Delete every pair and say how many went."
  (let ((n (length (pairs))))
    (write-pairs '())
    (clrhash *paused*)
    (format nil "ichor · deleted ~d pair~:p" n)))

;;; --- the exports ------------------------------------------------------------------

(defun export-record (pair)
  "One TRL conversational preference example: prompt, chosen, rejected as message lists."
  (nlk:json-object
   "prompt" (vector (nlk:json-object "role" "user" "content" (field pair "prompt")))
   "chosen" (vector (nlk:json-object "role" "assistant" "content" (field pair "accepted")))
   "rejected" (vector (nlk:json-object "role" "assistant" "content" (field pair "rejected")))
   "metadata" (nlk:json-object "source" "ichor"
                                "id" (integer-field pair "id")
                                "kind" (or (field pair "kind") "")
                                "category" (or (field pair "category") "")
                                "feedback" (or (field pair "feedback") "")
                                "created_at" (or (field pair "created_at") ""))))

(defun exportable ()
  "The kept and shared pairs, oldest first."
  (remove-if-not (lambda (row) (member (field row "status") '("kept" "shared") :test #'string=))
                 (pairs)))

(defun write-export (rows dir)
  "Write ROWS as DIR/export/dpo.jsonl and DIR/export/atropos_env/pairs.jsonl. => (VALUES COUNT FOLDER)."
  (let* ((folder (merge-pathnames "export/" dir))
         (dpo (merge-pathnames "dpo.jsonl" folder))
         (env (merge-pathnames "atropos_env/pairs.jsonl" folder))
         (count 0))
    (ensure-directories-exist env)
    (with-open-file (out dpo :direction :output :if-exists :supersede :external-format :utf-8)
      (dolist (pair rows)
        (when (and (plusp (length (or (field pair "prompt") "")))
                   (plusp (length (or (field pair "accepted") "")))
                   (plusp (length (or (field pair "rejected") ""))))
          (write-line (nlk:encode-json-object (export-record pair)) out)
          (incf count))))
    (uiop:copy-file dpo env)
    (values count folder)))

(define-verb export-pairs ()
  "Write the kept and shared pairs as dpo.jsonl and atropos_env/pairs.jsonl under `dir'."
  (let ((rows (exportable)))
    (if (null rows)
        "ichor · nothing to export yet: keep or share a pair first"
        (multiple-value-bind (count folder) (write-export rows (data-dir))
          (if (zerop count)
              "ichor · nothing to export yet: kept pairs were empty"
              (format nil "ichor · exported ~d pair~:p~%  dpo      ~a~%  atropos  ~a/"
                      count (namestring (merge-pathnames "dpo.jsonl" folder))
                      (namestring (merge-pathnames "atropos_env/" folder))))))))

;;; --- sharing: the one network call, only when asked -------------------------------

(defvar *http* #'nlk:http "The HTTP exchange, so a test can stub it.")
(defvar *hf-token* nil "A token a test supplies instead of the environment's.")

(defparameter +hub+ "https://huggingface.co" "The one host ichor ever calls.")

(defun hf-token ()
  "The Hugging Face token: from the environment variable the section names, never stored."
  (or *hf-token*
      (let ((name (nlk:trimmed (or (setting :hf-token-env) "HF_TOKEN"))))
        (and (plusp (length name)) (uiop:getenv name)))))

(defparameter +b64+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun base64 (text)
  "TEXT's UTF-8 octets in base64."
  (let ((bytes (nlk:utf-8-octets text))
        (out (make-string-output-stream)))
    (loop for i from 0 below (length bytes) by 3
          for b0 = (aref bytes i)
          for b1 = (if (< (1+ i) (length bytes)) (aref bytes (1+ i)) 0)
          for b2 = (if (< (+ i 2) (length bytes)) (aref bytes (+ i 2)) 0)
          for n = (+ (ash b0 16) (ash b1 8) b2)
          do (write-char (char +b64+ (ldb (byte 6 18) n)) out)
             (write-char (char +b64+ (ldb (byte 6 12) n)) out)
             (write-char (if (< (1+ i) (length bytes)) (char +b64+ (ldb (byte 6 6) n)) #\=) out)
             (write-char (if (< (+ i 2) (length bytes)) (char +b64+ (ldb (byte 6 0) n)) #\=) out))
    (get-output-stream-string out)))

(defun hub (method path token &key content content-type)
  "One Hub exchange. => (VALUES BODY STATUS)."
  (funcall *http* method (concatenate 'string +hub+ path)
           :headers (append (list (cons "authorization" (format nil "Bearer ~a" token)))
                            (when content-type (list (cons "content-type" content-type))))
           :content content
           :timeout 30))

(defun hub-json (body)
  (if (and (stringp body) (plusp (length (nlk:trimmed body))))
      (ignore-errors (nlk:decode-json body))
      nil))

(defun upload ()
  "Upload the pairs queued for sharing as one new JSONL file. => a line with the dataset URL."
  (let ((token (hf-token)))
    (unless (and token (plusp (length token)))
      (fail "sharing needs a Hugging Face token~%set ~a in the environment, then /ichor share"
            (or (setting :hf-token-env) "HF_TOKEN")))
    (let ((rows (remove-if-not (lambda (pair)
                                 (and (string= (or (field pair "status") "") "shared")
                                      (null (field pair "share_url"))))
                               (pairs))))
      (when (null rows)
        (fail "nothing queued to share~%queue a pair with /ichor share <id>"))
      (multiple-value-bind (body status) (hub :get "/api/whoami-v2" token)
        (let* ((who (hub-json body))
               (name (nlk:json-value who :string "name")))
          (unless (and (= status 200) name)
            (fail "Hugging Face rejected the token (HTTP ~d)" status))
          (let* ((repo (or (nlk:trimmed (or (setting :hf-repo) "")) "ichor-pairs"))
                 (private (setting :hf-private))
                 (id (format nil "~a/~a" name repo)))
            (multiple-value-bind (create-body create-status)
                (hub :post "/api/repos/create" token
                     :content (nlk:encode-json-object
                               (nlk:json-object "type" "dataset" "name" repo
                                                "private" (and private t)))
                     :content-type "application/json")
              (declare (ignore create-body))
              (when (= create-status 403)
                (fail "the token cannot create repositories: give it write access to your own"))
              (unless (member create-status '(200 201 409))
                (fail "could not create the dataset ~a (HTTP ~d)" id create-status))
              (let* ((stamp (format-time-now))
                     (text (with-output-to-string (out)
                             (dolist (pair rows)
                               (write-line (nlk:encode-json-object (export-record pair)) out))))
                     (body (format nil "~{~a~%~}"
                                   (list (nlk:encode-json-object
                                          (nlk:json-object
                                           "key" "header"
                                           "value" (nlk:json-object
                                                    "summary" (format nil "ichor: add ~d pair~:p" (length rows))
                                                    "description" "")))
                                         (nlk:encode-json-object
                                          (nlk:json-object
                                           "key" "file"
                                           "value" (nlk:json-object
                                                    "path" (format nil "data/~a-~d.jsonl" stamp (length rows))
                                                    "encoding" "base64"
                                                    "content" (base64 text))))))))
                (multiple-value-bind (answer upload-status)
                    (hub :post (format nil "/api/datasets/~a/commit/main" id) token
                         :content body :content-type "application/x-ndjson")
                  (declare (ignore answer))
                  (when (= upload-status 403)
                    (fail "the token cannot write to ~a" id))
                  (unless (member upload-status '(200 201))
                    (fail "upload to ~a failed (HTTP ~d)" id upload-status))
                  (mark-uploaded rows (format nil "~a/datasets/~a" +hub+ id))
                  (format nil "ichor · shared ~d pair~:p (~a repo)~%~a/datasets/~a"
                          (length rows) (if private "private" "public") +hub+ id))))))))))

(defun format-time-now ()
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (nlk:unix-now))
    (format nil "~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d" year month day hour minute second)))

(defun mark-uploaded (rows url)
  "Stamp ROWS with URL and with when they went, then write the file once."
  (let ((all (pairs))
        (ids (mapcar (lambda (row) (integer-field row "id")) rows))
        (at (nlk:iso-time (nlk:unix-now))))
    (dolist (row all)
      (when (member (integer-field row "id") ids :test #'=)
        (setf (gethash "share_url" row) url
              (gethash "uploaded_at" row) at)))
    (write-pairs all)))

(define-verb upload-pairs ()
  "Upload every pair queued for sharing, then answer the dataset's URL."
  (upload))

;;; --- /ichor -----------------------------------------------------------------------

(defparameter +help-text+
  (format nil "ichor commands~%  /ichor                 status and top mistakes~%  /ichor review          next pending pair~%  /ichor keep <id>       keep private~%  /ichor share <id>      queue for sharing~%  /ichor share           upload queued pairs to Hugging Face~%  /ichor discard <id>    drop it~%  /ichor export          write TRL + Atropos files~%  /ichor set repo <name> the Hugging Face dataset name~%  /ichor set public      share to a public repo~%  /ichor set private     share to a private repo~%  /ichor off | on        pause or resume here~%  /ichor off --global    pause everywhere~%  /ichor forget-all yes  delete every pair")
  "What /ichor help answers.")

(defparameter +repo-name+
  (cl-ppcre:create-scanner "^[A-Za-z0-9][A-Za-z0-9._-]{0,95}$")
  "A Hugging Face repository name the hub will take.")

(defun set-verb (rest)
  (cond
    ((and (equal (first rest) "repo") (= 2 (length rest)))
     (let ((name (second rest)))
       (unless (and (cl-ppcre:scan +repo-name+ name) (not (search ".." name)))
         (fail "a repo name uses letters, digits, '.', '_' and '-'"))
       (save-setting "hf_repo" name)
       (format nil "ichor · sharing to ~a" name)))
    ((member (first rest) '("public" "private") :test #'string=)
     (let ((public (string= (first rest) "public")))
       (save-setting "hf_private" (if public "0" "1"))
       (format nil "ichor · sharing to a ~a repo~@[~%new repos are created public; an existing repo keeps its visibility~]"
               (first rest) public)))
    (t (fail "usage: /ichor set repo <name> | public | private"))))

(defun dispatch (sub rest session)
  (cond
    ((member sub '("status" "stats") :test #'string=) (status))
    ((member sub '("help" "-h" "--help") :test #'string=) +help-text+)
    ((string= sub "review") (review))
    ((and (string= sub "share") (null rest)) (upload-pairs))
    ((member sub '("keep" "k" "share" "s" "discard" "d") :test #'string=)
     (let ((id (parse-int (first rest)))
           (status (cond ((member sub '("keep" "k") :test #'string=) "kept")
                         ((member sub '("share" "s") :test #'string=) "shared")
                         (t "discarded"))))
       (unless id
         (fail "usage: /ichor ~a <id>" sub))
       (settle id status)))
    ((member sub '("off" "on") :test #'string=)
     (let ((off (string= sub "off")))
       (if (member "--global" rest :test #'string=)
           (progn (save-setting "global_off" (if off "1" "0"))
                  (if off "ichor · off everywhere" "ichor · on everywhere"))
           (progn
             (unless (and (stringp session) (plusp (length session)))
               (fail "no session here yet: send a message first"))
             (if off (setf (gethash session *paused*) t) (remhash session *paused*))
             (if off "ichor · off for this session" "ichor · on for this session")))))
    ((string= sub "set") (set-verb rest))
    ((string= sub "forget-all")
     (if (not (equal (first rest) "yes"))
         (format nil "ichor · this deletes all ~d pairs~%confirm with /ichor forget-all yes"
                 (length (pairs)))
         (forget-all)))
    ((string= sub "export") (export-pairs))
    (t +help-text+)))

(defun run-command (args session-id)
  "One /ichor invocation => its answer, and a refusal as the same string."
  (let* ((words (nlk:split-words (or args "")))
         (sub (string-downcase (or (first words) "status")))
         (rest (rest words)))
    (handler-case (dispatch sub rest session-id)
      (ichor-error (condition) (format nil "ichor · ~a" (ichor-error-detail condition))))))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell ichor
  (:section ("ichor")
    (:guide "nothing here is required; correct the agent once and /ichor review shows the pair; /ichor export writes TRL data; a Hugging Face token is read only by /ichor share")
    ("capture" :boolean :default t :doc "watch completed turns for a correction")
    ("dir" :path :doc "folder holding pairs.jsonl and the exports; empty keeps them under the Nodecode home")
    ("scrub" :boolean :default t
             :doc "redact secrets, emails, IPs, phones and home paths before a pair is stored")
    ("hf_repo" :string :default "ichor-pairs" :doc "the Hugging Face dataset /ichor share writes to")
    ("hf_private" :boolean :default t :doc "create that dataset private")
    ("hf_token_env" :env :default "HF_TOKEN"
                    :doc "the environment variable holding the Hugging Face write token"))
  (:hook :frame (nle:observer #'observe-turn "nodecode-ichor"))
  (:help :ichor "ichor: the moments you correct the agent, as preference pairs" +primer+)
  (:command "ichor" 'run-command
            :description "Corrections captured as preference pairs"
            :argument-hint "[review|keep|share|discard <id>|export|share|set|off|on|forget-all|help]"))
