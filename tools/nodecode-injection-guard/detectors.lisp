;;;; detectors.lisp --- injection-guard's detection engine.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Deterministic and offline: every rule is a compiled cl-ppcre scanner run
;;;; over one text, and each family carries a weight, so a single weak signal
;;;; does not trip the guard while several medium signals do. No model is in
;;;; the decision path -- a model an attacker can talk to is not a firewall.
;;;;
;;;; The threshold matters more than any individual rule: a false positive
;;;; costs the user real content, a false negative costs them the machine. The
;;;; defaults flag only when one strong signal, or several medium ones, fire
;;;; together.

(in-package #:nodecode-injection-guard)

;;; --- unicode hiding places -------------------------------------------------

(defparameter +hidden-ranges+
  '((#x200B . #x200F)                   ; zero-width space/joiners, LRM/RLM
    (#x202A . #x202E)                   ; bidi embeddings and overrides
    (#x2060 . #x2064)                   ; word joiner, invisible operators
    (#x2066 . #x2069)                   ; bidi isolates
    (#xFEFF . #xFEFF)                   ; a BOM used mid-document
    (#xE0000 . #xE007F))                ; tag characters
  "The code points almost never legitimate in tool output: the cheapest way
to hide an instruction inside visible text.")

(defun hidden-p (code)
  "Whether CODE is one of the hiding places +HIDDEN-RANGES+ names."
  (loop for (low . high) in +hidden-ranges+
        thereis (<= low code high)))

(defun count-hidden (text)
  "How many hidden code points TEXT carries."
  (count-if (lambda (character) (hidden-p (char-code character))) text))

;;; --- the families ----------------------------------------------------------

(defparameter +families+
  ;; (NAME WEIGHT SCANNER-OPTIONS PATTERN...), a PATTERN a string or
  ;; (STRING . OPTIONS) where it needs its own. WEIGHT is intent, not
  ;; severity: an override phrase is worth more than a mention of "token".
  (list
   (list "instruction_override" 6 '(:case-insensitive-mode t)
         "\\b(ignore|disregard|forget|override)\\b[^.\\n]{0,40}\\b(previous|prior|above|earlier|all|any|your)\\b[^.\\n]{0,30}(instruction|prompt|rule|direction|context|message)s?\\b"
         "\\b(ignore|disregard)\\b[^.\\n]{0,30}\\b(everything|all of the above)\\b"
         "\\byour (new|real|actual|true) (instructions?|task|job|purpose|goal)\\b")
   ;; Unambiguous machine-level artifacts: raw chat-template tokens and fake
   ;; control markers do not appear in legitimate prose, so this family trips
   ;; alone. The line-anchored two are read line by line, and the "SYSTEM:"
   ;; one case-sensitively -- "System: Ubuntu" is not an injection.
   (list "role_spoofing" 6 '(:case-insensitive-mode t)
         "<\\|?\\s*(im_start|im_end|system|endoftext|eot_id)\\s*\\|?>"
         "\\[(OUT-OF-BAND USER MESSAGE|OUTBOUND USER MESSAGE|INTERNAL NOTIFICATION|GATEWAY MESSAGE)[^\\]]{0,60}\\]"
         (list "^\\s*#{2,}\\s*(instruction|system prompt|new task)s?\\s*#*\\s*$" :multi-line-mode t)
         (list "^\\s*SYSTEM\\s*[:：]\\s" :multi-line-mode t :case-insensitive-mode nil))
   ;; Persona shifting is real signal but also ordinary English ("you are now
   ;; ready to deploy"), so it only contributes toward the threshold.
   (list "persona_shift" 3 '(:case-insensitive-mode t)
         "\\byou are (now|no longer)\\b"
         "\\b(act|behave|pretend)\\b[^.\\n]{0,20}\\b(as if|as an?|like)\\b"
         "\\bfrom now on,? you (are|will|must)\\b")
   ;; Touching private keys, env files or a secret store has no benign reading
   ;; when it arrives inside fetched content, so it trips alone.
   (list "credential_access" 6 '(:case-insensitive-mode t)
         "\\b(cat|type|read|print|dump|grep|head|tail)\\b[^.\\n]{0,30}(~/\\.ssh|/\\.ssh|id_rsa|id_ed25519|\\.env\\b|\\.aws/credentials|keychain|credentials\\.json)"
         "\\b(api[ _-]?keys?|access[ _-]?tokens?|private keys?|passwords?|secrets?)\\b[^.\\n]{0,40}\\b(send|reveal|print|output|show|leak)\\b")
   ;; Generic "send the data somewhere" is ordinary business English, so it
   ;; only contributes toward the threshold.
   (list "exfiltration" 4 '(:case-insensitive-mode t)
         "\\b(send|email|post|upload|forward|transmit|leak)\\b[^.\\n]{0,60}\\b(contents?|data|file|env|credentials?|api[ _-]?keys?|secrets?|tokens?|passwords?|\\.ssh|id_rsa|\\.env)\\b"
         "\\b(wget|nc|ncat|scp|rsync|curl)\\b[^.\\n]{0,60}(https?://|[0-9]{1,3}\\.[0-9]{1,3}\\.)")
   (list "destructive" 6 '(:case-insensitive-mode t)
         "\\brm\\s+(-[a-z]*r[a-z]*f|-[a-z]*f[a-z]*r)\\b"
         "\\b(drop|truncate)\\s+(table|database|schema)\\b(?!\\s+if\\s+exists)"
         ;; --force-with-lease is the safe variant; only bare --force or -f count.
         "\\bgit\\s+push\\b[^.\\n]{0,30}(-f\\b|--force(?![\\w-]))"
         "\\bmkfs\\b"
         "\\bdd\\b[^.\\n]{0,12}?\\bif[=\\s]"
         ":\\(\\)\\s*\\{"
         ;; "delete all draft invoices" is ordinary business English; only a
         ;; destructive object makes it a command worth intercepting.
         "\\bdelete\\s+(all|every)\\b[^.\\n]{0,25}\\b(files?|data|records?|tables?|rows?|backups?|logs?|volumes?|databases?|snapshots?)\\b")
   ;; Concealment instructions have no legitimate reading inside fetched page
   ;; content, so this family trips alone.
   (list "concealment" 6 '(:case-insensitive-mode t)
         (list "<!--(?:(?!-->).){0,800}?(ignore|instruction|system|you must|assistant|prompt)(?:(?!-->).){0,800}?-->" :single-line-mode t)
         "style\\s*=\\s*[\"'][^\"']*(color\\s*:\\s*(#fff|#ffffff|white)|font-size\\s*:\\s*0|display\\s*:\\s*none|visibility\\s*:\\s*hidden|opacity\\s*:\\s*0)[^\"']*[\"']"
         "\\bdo not (tell|inform|mention|reveal)[^.\\n]{0,30}\\b(user|human|operator)\\b"
         "\\bwithout (telling|informing|notifying)[^.\\n]{0,30}\\b(user|human|operator)\\b"
         "\\b(hide|conceal|suppress)\\b[^.\\n]{0,30}\\bfrom the user\\b")
   (list "meta_awareness" 3 '(:case-insensitive-mode t)
         "\\b(if|when) you are an? (ai|llm|language model|assistant|agent)\\b"
         "\\b(language model|llm|ai assistant|autonomous agent)s?\\b[^.\\n]{0,40}\\b(must|should|will|shall)\\b"
         "\\bas an ai\\b"))
  "The signal families as data: one rule set to read, tune and test.")

(defparameter +base64-blob-scanner+
  (cl-ppcre:create-scanner "[A-Za-z0-9+/]{200,}={0,2}")
  "An encoded blob long enough that no page's prose is one.")

(defstruct (family (:constructor make-family (name weight scanners)))
  "One compiled signal family: its NAME, its WEIGHT, and its SCANNERS."
  (name "" :type string :read-only t)
  (weight 0 :type integer :read-only t)
  (scanners '() :read-only t))

(defstruct (finding (:constructor make-finding (family weight samples spans)))
  "One family's hit: its FAMILY and WEIGHT, up to three SAMPLES of the
matched text, and the SPANS (START . END) it matched in the scanned text."
  (family "" :type string :read-only t)
  (weight 0 :type integer :read-only t)
  (samples '() :read-only t)
  (spans '() :read-only t))

(defstruct (verdict (:constructor make-verdict (score findings hidden-chars base64-blobs)))
  "One scan's answer. SCORE is 0 unless the threshold was reached, so a
verdict only weak signals raised reads as clean while still naming them."
  (score 0 :type integer :read-only t)
  (findings '() :read-only t)
  (hidden-chars 0 :type integer :read-only t)
  (base64-blobs 0 :type integer :read-only t))

(defun merged-options (base extra)
  "BASE with EXTRA's keys set over it, so one pattern can overrule a family's."
  (let ((options (copy-list base)))
    (loop for (key value) on extra by #'cddr
          do (setf (getf options key) value))
    options))

(defun compile-pattern (label source options)
  "SOURCE compiled under OPTIONS, or NIL with one warning: a bad pattern must
not take the family down with it."
  (handler-case (apply #'cl-ppcre:create-scanner source options)
    (error (condition)
      (warn "injection-guard: ~a dropped a pattern: ~a" label (princ-to-string condition))
      nil)))

(defun compile-family (spec)
  "SPEC compiled into a FAMILY."
  (destructuring-bind (name weight options . patterns) spec
    (make-family
     name weight
     (loop for pattern in patterns
           for (source . own) = (if (consp pattern) pattern (list pattern))
           for scanner = (compile-pattern name source (merged-options options own))
           when scanner collect scanner))))

(defparameter *families* (mapcar #'compile-family +families+)
  "The families, compiled once at load.")

;;; --- one scan --------------------------------------------------------------

(defun collapsed (text)
  "TEXT with every run of whitespace collapsed to one space, and no leading or
trailing space."
  (with-output-to-string (out)
    (let ((pending nil) (wrote nil))
      (loop for character across text
            do (if (member character '(#\Space #\Tab #\Newline #\Return #\Page))
                   (setf pending t)
                   (progn (when (and pending wrote) (write-char #\Space out))
                          (setf pending nil wrote t)
                          (write-char character out)))))))

(defun family-hits (family text)
  "FAMILY read against TEXT => (values SAMPLES SPANS): at most three samples
and twelve spans, in the order its patterns find them."
  (let ((samples '()) (spans '()))
    (block reading
      (dolist (scanner (family-scanners family))
        (loop for (start end) on (cl-ppcre:all-matches scanner text) by #'cddr
              do (let ((sample (collapsed (subseq text start (min end (+ start 90))))))
                   (when (and (plusp (length sample)) (not (member sample samples :test #'string=)))
                     (push sample samples))
                   (when (> end start) (push (cons start end) spans)))
              when (and (>= (length samples) 3) (>= (length spans) 12))
                do (return-from reading nil))))
    (values (nreverse samples) (nreverse spans))))

(defun count-base64-blobs (text)
  "How many oversized encoded blobs TEXT carries."
  (floor (length (cl-ppcre:all-matches +base64-blob-scanner+ text)) 2))

(defun scan (text &key (threshold 6) (exempt '()))
  "TEXT's verdict. EXEMPT names families to skip for this call -- what a
local file read needs, where attack-shaped text is the file's own content.
A score of 0 means clean; the findings are kept either way."
  (if (or (null text) (zerop (length text)))
      (make-verdict 0 '() 0 0)
      (let ((findings '()) (total 0))
        (dolist (family *families*)
          (unless (member (family-name family) exempt :test #'string-equal)
            (multiple-value-bind (samples spans) (family-hits family text)
              (when samples
                (push (make-finding (family-name family) (family-weight family) samples spans)
                      findings)
                (incf total (family-weight family))))))
        (let* ((findings (nreverse findings))
               (hidden (count-hidden text))
               (blobs (count-base64-blobs text)))
          ;; Hidden characters are a strong signal on their own but capped, so a
          ;; file full of stray BOMs cannot alone exceed the threshold.
          (when (plusp hidden) (incf total (min hidden 6)))
          (when (plusp blobs) (incf total (min (* blobs 2) 4)))
          (if (< total threshold)
              (make-verdict 0 findings hidden blobs)
              (make-verdict total findings hidden blobs))))))

(defun verdict-flagged-p (verdict)
  "Whether VERDICT is one the guard acts on."
  (plusp (verdict-score verdict)))

(defun verdict-families (verdict)
  "The names of the families VERDICT's findings name, in the order read."
  (mapcar #'finding-family (verdict-findings verdict)))

(defun verdict-summary (verdict)
  "VERDICT in a few words: what fired and how loud, or clean."
  (let ((parts (loop for finding in (verdict-findings verdict)
                     collect (format nil "~a(+~a)" (finding-family finding) (finding-weight finding)))))
    (when (plusp (verdict-hidden-chars verdict))
      (setf parts (nconc parts (list (format nil "hidden_unicode(~a)" (verdict-hidden-chars verdict))))))
    (when (plusp (verdict-base64-blobs verdict))
      (setf parts (nconc parts (list (format nil "base64_blob(~a)" (verdict-base64-blobs verdict))))))
    (if parts (format nil "~{~a~^, ~}" parts) "clean")))
