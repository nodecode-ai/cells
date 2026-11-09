;;;; help.lisp --- what a program's own help says, and what a command asks of it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Both halves are pure: PARSE-HELP reads one help page into the options and
;;;; subcommands a card records, and CHECK-SEGMENT reads one command's words
;;;; against a card. Nothing here touches a process or the disk, so the tests
;;;; drive it with text.
;;;;
;;;; The checker keeps the original's strictness: only long options are
;;;; judged, only on a command whose help was read whole, never after --, and
;;;; never on a word the shell will expand. Everything else passes silently.

(in-package #:nodecode-fluent)

;;; --- text ------------------------------------------------------------------

(defun split-lines (text)
  "TEXT's lines, without their terminators."
  (let ((lines '()) (start 0))
    (loop for i from 0 below (length text)
          when (char= (char text i) #\Newline)
            do (push (subseq text start i) lines) (setf start (1+ i)))
    (push (subseq text start) lines)
    (mapcar (lambda (line)
              (if (and (plusp (length line)) (char= (char line (1- (length line))) #\Return))
                  (subseq line 0 (1- (length line)))
                  line))
            (nreverse lines))))

(defun blankp (line)
  (every (lambda (c) (member c '(#\Space #\Tab))) line))

(defun trimmed (text)
  (string-trim '(#\Space #\Tab) text))

(defun words-of (text)
  "TEXT split on whitespace."
  (let ((words '()) (start nil))
    (flet ((cut (end) (when start (push (subseq text start end) words) (setf start nil))))
      (loop for i from 0 below (length text)
            do (if (member (char text i) '(#\Space #\Tab))
                   (cut i)
                   (unless start (setf start i))))
      (cut (length text)))
    (nreverse words)))

(defun base-name (word)
  "WORD without a directory: /usr/local/bin/docker reads docker."
  (let ((slash (position #\/ word :from-end t)))
    (if slash (subseq word (1+ slash)) word)))

(defun dynamicp (word)
  "Is WORD one the shell expands, so its spelling says nothing?"
  (some (lambda (c) (find c "$*?[{\\`(" :test #'char=)) word))

;;; --- one option line ---------------------------------------------------------

(defun spec-end (line)
  "Where LINE's option spec stops: the first tab, or the first of two spaces."
  (let ((n (length line)))
    (loop for i from 0 below n
          when (char= (char line i) #\Tab) return i
          when (and (char= (char line i) #\Space)
                    (< (1+ i) n)
                    (char= (char line (1+ i)) #\Space))
            return i)))

(defun spec-tokens (spec)
  "SPEC's words, cut at spaces, commas, pipes and the = that opens a value."
  (let ((tokens '()) (start nil) (n (length spec)))
    (flet ((cut (end) (when start (push (subseq spec start end) tokens) (setf start nil))))
      (loop for i from 0 below n
            do (if (member (char spec i) '(#\Space #\Tab #\, #\| #\=))
                   (cut i)
                   (unless start (setf start i))))
      (cut n))
    (nreverse tokens)))

(defun option-name (token)
  "TOKEN's option name, --[no-]quiet reading as --quiet, or NIL when it is no name."
  (let* ((dashes (if (and (> (length token) 1) (char= (char token 1) #\-)) 2 1))
         (body (subseq token dashes)))
    (when (and (> (length body) 5) (string= "[no-]" body :end2 5))
      (setf body (subseq body 5)))
    (when (and (plusp (length body))
               (alphanumericp (char body 0))
               (every (lambda (c) (or (alphanumericp c) (member c '(#\- #\? #\@)))) body))
      (concatenate 'string (subseq token 0 dashes) body))))

(defun option-line-p (line)
  "Does LINE start an option: -x or --long, at most indented?"
  (let ((text (trimmed line)))
    (and (> (length text) 2)
         (char= (char text 0) #\-)
         (let ((c (char text 1)))
           (or (char= c #\-) (alphanumericp c))))))

(defun option-line (line)
  "LINE read as one option: (:names (...) :value :no/:yes/:maybe :desc TEXT), or NIL."
  (let* ((text (trimmed line))
         (end (spec-end text))
         (spec (if end (subseq text 0 end) text))
         (desc (trimmed (if end (subseq text end) "")))
         (names '())
         (value :no))
    (dolist (token (spec-tokens spec))
      (let ((name (option-name token)))
        (cond (name (pushnew name names :test #'equal))
              ((plusp (length token)) (setf value :yes)))))
    (when names
      (list :names (nreverse names) :value value :desc desc))))

;;; --- one help page -----------------------------------------------------------

(defun header-kind (line)
  "The section a header line opens: :commands, :flags, :other, or NIL."
  (let ((text (trimmed line)))
    (when (and (> (length text) 1)
               (char= (char text (1- (length text))) #\:)
               (<= (length (words-of text)) 4))
      (let ((down (string-downcase text)))
        (cond ((search "command" down) :commands)
              ((search "usage" down) :usage)
              ((or (search "flag" down) (search "option" down)) :flags)
              (t :other))))))

(defun lowercase-word-p (word)
  (and (plusp (length word))
       (lower-case-p (char word 0))
       (every (lambda (c) (or (lower-case-p c) (digit-char-p c) (member c '(#\- #\. #\_))))
              word)))

(defun command-line (line)
  "The subcommand LINE lists, as (values NAME SUMMARY), or NIL."
  (let* ((text (trimmed line))
         (gap (search "  " text))
         (name (and gap (subseq text 0 gap)))
         (summary (and gap (trimmed (subseq text (+ gap 2))))))
    (when (and gap (lowercase-word-p name) summary (plusp (length summary)))
      (values name summary))))

(defun parse-help (text)
  "TEXT, one help page, as (:flags (...) :subcommands (...) :certain BOOL)."
  (let ((flags '()) (subcommands '()) (usage nil) (state :other))
    (dolist (line (split-lines (or text "")))
      (cond
        ((blankp line))
        ((header-kind line)
         (setf state (header-kind line))
         (when (eq state :usage) (setf usage t)))
        ((option-line-p line)
         (let ((option (option-line line)))
           (when option (pushnew option flags :test (lambda (a b) (equal (getf a :names) (getf b :names)))))))
        ((eq state :commands)
         (multiple-value-bind (name summary) (command-line line)
           (when (and name (not (assoc name subcommands :test #'equal)))
             (push (list :name name :summary summary :flags '() :certain nil) subcommands))))
        (t (let ((head (string-downcase (trimmed line))))
             (when (or (eql 0 (search "usage:" head)) (eql 0 (search "usage " head)))
               (setf usage t))))))
    (list :flags (nreverse flags)
          :subcommands (nreverse subcommands)
          :certain (and usage (plusp (length flags))))))

;;; --- reading a command -------------------------------------------------------

(defun split-commands (text)
  "TEXT's commands, each a list of words: an unquoted ; & | or newline starts the next."
  (let ((commands '()) (words '()) (word (make-string-output-stream))
        (in-word nil) (quote nil) (i 0) (n (length text)))
    (labels ((end-word () (when in-word (push (get-out word) words) (setf in-word nil)))
             (end-command () (end-word) (when words (push (nreverse words) commands) (setf words nil))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond
                   ((and quote (char= c #\\)) (write-char c word) (setf in-word t) (incf i)
                    (when (< i n) (write-char (char text i) word) (incf i)))
                   (quote (if (char= c quote) (setf quote nil) (write-char c word))
                    (setf in-word t) (incf i))
                   ((member c '(#\" #\')) (setf quote c in-word t) (incf i))
                   ((char= c #\\) (incf i) (when (< i n) (write-char (char text i) word) (setf in-word t)) (incf i))
                   ((member c '(#\Space #\Tab)) (end-word) (incf i))
                   ((member c '(#\; #\& #\| #\Newline)) (end-command) (incf i))
                   (t (write-char c word) (setf in-word t) (incf i)))))
      (end-command))
    (nreverse commands)))

(defun get-out (stream)
  (get-output-stream-string stream))

(defun program-of (words)
  "WORDS' program and the words after it, wrappers and assignments stepped over."
  (let ((rest words))
    (loop
      (let ((word (first rest)))
        (cond ((null word) (return nil))
              ((find #\= word) (setf rest (rest rest)))
              ((member word '("sudo" "doas" "env" "nohup" "exec" "command" "nice" "time") :test #'equal)
               (setf rest (rest rest)))
              ((equal word "timeout") (setf rest (cddr rest)))
              ((and (plusp (length word)) (char= (char word 0) #\-)) (setf rest (rest rest)))
              (t (return (values (base-name word) (cdr rest)))))))))

;;; --- judging one command -----------------------------------------------------

(defun long-names (flags)
  (let ((names '()))
    (dolist (flag flags) (dolist (name (getf flag :names)) (pushnew (string-downcase name) names :test #'equal)))
    (nreverse names)))

(defun flag-named (flags name)
  (find-if (lambda (flag) (member name (getf flag :names) :test #'equal)) flags))

(defun unique-prefix (name names)
  "The one long option NAME abbreviates, or NIL."
  (let ((hits (remove-if-not (lambda (candidate)
                                (and (< (length name) (length candidate))
                                     (string= name candidate :end2 (length name))))
                              names)))
    (and (= 1 (length (remove-duplicates hits :test #'equal))) (first hits))))

(defun edit-distance (a b)
  (let* ((n (length a)) (m (length b))
         (d (make-array (list (1+ n) (1+ m)) :initial-element 0)))
    (dotimes (i (1+ n)) (setf (aref d i 0) i))
    (dotimes (j (1+ m)) (setf (aref d 0 j) j))
    (loop for i from 1 to n do
      (loop for j from 1 to m do
        (setf (aref d i j)
              (min (1+ (aref d (1- i) j))
                   (1+ (aref d i (1- j)))
                   (+ (aref d (1- i) (1- j))
                      (if (char-equal (char a (1- i)) (char b (1- j))) 0 1))))))
    (aref d n m)))

(defun closest (name names)
  "The option NAME most likely meant, or NIL when none is close enough."
  (let ((best nil) (distance nil))
    (dolist (candidate names)
      (let ((d (edit-distance name candidate)))
        (when (or (null distance) (< d distance))
          (setf best candidate distance d))))
    (when (and best (<= distance (max 1 (floor (length name) 3)))) best)))

(defun known-option-p (flags name)
  (or (flag-named flags name)
      (member name '("--help" "--version") :test #'equal)
      (and (> (length name) 5)
           (string= "--no-" name :end2 5)
           (flag-named flags (concatenate 'string "--" (subseq name 5))))))

(defun judge-option (word flags certain program path)
  "WORD's finding, and whether the word after it is its value."
  (let ((name (subseq word 0 (or (position #\= word) (length word)))))
    (cond
      ((not (and certain (> (length name) 2) (string= "--" name :end2 2))) (values nil nil))
      ((dynamicp name) (values nil nil))
      (t (let ((flag (flag-named flags name)))
           (cond (flag (values nil (eq :yes (getf flag :value))))
                 ((unique-prefix name (long-names flags)) (values nil nil))
                 ((known-option-p flags name) (values nil nil))
                 (t (values (list :program program
                                  :command (format nil "~{~a~^ ~}" (cons program path))
                                  :token name
                                  :suggestion (or (closest name (long-names flags)) ""))
                            nil))))))))

(defun check-segment (tokens card)
  "Every finding TOKENS earn against CARD, in the order they are written."
  (let* ((program (getf card :program))
         (flags (getf card :flags))
         (subcommands (getf card :subcommands))
         (certain (getf card :certain))
         (path '())
         (findings '())
         (stop nil)
         (words (rest tokens)))
    ;; 1. the subcommand walk: an option met on the way belongs to the command
    ;; walked so far; a word that is no subcommand ends the walk, and nothing
    ;; past it is judged -- it may be an alias or a plugin's own name.
    (loop while (and words subcommands (not stop))
          do (let ((word (first words)))
               (cond
                 ((equal word "--") (setf stop t))
                 ((and (plusp (length word)) (char= (char word 0) #\-))
                  (multiple-value-bind (finding takes) (judge-option word flags certain program path)
                    (when finding (push finding findings))
                    (setf words (rest words))
                    (when takes (setf words (rest words)))))
                 (t (let ((sub (find word subcommands :test #'equal :key (lambda (s) (getf s :name)))))
                      (if sub
                          (setf flags (getf sub :flags)
                                certain (getf sub :certain)
                                path (append path (list word))
                                subcommands nil
                                words (rest words))
                          (setf stop t)))))))
    ;; 2. the options of the command finally resolved.
    (when (and (not stop) (null subcommands))
      (let ((skip-next nil))
        (loop while words
              do (let ((word (first words)))
                   (setf words (rest words))
                   (cond
                     ((equal word "--") (setf words nil))
                     (skip-next (setf skip-next nil))
                     ((not (plusp (length word))) nil)
                     ((not (char= (char word 0) #\-)) nil)
                     ((dynamicp word) nil)
                     (t (multiple-value-bind (finding takes) (judge-option word flags certain program path)
                          (when finding (push finding findings))
                          (setf skip-next takes))))))))
    (nreverse findings)))

(defparameter *cards* (make-hash-table :test #'equal)
  "The cards read so far, by program name: what a check reads.")

(defun check-command (command &optional (cards *cards*))
  "Every finding COMMAND earns from the cards read so far, in order."
  (loop for words in (split-commands (or command ""))
        append (multiple-value-bind (program rest) (program-of words)
                 (let ((card (and program (not (dynamicp program)) (gethash program cards))))
                   (when card (check-segment (cons program rest) card))))))
