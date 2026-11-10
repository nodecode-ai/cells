;;;; cell.lisp --- earned trust: a level gates a call, a proof raises it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of percyhendrux123-prog's hermes-standing (MIT); the
;;;; repository, its commit and what was left out are in NOTICE. The whole
;;;; cell is one :TOOL hook, one tool, one command and a small local record:
;;;;
;;;;   - A LEVEL, L0 to L3, per actor, kept in a file of the nodecode home.
;;;;     L0 reads; L1 also writes inside the workspace; L2 also runs shells,
;;;;     sends, spins, and does what it cannot classify; L3 does anything.
;;;;     Below the level a call needs, the operator is asked Allow once or
;;;;     Deny, and a call nobody allowed does not run.
;;;;   - A PROOF: standing_proof re-runs a check itself -- a file's sha256 or
;;;;     a text in it, a page's text over an allowlisted URL, an exact
;;;;     allowlisted command's stdout -- and the record counts it. Enough
;;;;     passes raise the level; one failed check lowers it.
;;;;   - A RECORD: /standing shows every actor's level, passes and fails, and
;;;;     /standing log shows the events, and /standing set is the owner's own
;;;;     word on a level, which no model can say.
;;;;
;;;; A call is read, not grepped: Nodecode is a mono-tool organism, so what a
;;;; call does is the form it evaluates. The form's spawn calls carry a
;;;; command that the shell rules read; a write's path decides whether it is
;;;; a workspace write or a foreign one; a form that does not read, or whose
;;;; command hides behind a variable, is unknown and asks below L2.

(defpackage #:nodecode-standing
  (:use #:cl)
  (:nicknames #:standing))

(in-package #:nodecode-standing)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Standing gates a call on the trust this actor has earned, and a check it ran
itself is what earns it: functions in the standing: package, called through
eval. Each answers a string.
  (standing:proof \"file\" \"/path/to/artifact\" \"sha256-or-text\")   a
      file's SHA-256 (64 hex, optionally sha256: prefixed) or a text it must
      contain, inside the configured file proof roots -- never a credential
      store.
  (standing:proof \"http\" \"https://host/health\" \"ok\")   a page under an
      allowlisted URL prefix; its body must contain the text.
  (standing:proof \"command\" \"git status --short\" \"clean\")   an exact
      allowlisted command, exiting 0, whose output contains the text.
  (standing:status)   this actor's level, passes and fails, and the levels'
      map.
A pass earns one point and a proof handle; enough passes raise the level by
one, up to max_auto_level. A completed check that does not match lowers it by
one. A check that could not run -- an unconfigured target, a missing file,
a command not on the list -- earns nothing and costs nothing.
What a level lets through without asking the operator:
  L0  reads and computations.
  L1  also writes whose path resolves inside the workspace.
  L2  also shells, sends, spins and anything this cell cannot classify.
  L3  anything. Spending and deletion still ask below L3, as does a write
      outside the workspace.
The operator sets levels with /standing set <actor> <L0-L3> [reason], reads
them with /standing, and reads the events with /standing log <actor>."
  "What (help :standing) answers while the cell runs.")

(defparameter *levels-map-text*
  "L0 read; L1 + workspace writes; L2 + shell, send, spin, unknown; L3 all"
  "The one line /standing puts over its table.")

;;; --- what a category is --------------------------------------------------------

(defparameter +categories+ '(:read :local_write :write :shell :send :spend :spin :scrub :unknown))

(defparameter +level-categories+
  '((0 :read)
    (1 :read :local_write)
    (2 :read :local_write :shell :send :spin :unknown))
  "What each level lets through without asking. L3 asks about nothing.
A category outside a level's list asks; :write and :scrub and :spend ask at
every level below 3, which is the original's map.")

(defparameter +spawn-heads+ '("sh" "run-program" "launch-program" "run-shell-command")
  "The heads whose argument is a command the call runs.")

(defparameter +write-heads+ '("write-file" "write-file-string" "write-file-lines" "append-file"
                              "edit" "copy-file")
  "The heads that write a file at their first string argument.")

(defparameter +scrub-heads+ '("delete-file" "delete-directory" "delete-directory-tree"
                              "rename-file" "move-file")
  "The heads that remove or move something on disk.")

(defparameter +send-heads+ '("send-email" "send-mail" "send-message" "post-message"
                             "post-tweet" "publish" "webhook")
  "The heads that put something in front of other people.")

(defparameter +spend-heads+ '("pay" "payment" "purchase" "buy" "checkout" "charge"
                              "refund" "create-payment")
  "The heads that move money.")

(defparameter +spin-heads+ '("deploy" "create-job" "cron-create" "schedule-job" "start-worker")
  "The heads that start work of their own.")

(defparameter +command-rules+
  '((:scrub "\\b(?:rm|mv|unlink|rmdir|truncate)\\b|\\bgit\\s+(?:reset|clean)\\b")
    (:send "\\bgit\\s+push\\b|\\bgh\\s+(?:pr\\s+(?:create|merge)|release\\s+create)\\b|\\bcurl\\b[^\\n]*-X\\s*(?:POST|PUT|PATCH|DELETE)")
    (:spin "\\b(?:deploy|terraform\\s+apply|kubectl\\s+apply|crontab)\\b"))
  "Read against a command a call runs: an unquoted rm is a scrub, a git push
is a send, a deploy is a spin. A category it finds asks at L2.")

;;; --- the record ----------------------------------------------------------------

(nlk:define-peripheral standing :not-running t)

(defvar *lock* (bt2:make-lock :name "standing record")
  "Held across one read-modify-write of the record: the hook, the tool and the
command all reach it from different threads.")

(defparameter *event-cap* 200
  "How many events the record keeps, newest first.")

(defparameter *receipts* (make-hash-table :test #'equal)
  "The sessions that passed a proof since this image started, keyed by
actor and session id. Memory-only: a restart starts owing a proof.")

(defun as-path (value)
  "VALUE, a :path setting or a string, as a pathname, or NIL."
  (typecase value
    (pathname value)
    (string (and (plusp (length value)) (uiop:parse-native-namestring value)))
    (t nil)))

(defun list-setting (key)
  "KEY's value as a list: absent, a vector, or a list, whichever the section
materialized."
  (let ((value (setting key)))
    (cond ((null value) '())
          ((eq value :nil) '())
          ((listp value) value)
          ((vectorp value) (coerce value 'list))
          (t '()))))

(defun ledger-path ()
  "The file the record lives in: the setting when the operator names one, else
`standing/ledger.sexp' in the nodecode home."
  (or (as-path (setting :ledger))
      (nlk:home "standing/ledger.sexp")))

(defun load-record ()
  "The record, :actors and :events, as the plist that was written. A missing
or unreadable file is an empty record, never an error."
  (handler-case
      (if (probe-file (ledger-path))
          (with-open-file (in (ledger-path) :direction :input)
            ;; CL-USER, not KEYWORD: read under the keyword package and every
            ;; NIL in the file comes back as KEYWORD::NIL, which is no list.
            (let ((*read-eval* nil) (*package* (find-package '#:cl-user)))
              (let ((data (read in nil nil)))
                ;; A record another run or a hand edit left half-written is
                ;; empty where it does not read as a list, never an error.
                (if (consp data)
                    (list :actors (let ((actors (getf data :actors)))
                                    (if (listp actors) actors '()))
                          :events (let ((events (getf data :events)))
                                    (if (listp events) events '())))
                    (list :actors '() :events '())))))
          (list :actors '() :events '()))
    (error () (list :actors '() :events '()))))

(defun save-record (data)
  "Write DATA to the record's file, making its folder first."
  (let ((path (ledger-path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (let ((*print-readably* t) (*print-pretty* nil))
        (prin1 data out)))))

(defun level-of (value)
  "VALUE, \"L0\" to \"L3\" or 0 to 3, as a number."
  (typecase value
    (integer (max 0 (min 3 value)))
    (string (or (and (find (char-upcase (char value 0)) "L" :test #'char=)
                      (ignore-errors (parse-integer value :start 1)))
                1))
    (t 1)))

(defun level-value (value) (level-of value))

(defun flag-setting (key)
  "KEY's value as a boolean: a false or absent member is NIL, and so is the
sentinel an absent boolean comes in."
  (let ((value (setting key)))
    (and value (not (eq value :nil)))))

(defun number-setting (key default)
  "KEY's value when it is a number, else DEFAULT."
  (let ((value (setting key)))
    (if (integerp value) value default)))

(defun actor-row (data name)
  "NAME's row in DATA, made at the default level when it is new."
  (or (assoc name (getf data :actors) :test #'equal)
      (let ((row (list name (level-of (setting :level)) 0 0 (nlk:iso-now))))
        (push row (getf data :actors))
        row)))

(defun add-event (data actor kind result reason)
  "One event on DATA's log, newest first, capped at *EVENT-CAP*."
  (push (list (nlk:iso-now) actor kind result reason) (getf data :events))
  (when (> (length (getf data :events)) *event-cap*)
    (setf (getf data :events) (subseq (getf data :events) 0 *event-cap*)))
  data)

(defun record-level (actor)
  "ACTOR's level, as the record has it."
  (bt2:with-lock-held (*lock*)
    (second (actor-row (load-record) actor))))

(defun record-proof (actor kind passed reason)
  "Count one completed check. PASSED is :pass, :fail or :unavailable.
=> (values LEVEL PASSES FAILS)."
  (bt2:with-lock-held (*lock*)
    (let* ((data (load-record))
           (row (actor-row data actor))
           (level (second row)) (passes (third row)) (fails (fourth row)))
      (cond ((eq passed :pass)
             (setf passes (1+ passes))
             (when (and (>= passes (number-setting :promote-after 10))
                        (< level (level-of (setting :max-auto-level))))
               (setf level (1+ level) passes 0)))
            ((eq passed :fail)
             (setf level (max 0 (1- level)) passes 0 fails (1+ fails))))
      (setf (second row) level (third row) passes (fourth row) fails
            (fifth row) (nlk:iso-now))
      (add-event data actor kind (string-downcase (string passed)) reason)
      (save-record data)
      (values level passes fails))))

(defun record-decision (actor kind result)
  "One action-gate decision on the log: KIND the categories, RESULT what was
decided. A busy gate writes at most once per call, and a call it let through
quietly is not on the log at all."
  (bt2:with-lock-held (*lock*)
    (save-record (add-event (load-record) actor kind result ""))))

(defun set-actor-level (actor level reason)
  "The owner's word on ACTOR's level: LEVEL is what it is now, not a step."
  (bt2:with-lock-held (*lock*)
    (let* ((data (load-record))
           (row (actor-row data actor)))
      (setf (second row) level (third row) 0 (fifth row) (nlk:iso-now))
      (add-event data actor "owner" (format nil "L~d" level) (or reason ""))
      (save-record data)
      level)))

(defun record-rows ()
  "Every actor's row, by name."
  (sort (copy-list (getf (load-record) :actors)) #'string-lessp :key #'first))

(defun record-events (actor)
  "ACTOR's last twenty events, newest first."
  (let ((events (getf (load-record) :events)))
    (subseq events 0 (min 20 (length events)))))

;;; --- reading a call ------------------------------------------------------------

(defun head-name (x)
  "X's operator name, lowercased, or NIL."
  (and (consp x) (symbolp (car x)) (string-downcase (symbol-name (car x)))))

(defun read-forms (text)
  "Every toplevel form of TEXT, read as the eval tool reads them, or NIL when
one does not read. No #. runs: a form that needs the reader to evaluate is
unknown, which asks below L2."
  (when (stringp text)
    (handler-case
        (with-input-from-string (in text)
          (let ((*package* (find-package '#:nodecode.evolved))
                (*read-eval* nil)
                (forms '()))
            (loop for form = (read in nil :eof)
                  until (eq form :eof)
                  do (push form forms))
            (nreverse forms)))
      (error () nil))))

(defun literal-text (x strings)
  "The text X alone names, or how to read it. => (values TEXT HIDDEN-P)."
  (cond ((stringp x) (values x nil))
        ((and (consp x) (member (head-name x) '("$") :test #'equal))
         (let ((text (and strings (gethash (second x) strings))))
           (if (stringp text) (values text nil) (values nil t))))
        ((and (consp x) (member (head-name x) '("list" "quote") :test #'equal))
         (let ((items (if (equal (head-name x) "quote") (second x) (rest x))))
           (if (and (listp items) (every #'stringp items))
               (values (format nil "~{~a~^ ~}" items) nil)
               (values nil t))))
        (t (values nil t))))

(defun classify-command (text)
  "The categories a command's text shows."
  (loop for (category pattern) in +command-rules+
        when (ppcre:scan pattern text) collect category))

(defun http-method (x)
  "The method HTTP's call names, as a keyword, or NIL."
  (let ((method (second x)))
    (cond ((keywordp method) method)
          ((and (stringp method) (find-symbol (string-upcase method) '#:keyword))
           (intern (string-upcase method) '#:keyword))
          (t nil))))

(defun workspace-root ()
  "The configured workspace as a directory pathname, or NIL."
  (let ((path (as-path (setting :workspace))))
    (and path (uiop:ensure-directory-pathname path))))

(defun inside-p (path root)
  "Whether PATH, resolved, sits inside ROOT, resolved."
  (handler-case
      (let ((real (or (uiop:probe-file* path) path))
            (real-root (or (uiop:probe-file* root) root)))
        (and (uiop:subpathp (uiop:ensure-pathname real :want-absolute t) real-root) t))
    (error () nil)))

(defun resolve-path (value cwd)
  "VALUE as an absolute pathname, relative to CWD."
  (let ((path (uiop:parse-native-namestring value)))
    (if (uiop:absolute-pathname-p path)
        path
        (uiop:merge-pathnames* path cwd))))

(defun classify (forms strings cwd)
  "The categories FORMS need, and whether every write stays in the workspace.
=> (values CATEGORIES LOCAL-P)."
  (let ((categories '()) (paths '()) (workspace (workspace-root)))
    (labels ((add (category) (pushnew category categories))
             (walk (x)
               (cond
                 ((consp x)
                  (let ((name (head-name x)))
                    (cond
                      ((member name +spawn-heads+ :test #'equal)
                       (add :shell)
                       (multiple-value-bind (text hidden) (literal-text (second x) strings)
                         (cond (text (dolist (category (classify-command text)) (add category)))
                               (hidden (add :unknown)))))
                      ((member name +write-heads+ :test #'equal)
                       (add :write)
                       (let ((target (second x)))
                         (when (stringp target) (push target paths))))
                      ((member name +scrub-heads+ :test #'equal) (add :scrub))
                      ((member name +send-heads+ :test #'equal) (add :send))
                      ((member name +spend-heads+ :test #'equal) (add :spend))
                      ((member name +spin-heads+ :test #'equal) (add :spin))
                      ((equal name "http")
                       (if (member (http-method x) '(:get :head)) (add :read) (add :send)))))
                  (walk (car x))
                  (walk (cdr x))))))
      (dolist (form forms) (walk form)))
    (values (or (nreverse categories) '(:read))
            (and paths
                 (loop for path in paths
                       always (and workspace (inside-p (resolve-path path cwd) workspace)))))))

(defun call-categories (form strings cwd)
  "=> (values CATEGORIES LOCAL-P) for the eval call's FORM."
  (let ((forms (read-forms form)))
    (if forms
        (classify forms strings cwd)
        (values '(:unknown) nil))))

;;; --- levels --------------------------------------------------------------------

(defun allowed-p (level categories local)
  "Whether LEVEL lets CATEGORIES through: a workspace write is a local_write,
and L3 lets everything through."
  (or (>= level 3)
      (let ((allowed (cdr (assoc level +level-categories+))))
        (every (lambda (category)
                 (member (if (and (eq category :write) local) :local_write category)
                         allowed))
               categories))))

(defun category-text (categories)
  (format nil "~{~(~a~)~^, ~}" categories))

;;; --- asking the operator -------------------------------------------------------

(defvar *ask* nil
  "A stand-in for the core's ASK-OPERATOR: (lambda (title &key detail choices
seconds) => (values answer why)). NIL asks the organism; a test binds one.")

(defun ask-operator-function ()
  "The core's ASK-OPERATOR, or NIL in a core that has none."
  (let ((symbol (find-symbol "ASK-OPERATOR" '#:nodecode.evolved)))
    (and symbol (fboundp symbol) (symbol-function symbol))))

(defun ask-allow-p (actor level categories detail)
  "Ask Allow once or Deny in the surface that answers questions.
=> T when the operator allowed this one call."
  (let ((ask (and (flag-setting :ask) (or *ask* (ask-operator-function)))))
    (and ask
         (equal "Allow once"
                (funcall ask
                         (format nil "Standing: ~a is at L~d; this call is ~a"
                                 actor level categories)
                         :detail detail
                         :choices (list "Allow once" "Deny")
                         :seconds (max 5 (number-setting :ask-seconds 120)))))))

;;; --- the gate ------------------------------------------------------------------

(defun refusal-text (actor level categories)
  (format nil "ERROR: refused by nodecode-standing: ~a is at L~d, which does not ~
allow a ~a call; the call did not run -- do not retry it as it stands. Either ~
work at this level, or ask the operator to raise it with /standing set ~a L~d ~
(or L3 for anything)."
          actor level categories actor (min 3 (1+ level))))

(defun proof-owed-text (actor tool)
  (format nil "ERROR: refused by nodecode-standing: ~a is set to run a ~a call ~
only after a passing proof in this session, and there is none; call ~
standing_proof first, then run the call."
          actor tool))

(defun session-id ()
  "The running turn's session id, or empty outside one."
  (or (getf (nle:turn) :session-id) ""))

(defun call-cwd (session)
  "The session's folder, else this process's."
  (or (ignore-errors (nlk:find-session-cwd session))
      (uiop:getcwd)))

(defun call-form (op)
  "The eval call's form and strings object."
  (let ((arguments (getf op :arguments)))
    (if (hash-table-p arguments)
        (values (gethash "form" arguments) (gethash "strings" arguments))
        (values nil nil))))

(defun judge (tool form strings cwd session)
  "Whether this call may run: the level's word, or the operator's.
=> (values ALLOW-P REFUSAL)."
  (let ((actor (setting :actor)))
    (if (and (member tool (list-setting :proof-before) :test #'equal)
             (not (gethash (list actor session) *receipts*)))
        (values nil (proof-owed-text actor tool))
        (multiple-value-bind (categories local) (call-categories form strings cwd)
          (let ((level (record-level actor)))
            (cond
              ((allowed-p level categories local) (values t nil))
              ((ask-allow-p actor level (category-text categories)
                             (if (> (length (princ-to-string form)) 1200)
                  (concatenate 'string (subseq (princ-to-string form) 0 1200) " ...")
                  (princ-to-string form)))
               (record-decision actor (category-text categories) (format nil "asked, allowed at L~d" level))
               (values t nil))
              (t
               (record-decision actor (category-text categories) (format nil "refused at L~d" level))
               (values nil (refusal-text actor level (category-text categories))))))))))

(defun gate (op next)
  "The :TOOL point: an eval call the level does not cover asks the operator, and
a call nobody allowed does not run."
  (let ((tool (let ((name (getf op :name)))
                (if (stringp name) name (princ-to-string name)))))
    (if (not (equal tool "eval"))
        (funcall next op)
        (multiple-value-bind (form strings) (call-form op)
          (let ((session (session-id)))
            ;; A gate that cannot read its own record lets the call through: it
            ;; is advice over the core's own trust, never the last word, and no
            ;; call may fail because this cell could not tell.
            (multiple-value-bind (allow refusal)
                (handler-case (judge tool form strings (call-cwd session) session)
                  (error () (values t nil)))
              (if allow (funcall next op) (nle:failure refusal))))))))

;;; --- the proofs ----------------------------------------------------------------

(defparameter *http* nil
  "A stand-in for NLK:HTTP in a test: (lambda (url) => (values BODY STATUS)).
NIL dials for real; nothing here dials except a configured http proof.")

(defparameter +limit+ (* 2 1024 1024)
  "How many bytes of a file, a page or a command's output a check reads.")

(defun credential-name-p (name)
  "Whether NAME is a credential store's, which no file proof reads."
  (let ((name (string-downcase name)))
    (or (member name '(".ssh" ".aws" ".gnupg" "auth.json" ".netrc" ".npmrc" ".pypirc"
                       ".env" "credentials" "id_rsa" "id_ed25519" "id_ecdsa")
                :test #'equal)
        (and (ppcre:scan "credential|keychain|\\.pem$|\\.key$|^id_|\\.env" name) t))))

(defun credential-path-p (path)
  "Whether PATH names a credential store, by any part of its folder or its name."
  (or (some #'credential-name-p (rest (pathname-directory path)))
      (credential-name-p (file-namestring path))))

(defun proof-roots ()
  "The folders and files a file proof may read: the operator's own list, else
the workspace. Empty refuses every file proof."
  (let ((roots (list-setting :file-proof-roots)))
    (if roots
        (remove nil (mapcar #'as-path roots))
        (let ((workspace (as-path (setting :workspace))))
          (and workspace (list (uiop:ensure-directory-pathname workspace)))))))

(defun hex-digest-p (text)
  (and (= (length text) 64) (every (lambda (c) (digit-char-p c 16)) text)))

(defun subvector-p (needle haystack)
  "Whether HAYSTACK, octets, holds NEEDLE's octets."
  (and needle (search (sb-ext:string-to-octets needle :external-format :utf-8) haystack :test #'=)
       t))

(defun read-bytes (path)
  "PATH's first *LIMIT* bytes, as octets."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let* ((length (min +limit+ (or (ignore-errors (file-length in)) +limit+)))
           (buffer (make-array length :element-type '(unsigned-byte 8))))
      (subseq buffer 0 (read-sequence buffer in)))))

(defun file-proof (target expect cwd)
  "=> (values :pass, :fail or :unavailable, REASON)."
  (let* ((path (resolve-path target cwd))
         (roots (proof-roots)))
    (cond
      ((null roots) (values :unavailable "no file proof roots are configured"))
      ((credential-path-p path) (values :unavailable "the target names a credential store"))
      ((notany (lambda (root) (inside-p path root)) roots)
       (values :unavailable "the target is outside the configured file proof roots"))
      ((not (probe-file path)) (values :unavailable "the file is not there"))
      (t (let ((bytes (handler-case (read-bytes path) (error () nil))))
           (if (null bytes)
               (values :unavailable "the file could not be read")
               (let ((digest (string-downcase (string-trim " " (if (search "sha256:" expect :test #'char-equal)
                                                                (subseq expect 7) expect)))))
                 (cond
                   ((hex-digest-p digest)
                    (if (string-equal digest (subseq (nlk:sha256-text bytes) 7))
                        (values :pass "the file's sha256 matches")
                        (values :fail "the file's sha256 does not match")))
                   ((subvector-p expect bytes) (values :pass "the text is in the file"))
                   (t (values :fail "the text is not in the file"))))))))))

(defun url-allowed-p (target prefix)
  "Whether TARGET is PREFIX, or under it when PREFIX ends in a slash, with the
same origin and the exact query."
  (let* ((target (subseq target 0 (or (position #\# target) (length target))))
         (prefix (subseq prefix 0 (or (position #\# prefix) (length prefix))))
         (last (and (plusp (length prefix)) (char prefix (1- (length prefix))))))
    (if (eql last #\/)
        (and (>= (length target) (length prefix))
             (string-equal prefix target :end2 (length prefix)))
        (or (string-equal prefix target)
            (and (> (length target) (length prefix))
                 (string-equal prefix target :end2 (length prefix))
                 (eql (char target (length prefix)) #\?))))))

(defun http-proof (target expect)
  "=> (values :pass, :fail or :unavailable, REASON)."
  (let ((prefixes (list-setting :http-proof-targets)))
    (cond
      ((null prefixes) (values :unavailable "no http proof targets are configured"))
      ((notany (lambda (prefix) (url-allowed-p target prefix)) prefixes)
       (values :unavailable "the URL is outside the configured proof targets"))
      (t (handler-case
             (multiple-value-bind (body status)
                 (if *http*
                     (funcall *http* target)
                     (nlk:http :get target :timeout (number-setting :timeout 10) :max-bytes +limit+))
               (if (/= 200 (or status 0))
                   (values :fail (format nil "the page answered ~a" status))
                   (let ((text (if (stringp body)
                                   body
                                   (sb-ext:octets-to-string body :external-format :utf-8
                                                                 :end (min (length body) +limit+)))))
                     (if (search expect text) (values :pass "the text is on the page")
                         (values :fail "the text is not on the page")))))
           (error () (values :unavailable "the page could not be fetched")))))))

(defun split-words (text)
  (remove "" (uiop:split-string text :separator '(#\Space #\Tab)) :test #'equal))

(defun command-proof (target expect)
  "=> (values :pass, :fail or :unavailable, REASON). Target must be one exact
string on the operator's list; nothing else runs, and no shell reads it."
  (let ((words (split-words target))
        (allowed (mapcar #'split-words (list-setting :command-allowlist))))
    (cond
      ((null words) (values :unavailable "the command is empty"))
      ((not (member words allowed :test #'equal))
       (values :unavailable "that exact command is not on the operator's list"))
      (t (handler-case
             (multiple-value-bind (out err code)
                 (sb-sys:with-deadline (:seconds (max 1 (number-setting :timeout 10)))
                   (uiop:run-program words :output :string :error-output :string
                                            :ignore-error-status t))
               (declare (ignore err))
               (if (not (eql 0 code))
                   (values :fail (format nil "the command exited ~a" code))
                   (if (search expect (subseq out 0 (min (length out) +limit+)))
                       (values :pass "the command's output holds the text")
                       (values :fail "the command's output does not hold the text"))))
           (error () (values :unavailable "the command did not finish in time")))))))

(defun run-proof (kind target expect cwd)
  "=> (values :pass, :fail or :unavailable, REASON)."
  (cond
    ((not (and (stringp expect) (plusp (length expect))))
     (values :unavailable "a check needs a non-empty expected result"))
    ((not (stringp target)) (values :unavailable "a check needs a target string"))
    ((string-equal kind "file") (file-proof target expect cwd))
    ((string-equal kind "http") (http-proof target expect))
    ((string-equal kind "command") (command-proof target expect))
    (t (values :unavailable "choose a file, http or command check"))))

;;; --- the tool ------------------------------------------------------------------

(nlk:define-contract standing-proof
  (:doc "Independently check a file, an HTTP page, or an exact allowlisted
command, and have the pass counted in Standing's record. A pass earns one
point; enough passes raise this actor's level. A completed check that does not
match lowers it by one; a check that could not run earns nothing and costs
nothing. File proofs read only inside the configured proof roots and never a
credential store. Do not put secrets in a target or an expected result.")
  ("kind" :text :required t :options '("file" "http" "command")
   :doc "file: a path's sha256 or a text inside it; http: a page's body; command: an exact allowlisted command's output")
  ("target" :text :required t
   :doc "a file path, an http(s) URL under an allowlisted prefix, or one exact allowlisted command line")
  ("expect" :text :required t
   :doc "the text the check must find, or a file's SHA-256 (64 hex, sha256: prefixed or not)"))

(defun prove (proof)
  "standing_proof: run the check, count it, and say what the record says now."
  (let* ((actor (setting :actor))
         (session (session-id))
         (kind (standing-proof-kind proof))
         (target (standing-proof-target proof))
         (expect (standing-proof-expect proof)))
    (multiple-value-bind (passed reason)
        (run-proof kind target expect (call-cwd session))
      (multiple-value-bind (level passes fails)
          (record-proof actor kind passed reason)
        (when (eq passed :pass)
          (setf (gethash (list actor session) *receipts*) kind))
        (format nil "~a: ~a -- ~a is at L~d, ~d pass~:p this window, ~d fail~:p all time"
                (ecase passed (:pass "pass") (:fail "fail") (:unavailable "unavailable"))
                reason actor level passes fails)))))

;;; --- /standing -----------------------------------------------------------------

(defun status-text ()
  "The table /standing answers, and each actor's last event under it."
  (with-output-to-string (out)
    (format out "standing: ~a~%" *levels-map-text*)
    (format out "~&actor | level | passes | fails | last event~%")
    (loop for (name level passes fails updated) in (record-rows)
          for last = (first (record-events name))
          do (format out "~a | L~d | ~d | ~d | ~a~%"
                     name level passes fails
                     (if last (format nil "~a ~a ~a" (second last) (third last) (fourth last))
                         "no checks yet")))
    (format out "ledger: ~a" (namestring (ledger-path)))))

(defun log-text (actor)
  (let ((events (record-events actor)))
    (if events
        (with-output-to-string (out)
          (format out "standing: ~a's last ~d event~:p~%" actor (length events))
          (loop for (at name kind result reason) in events
                do (format out "~a | ~a | ~a | ~a~@[ | ~a~]~%"
                           at name kind result (and reason (plusp (length reason)) reason))))
        (format nil "standing: no events for ~a yet." actor))))

(defun run-slash (args session-id)
  "The operator's own word on the record: the table, one actor's log, or a
level set."
  (declare (ignore session-id))
  (let* ((parts (split-words (or args "")))
         (head (first parts)))
    (handler-case
        (cond
          ((null parts) (status-text))
          ((equal head "log")
           (log-text (or (second parts) (setting :actor))))
          ((equal head "set")
           (let* ((actor (or (second parts) (setting :actor)))
                  (level (third parts)))
             (if (null level)
                 "Use /standing set <actor> <L0-L3> [reason]."
                 (format nil "standing: ~a is now at L~d. ~a"
                         actor (set-actor-level actor (level-of level)
                                                (format nil "~{~a~^ ~}" (cdddr parts)))
                         *levels-map-text*))))
          (t "Use /standing, /standing log <actor>, or /standing set <actor> <L0-L3> [reason]."))
      (error (condition) (format nil "standing: the record could not be read or written: ~a"
                                (type-of condition))))))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell standing
  (:section ("standing")
    (:guide "actor is who this record is kept for; level is where a new actor starts; leave the rest unless you want proofs or a different gate")
    ("actor" :string :default "default"
     :doc "the name this record keeps this session's standing under")
    ("ledger" :path
     :doc "the file the record lives in; empty keeps it in the nodecode home")
    ("level" :choice :options '("L0" "L1" "L2" "L3") :default "L1"
     :doc "the level a new actor starts at")
    ("promote_after" :integer :default 10 :min 1
     :doc "how many passing checks in a row raise the level by one")
    ("max_auto_level" :choice :options '("L0" "L1" "L2" "L3") :default "L2"
     :doc "the highest level a passing check may raise it to; L3 is the operator's own word")
    ("workspace" :path
     :doc "the folder whose writes count as local; empty asks about every write")
    ("file_proof_roots" :list
     :doc "folders a file proof may read; empty uses the workspace, and with neither no file is read")
    ("http_proof_targets" :list
     :doc "URL prefixes an http proof may fetch, a trailing slash allowing a subtree; empty refuses every fetch")
    ("command_allowlist" :list
     :doc "exact read-only command lines a command proof may run, as written")
    ("proof_before" :list
     :doc "tool names that need a passing proof in this session before they run, as the model calls them; empty gates nothing")
    ("ask" :boolean :default t
     :doc "ask Allow once or Deny in the web page or the desktop app before refusing; with nobody there, or no answer, the call is refused")
    ("ask_seconds" :integer :default 120 :min 5
     :doc "how long a question waits for an answer")
    ("timeout" :integer :default 10 :min 1
     :doc "a proof's ceiling in seconds: one fetch or one command"))
  (:hook :tool #'gate)
  (:tool "standing_proof" (nlk:contract 'standing-proof) 'prove)
  (:command "standing" 'run-slash
            :description "Earned trust: the levels, the log, or a level set"
            :argument-hint "[log <actor> | set <actor> <L0-L3> [reason]]")
  (:help :standing "standing: earned trust by level, and a proof it runs itself" *manual*)
  (:start (lambda ()
            ;; The record exists from the first start, so the table and the log
            ;; answer before any check has run.
            (handler-case (progn (load-record) (when (probe-file (ledger-path)) t)
                                 (save-record (load-record)))
              (error () nil)))))
