;;;; lineage.lisp --- the record: keyed hashes, operative anchors, the chain, the graph.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Nothing here keeps text. Every string a caller hands the cell becomes a
;;;; keyed hash before it touches the disk: a source's shingles and anchors,
;;;; and a skill version's content fingerprint. What a reader gets back is
;;;; the store's own vocabulary -- node ids, kinds, counts -- never a
;;;; sentence. The key lives beside the chain, so two installs cannot be
;;;; compared against each other; the chain itself is a line an entry, each
;;;; line hashed over the previous one.

(in-package #:nodecode-skill-lineage)

;;; --- where it keeps what it keeps ----------------------------------------------

(defparameter *data-dir* nil
  "The folder this run reads and writes; NIL is skill-lineage/ under the home.
A test binds it, so a run never touches the operator's chain.")

(defvar *key* nil
  "The install key of the folder *DATA-DIR* names, read once per folder.")

(defun data-dir ()
  (or *data-dir* (nlk:home "skill-lineage/")))

(defun data-file (name) (merge-pathnames name (data-dir)))
(defun chain-path () (data-file "chain.tsv"))
(defun key-path () (data-file "install.key"))
(defun counts-path () (data-file "anchors.json"))
(defun nodes-path () (data-file "nodes.txt"))

(defun digest (text)
  "TEXT's SHA-256 as lowercase hex, without the algorithm prefix."
  (subseq (nlk:sha256-text (or text "")) 7))

(defparameter +genesis+ (make-string 64 :initial-element #\0)
  "The prev of the first entry: no entry before it.")

;;; --- the key ---------------------------------------------------------------------

(defun new-key ()
  "32 random bytes as 64 hex characters."
  (format nil "~{~2,'0x~}" (loop repeat 32 collect (random 256))))

(defun write-key (path)
  (let ((text (new-key)))
    (nlk:write-file-atomically path (format nil "~a~%" text) :mode #o600 :directory-mode #o700)
    text))

(defun install-key ()
  "The folder's install key, made when there is none. A key file that is not
64 hex characters is replaced, and the replacement is recorded in the chain as
a key_reset entry: a reset is never silent."
  (or *key*
      (setf *key*
            (let ((path (key-path)))
              (ensure-directories-exist path)
              (when (probe-file path)
                (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                                          (nlk:read-text path))))
                  (if (= 64 (length text))
                      (return-from install-key text)
                      (progn
                        (ignore-errors (delete-file path))
                        (let ((fresh (write-key path)))
                          (append-entries
                           (list (nlk:json-object "kind" "key_reset" "at" (nlk:iso-now)
                                                   "reason" "the key file was not 64 hex characters")))
                          fresh)))))
              (write-key path)))))

(defun keyed (text &optional (size 32))
  "SIZE hex characters of a hash keyed by the install key: the same text in
two folders, or after a key reset, hashes differently, so two installs cannot
be compared."
  (subseq (digest (concatenate 'string (install-key) "|" (or text ""))) 0 size))

;;; --- text features: words, shingles, anchors -----------------------------------------

(defparameter +shingle-words+ 8
  "How many consecutive words one shingle spans: the unit a strong edge counts.")

(defun word-list (text)
  "TEXT's words, lowercased: runs of alphanumeric characters."
  (let ((words '()) (start nil) (text (or text "")))
    (loop for i from 0 below (length text)
          for c = (char text i)
          do (if (alphanumericp c)
                 (unless start (setf start i))
                 (when start (push (subseq text start i) words) (setf start nil))))
    (when start (push (subseq text start) words))
    (mapcar #'string-downcase (nreverse words))))

(defun shingles (text)
  "Every run of +SHINGLE-WORDS+ consecutive words, keyed and hashed."
  (let ((words (word-list text)))
    (when (>= (length words) +shingle-words+)
      (remove-duplicates
       (loop for i from 0 to (- (length words) +shingle-words+)
             collect (keyed (format nil "~{~a~^ ~}" (subseq words i (+ i +shingle-words+))) 16))
       :test #'equal))))

(defparameter +command-words+
  '("curl" "wget" "bash" "sh" "zsh" "python" "python3" "node" "npx" "uvx" "chmod" "chown"
    "rm" "ssh" "scp" "nc" "ncat" "base64" "eval" "exec" "sudo" "docker" "kubectl" "git"
    "powershell" "pwsh" "certutil" "crontab" "systemctl" "launchctl" "openssl" "gpg"
    "export" "source" "dd" "mv" "cp")
  "The words that make the next operand an operative token.")

(defparameter +install-words+
  '("pip" "pip3" "uv" "npm" "pnpm" "yarn" "gem" "cargo" "go" "brew" "apt" "apt-get"
    "choco" "winget")
  "The package managers whose installed package names are operative tokens.")

(defparameter +install-verbs+ '("install" "add" "get"))

(defparameter +plain-words+
  (let ((table (make-hash-table :test #'equal)))
    (dolist (word '("and" "or" "then" "to" "the" "a" "an" "with" "from" "before" "after" "so"
                    "in" "on" "of" "for" "if" "when" "that" "this" "it" "is" "be" "as" "at"
                    "by" "into" "your" "our" "their" "run" "use" "set"))
      (setf (gethash word table) t))
    table)
  "English words that make a following token no anchor.")

(defparameter +trim-chars+ ".,;:!?\"')]}>`"
  "What a token carries from the sentence around it, not from itself.")

(defun clean-token (token)
  (string-trim (list #\Space) (string-trim +trim-chars+ token)))

(defun url-p (token)
  (and (>= (length token) 11)
       (or (uiop:string-prefix-p "http://" token)
           (uiop:string-prefix-p "https://" token)
           (uiop:string-prefix-p "ftp://" token))))

(defun url-host (url)
  "URL's host, when it names one."
  (let* ((scheme (find-if (lambda (prefix) (uiop:string-prefix-p prefix url))
                           '("http://" "https://" "ftp://")))
         (rest (and scheme (subseq url (length scheme))))
         (host (and rest (subseq rest 0 (or (position #\/ rest) (length rest)))))
         (host (and host (car (last (uiop:split-string host :separator '(#\@))))))
         (host (and host (subseq host 0 (or (position #\: host) (length host))))))
    (and host (find #\. host) host)))

(defun digits-p (text &optional (from 0) (to (length text)))
  (and (< from to) (every #'digit-char-p (subseq text from to))))

(defun email-p (token)
  (let ((at (position #\@ token)))
    (and at (plusp at) (<= at 64)
         (not (find #\/ token))
         (find #\. token :start (1+ at))
         (every (lambda (c) (or (alphanumericp c) (find c "._+-"))) (subseq token 0 at)))))

(defun ip-p (token)
  (let ((parts (uiop:split-string token :separator '(#\.))))
    (and (= 4 (length parts))
         (every (lambda (part) (and (<= 1 (length part) 3) (digits-p part)
                                    (<= (parse-integer part) 255)))
                parts)
         (not (member token '("0.0.0.0" "127.0.0.1") :test #'equal)))))

(defun env-name (token)
  "The environment variable TOKEN names: $NAME, ${NAME} or NAME=value."
  (cond ((and (plusp (length token)) (char= #\$ (char token 0)))
         (let ((name (string-trim "{}" (subseq token 1))))
           (and (>= (length name) 3)
                (every (lambda (c) (or (char<= #\A c #\Z) (char<= #\0 c #\9) (char= c #\_))) name)
                name)))
        ((position #\= token)
         (let ((name (subseq token 0 (position #\= token))))
           (and (>= (length name) 4)
                (every (lambda (c) (or (char<= #\A c #\Z) (char<= #\0 c #\9) (char= c #\_))) name)
                name)))))

(defun path-p (token)
  (and (> (length token) 4)
       (not (url-p token))
       (not (uiop:string-prefix-p "//" token))
       (or (and (find #\/ token) (>= (count #\/ token) 2))
           (uiop:string-prefix-p "~/" token))))

(defun operative-arg-p (arg)
  "Whether ARG is worth anchoring: not a plain English word, and carrying a
slash, a dot, a variable sign or a digit."
  (and (plusp (length arg))
       (not (gethash (string-downcase arg) +plain-words+))
       (or (find-if (lambda (c) (find c "/.:$~=@\\")) arg)
           (find-if #'digit-char-p arg))))

(defun package-p (token)
  (and (plusp (length token))
       (alphanumericp (char token 0))
       (every (lambda (c) (or (alphanumericp c) (find c "._@/-[]=<>!~+"))) token)))

(defun url-anchor-strings (text)
  "Every URL in TEXT, normalised as an anchor, and each host with it."
  (let ((found '()))
    (dolist (chunk (nlk:split-words (or text "")))
      (let ((token (string-downcase (clean-token chunk))))
        (when (url-p token)
          (pushnew (concatenate 'string "url:" token) found :test #'equal)
          (let ((host (url-host token)))
            (when (and host (find #\. host))
              (pushnew (concatenate 'string "host:" host) found :test #'equal))))))
    found))

(defun anchor-strings (text)
  "The operative tokens TEXT carries, normalised: a URL and its host, an
e-mail address, an IP, an environment variable, a file path, a shell command
with its first operand, a package install. Plain text, hashed by the caller
at once."
  (let ((chunks (mapcar #'clean-token (nlk:split-words (or text ""))))
        (found '()))
    (flet ((note (value) (pushnew value found :test #'equal)))
      (dolist (chunk chunks)
        (let ((low (string-downcase chunk)))
          (cond ((url-p low)
                 (note (concatenate 'string "url:" low))
                 (let ((host (url-host low)))
                   (when host (note (concatenate 'string "host:" host)))))
                ((email-p low) (note (concatenate 'string "email:" low)))
                ((ip-p low) (note (concatenate 'string "ip:" low)))
                ((env-name chunk) (note (concatenate 'string "env:" (env-name chunk))))
                ((path-p chunk) (note (concatenate 'string "path:" chunk))))))
      ;; a command with the one operand that changes what it does
      (loop for rest on chunks
            for head = (string-downcase (first rest))
            when (member head +command-words+ :test #'equal)
              do (let ((arg (find-if #'operative-arg-p (subseq (rest rest) 0 (min 6 (length (rest rest)))))))
                   (when arg
                     (note (format nil "cmd:~a ~a" head (string-downcase arg))))))
      ;; a package install
      (loop for rest on chunks
            for head = (string-downcase (first rest))
            for verb = (string-downcase (or (second rest) ""))
            when (and (member head +install-words+ :test #'equal)
                      (member verb +install-verbs+ :test #'equal))
              do (loop for package in (subseq (cddr rest) 0 (min 4 (length (cddr rest))))
                       while (package-p package)
                       do (note (format nil "pkg:~a ~a" head (string-downcase package))))))
    (sort found #'string<)))

(defun anchors (text)
  "TEXT's operative anchors, keyed and hashed."
  (mapcar (lambda (value) (keyed value 32)) (anchor-strings text)))

(defun url-hash (url)
  "The keyed hash a URL is looked up by, normalised as anchor-strings does."
  (keyed (concatenate 'string "url:" (string-downcase (clean-token url))) 32))

(defun added-text (before after)
  "The lines of AFTER that BEFORE did not carry: what this write added."
  (let ((after (or after "")))
    (if (or (null before) (zerop (length before)))
        after
        (let ((known (make-hash-table :test #'equal)))
          (dolist (line (nlk:lines before)) (setf (gethash (string-trim " " line) known) t))
          (format nil "~{~a~%~}"
                  (remove-if (lambda (line) (gethash (string-trim " " line) known))
                             (nlk:lines after)))))))

;;; --- the chain ------------------------------------------------------------------

(defstruct (entry (:constructor make-entry (seq prev hash body)))
  "One chain line: SEQ number, PREV hash, HASH over (seq, prev, body), BODY a
decoded JSON object."
  seq prev hash body)

(defun entry-kind (entry) (nlk:json-value (entry-body entry) :string "kind"))

(defun chain-line (seq prev hash json)
  (format nil "~d~c~a~c~a~c~a" seq #\Tab prev #\Tab hash #\Tab json))

(defun split-line (line)
  "SEQ, PREV, HASH and BODY text of one chain line, or NIL when it is not one."
  (let* ((a (position #\Tab line))
         (b (and a (position #\Tab line :start (1+ a))))
         (c (and b (position #\Tab line :start (1+ b)))))
    (when c
      (let ((seq (ignore-errors (parse-integer line :end a :junk-allowed t))))
        (when (and (integerp seq) (plusp seq))
          (values seq (subseq line (1+ a) b) (subseq line (1+ b) c) (subseq line (1+ c))))))))

(defun read-entries (&optional (path (chain-path)))
  "Every line that parses, in file order; a torn trailing line is skipped by
the reader and reported by the verifier."
  (let ((entries '()))
    (when (probe-file path)
      (dolist (line (nlk:lines (nlk:read-text path)))
        (when (plusp (length (string-trim " " line)))
          (multiple-value-bind (seq prev hash body) (split-line line)
            (when (and seq hash body)
              (let ((decoded (ignore-errors (nlk:decode-json body))))
                (when (hash-table-p decoded)
                  (push (make-entry seq prev hash decoded) entries))))))))
    (nreverse entries)))

(defun ends-with-newline-p (path)
  "Whether PATH's last byte ends a line: a missing or empty file does."
  (if (probe-file path)
      (let ((text (nlk:read-text path)))
        (or (zerop (length text)) (char= #\Newline (char text (1- (length text))))))
      t))

(defun chain-head ()
  "SEQ and HASH of the newest entry, or (values 0 +GENESIS+)."
  (let ((entries (read-entries)))
    (if entries
        (let ((last (car (last entries))))
          (values (entry-seq last) (entry-hash last)))
        (values 0 +genesis+))))

(defun append-entries (bodies)
  "Append one line per BODY (a JSON object) after the current head => the
entries written. The write holds the cell's lock, so two sessions recording
at once cannot interleave two lines."
  (with-skill-lineage-lock
    (multiple-value-bind (seq prev) (chain-head)
      (let ((lines '()) (written '()))
        (dolist (body bodies)
          (let* ((json (nlk:encode-json-object body))
                 (number (1+ seq))
                 (hash (digest (format nil "~d~c~a~c~a" number #\Tab prev #\Tab json))))
            (push (format nil "~a~%" (chain-line number prev hash json)) lines)
            (push (make-entry number prev hash body) written)
            (setf seq number prev hash)))
        (let ((path (chain-path)))
          (ensure-directories-exist path)
          (with-open-file (out path :direction :output :if-exists :append
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
            ;; A torn trailing line has no terminator of its own: without this
            ;; the next entry would be glued onto it and be unreadable too.
            (unless (ends-with-newline-p path) (write-char #\Newline out))
            (dolist (line (nreverse lines)) (write-string line out))))
        (nreverse written)))))

(defun chain-hashes (&optional (path (chain-path)))
  "Every line's own hash, in file order: what a head recorded elsewhere is
looked for in."
  (let ((hashes '()))
    (when (probe-file path)
      (dolist (line (nlk:lines (nlk:read-text path)))
        (when (plusp (length (string-trim " " line)))
          (multiple-value-bind (seq prev hash) (split-line line)
            (declare (ignore seq prev))
            (when hash (push hash hashes))))))
    (nreverse hashes)))

(defun chain-verify (&key head)
  "Re-walk the chain: every line parses, seq is contiguous from 1, prev links,
and each hash recomputes. With HEAD, the hash recorded elsewhere must be in
it -- the one check that tells truncation from an intact chain. => a plist."
  (let ((path (chain-path)))
    (if (not (probe-file path))
        (list :ok nil :entries 0 :head-seq 0 :head-hash +genesis+
              :bad-seq nil :reason "chain file not found")
        (let ((prev +genesis+) (count 0) (prev-entry nil) (bad nil) (reason ""))
          (dolist (line (nlk:lines (nlk:read-text path)))
            (when (and (null bad) (plusp (length (string-trim " " line))))
              (incf count)
              (multiple-value-bind (seq prev-in hash body) (split-line line)
                (let ((decoded (and body (ignore-errors (nlk:decode-json body)))))
                  (cond ((or (null seq) (null decoded))
                         (setf bad count reason "line does not parse as a chain entry"))
                        ((/= seq count)
                         (setf bad count reason (format nil "seq is ~a, expected ~d" seq count)))
                        ((not (equal prev-in prev))
                         (setf bad count reason "prev does not match the previous hash"))
                        ((not (equal hash (digest (format nil "~d~c~a~c~a" seq #\Tab prev #\Tab body))))
                         (setf bad count reason "hash does not match the entry content"))
                        (t (setf prev hash prev-entry (list seq hash))))))))
          (cond (bad (list :ok nil :entries (1- count) :head-seq (1- count) :head-hash prev
                           :bad-seq bad :reason reason))
                ((and head (plusp (length (string-trim " " head)))
                      (not (member (string-trim " " head) (chain-hashes path) :test #'equal)))
                 (list :ok nil :entries count :head-seq (max 0 (1- count)) :head-hash prev
                       :bad-seq nil :reason "the given head hash is not in this chain (truncated or rewritten)"))
                (t (list :ok t :entries (if prev-entry count 0)
                         :head-seq (if prev-entry (first prev-entry) 0)
                         :head-hash prev :bad-seq nil :reason "")))))))

;;; --- the graph ------------------------------------------------------------------

(defstruct (graph (:constructor make-graph))
  (versions (make-hash-table :test #'equal))
  (latest (make-hash-table :test #'equal))
  (sources (make-hash-table :test #'equal))
  (parents (make-hash-table :test #'equal))
  (children (make-hash-table :test #'equal))
  (by-url (make-hash-table :test #'equal))
  (by-mention (make-hash-table :test #'equal)))

(defun version-node (skill fingerprint) (format nil "skill:~a@~a" skill fingerprint))

(defun load-graph ()
  "The chain as a graph: a source node per recorded source, a version node per
skill write, an edge from a source to the version it reached."
  (let ((g (make-graph)))
    (dolist (entry (read-entries))
      (let ((body (entry-body entry)))
        (cond
          ((equal (entry-kind entry) "source")
           (let ((node (nlk:json-value body :string "node")))
             (when node
               (setf (gethash node (graph-sources g)) body)
               (loop for url across (or (nlk:json-value body :array "urls") #())
                     do (pushnew node (gethash url (graph-by-url g) '()) :test #'equal))
               (loop for url across (or (nlk:json-value body :array "mentions") #())
                     do (pushnew node (gethash url (graph-by-mention g) '()) :test #'equal)))))
          ((equal (entry-kind entry) "version")
           (let ((skill (nlk:json-value body :string "skill"))
                 (after (nlk:json-value body :string "after")))
             (cond ((null after) (remhash skill (graph-latest g)))
                   ((null skill))
                   (t (let ((node (version-node skill after)))
                        (setf (gethash node (graph-versions g)) body
                              (gethash skill (graph-latest g)) node)
                        (loop for edge across (or (nlk:json-value body :array "edges") #())
                              for from = (nlk:json-value edge :string "from")
                              when from
                                do (push (cons from edge) (gethash node (graph-parents g) '()))
                                   (push (cons node edge) (gethash from (graph-children g) '())))))))))))
    g))

(defparameter +default-kinds+ '("strong" "anchor" "prev" "read-strong" "read-anchor" "merge")
  "The edge kinds a walk follows: what measurably links a source to a write.")
(defparameter +weak-kinds+ '("context" "read-context")
  "The edge kinds a walk follows only when asked: a source the session saw,
with nothing measurable shared.")

(defun edge-kind (edge) (nlk:json-value edge :string "k"))

(defun walk (g start forward kinds)
  "Every node reachable from START in one direction, each with the node it was
reached from and the edge used."
  (let ((reached '()) (seen (copy-list start)) (queue (copy-list start)))
    (loop while queue
          do (let* ((node (pop queue))
                    (links (gethash node (if forward (graph-children g) (graph-parents g)))))
               (dolist (link links)
                 (let ((other (car link)) (edge (cdr link)))
                   (when (and (member (edge-kind edge) kinds :test #'equal)
                              (not (member other seen :test #'equal)))
                     (push other seen)
                     (push (list other node edge) reached)
                     (push other queue))))))
    (nreverse reached)))

(defun start-nodes (g target url mentions)
  "A node id, every recorded version of a skill name, or the sources that
fetched a URL -- plus, with MENTIONS, the sources that only name it."
  (cond ((or (gethash target (graph-sources g)) (gethash target (graph-versions g)))
         (list target))
        ((and url (or (gethash url (graph-by-url g))
                      (and mentions (gethash url (graph-by-mention g)))))
         (append (copy-list (gethash url (graph-by-url g)))
                 (if mentions (copy-list (gethash url (graph-by-mention g))))))
        (t (let ((nodes '()))
             (maphash (lambda (node body)
                        (when (equal (nlk:json-value body :string "skill") target)
                          (push node nodes)))
                      (graph-versions g))
             nodes))))

(defun kinds-for (weak)
  (if weak (append +default-kinds+ +weak-kinds+) +default-kinds+))

(defun short-node (node)
  (if (> (length node) 28) (concatenate 'string (subseq node 0 28) "…") node))


(defun node-label (g node)
  "NODE as a reader sees it: a source's class, or a skill version's name and
fingerprint."
  (let ((source (gethash node (graph-sources g)))
        (version (gethash node (graph-versions g))))
    (cond (source
           (format nil "~a  [~a]" (short-node node)
                   (or (nlk:json-value source :string "cls") "source")))
          (version
           (format nil "skill ~a @~a"
                   (nlk:json-value version :string "skill")
                   (subseq (or (nlk:json-value version :string "after") "?") 0 10)))
          (t (short-node node)))))

(defun edge-text (edge)
  "EDGE as one reader's phrase: its kind, shingles and anchors."
  (let ((parts (list (or (edge-kind edge) "?")))
        (sh (nlk:json-value edge :integer "sh"))
        (an (nlk:json-value edge :integer "an")))
    (when (and sh (plusp sh)) (push (format nil "~d shingles" sh) parts))
    (when (and an (plusp an)) (push (format nil "~d anchors" an) parts))
    (format nil "~{~a~^, ~}" (nreverse parts))))

;;; --- the verbs --------------------------------------------------------------------

(defparameter +skill-name-chars+ "abcdefghijklmnopqrstuvwxyz0123456789-_."
  "What a skill name may be spelled with.")

(defun check-skill-name (skill)
  (let ((name (string-downcase (string-trim " " (princ-to-string (or skill ""))))))
    (unless (and (<= 1 (length name) 64)
                 (every (lambda (c) (find c +skill-name-chars+)) name)
                 (alphanumericp (char name 0)))
      (fail "~s is no skill name: lowercase letters, digits, - _ and ., at most 64, a letter or digit first"
            skill))
    name))

(defun anchor-state ()
  (let ((path (counts-path)))
    (if (probe-file path)
        (let ((data (ignore-errors (nlk:decode-json (nlk:read-text path)))))
          (if (hash-table-p data) data (nlk:make-json-object "sources" 0 "counts" (make-hash-table :test #'equal))))
        (nlk:make-json-object "sources" 0 "counts" (make-hash-table :test #'equal)))))

(defun common-threshold (sources)
  "How many distinct sources must carry an anchor before it is boilerplate."
  (max 5 (ceiling (* 0.05 sources))))

(defun common-anchors ()
  "The anchors seen in enough distinct sources to be boilerplate: they never
make an edge, since a page sharing them says nothing about where the text came
from."
  (let* ((data (anchor-state))
         (sources (or (nlk:json-value data :integer "sources") 0))
         (counts (nlk:json-value data :object "counts"))
         (limit (common-threshold sources))
         (common '()))
    (when counts
      (maphash (lambda (hash n) (when (>= n limit) (push hash common))) counts))
    common))

(defun note-anchors (anchor-list)
  "Count one more source for each of ANCHOR-LIST."
  (let* ((data (anchor-state))
         (counts (or (nlk:json-value data :object "counts")
                     (setf (gethash "counts" data) (make-hash-table :test #'equal)))))
    (setf (gethash "sources" data) (1+ (or (nlk:json-value data :integer "sources") 0)))
    (dolist (hash anchor-list) (incf (gethash hash counts 0)))
    (nlk:write-file-atomically (counts-path) (nlk:encode-json-object data)
                               :mode #o600 :directory-mode #o700)))

(defun known-nodes ()
  (let ((table (make-hash-table :test #'equal)))
    (when (probe-file (nodes-path))
      (dolist (line (nlk:lines (nlk:read-text (nodes-path))))
        (setf (gethash (string-trim " " line) table) t)))
    table))

(defun note-nodes (nodes)
  (let ((path (nodes-path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :append :if-does-not-exist :create
                              :external-format :utf-8)
      (dolist (node nodes) (write-line node out)))))

(defun source-urls (text)
  "The keyed hashes of the URLs TEXT names: the sources that fetched them."
  (mapcar (lambda (value) (keyed value 32))
          (remove-if-not (lambda (value) (uiop:string-prefix-p "url:" value))
                         (anchor-strings text))))

(defun make-seen (asked sources)
  "The sources one record call names: ASKED, the operator's words, and SOURCES,
the texts it read. => one (NODE CLASS TEXT SHINGLES ANCHORS) per source, once."
  (let ((seen '()))
    (labels ((add (cls text)
               (when (and (stringp text) (plusp (length (string-trim " " text))))
                 (let ((node (format nil "src:~a:~a" cls (keyed text 16))))
                   (unless (assoc node seen :test #'equal)
                     (push (list node cls text (shingles text) (anchors text)) seen))))))
      (add "user" asked)
      (dolist (source (if (listp sources) sources (list sources)))
        (add "ext" source)))
    (nreverse seen)))

(defun compute-edges (added seen common)
  "The edges from SEEN (node, class, shingles, anchors) into the text ADDED,
and how many of ADDED's operative tokens no source explains. A shingle or an
anchor common across recorded sources is boilerplate and never makes an edge;
two sources of one class sharing a token cannot say which one it came from, so
only overlap unique to one of them counts."
  (let* ((added-shingles (shingles added))
         (added-anchors (remove-if (lambda (hash) (member hash common :test #'equal)) (anchors added)))
         (explained (make-hash-table :test #'equal))
         (saw-external (find-if (lambda (item) (equal (second item) "ext")) seen))
         (edges '()))
    (dolist (item seen)
      (destructuring-bind (node cls text own-shingles own-anchors) item
        (declare (ignore text))
        (dolist (hash own-anchors) (setf (gethash hash explained) t))
        (let* ((peers (remove-if-not (lambda (other) (and (not (eq other item)) (equal (second other) cls)))
                                      seen))
               (peer-shingles (loop for peer in peers append (third peer)))
               (peer-anchors (loop for peer in peers append (fourth peer)))
               (unique-shingles (set-difference own-shingles peer-shingles :test #'equal))
               (unique-anchors (set-difference own-anchors peer-anchors :test #'equal))
               (shared-shingles (length (intersection added-shingles unique-shingles :test #'equal)))
               (shared-anchors (length (intersection added-anchors unique-anchors :test #'equal))))
          (cond ((>= shared-shingles (setting :strong-min))
                 (push (list node "strong" shared-shingles shared-anchors) edges))
                ((>= shared-anchors 1)
                 (push (list node "anchor" shared-shingles shared-anchors) edges))
                (t (push (list node "context" 0 0) edges))))))
    (values (nreverse edges)
            (if saw-external
                (count-if (lambda (hash) (not (gethash hash explained))) added-anchors)
                0))))

(defun edge-rows (edges)
  "EDGE rows for the chain: context edges past the section's budget are
counted, not listed. => rows, how many were dropped."
  (let ((rows '()) (dropped 0) (context 0) (limit (setting :context-edges)))
    (dolist (edge edges)
      (destructuring-bind (node kind sh an) edge
        (cond ((equal kind "context")
               (if (>= context limit)
                   (incf dropped)
                   (progn (incf context)
                          (push (nlk:json-object "k" kind "from" node) rows))))
              (t (push (nlk:json-object "k" kind "from" node "sh" sh "an" an) rows)))))
    (values (coerce (nreverse rows) 'vector) dropped)))

(defun latest-fingerprint (skill)
  "The content fingerprint of SKILL's newest recorded version, or NIL."
  (let ((fingerprint nil))
    (dolist (entry (read-entries))
      (when (equal (entry-kind entry) "version")
        (let ((body (entry-body entry)))
          (when (equal (nlk:json-value body :string "skill") skill)
            (setf fingerprint (nlk:json-value body :string "after"))))))
    fingerprint))

(define-verb record (skill &key after before sources asked)
  "Record one skill version: SKILL its name, AFTER its body as it now reads,
BEFORE the body it had (empty for a new skill), SOURCES the texts it drew on
(a page, a file, a tool result), ASKED what the operator typed this session.
Only keyed hashes are kept. => one line saying what was written."
  (let* ((name (check-skill-name skill))
         (after (or after ""))
         (before (or before "")))
    (unless (plusp (length (string-trim " " after)))
      (fail "a version needs its body: pass the skill's text after the write"))
    (let ((after-fp (keyed after 16))
          (before-fp (and (plusp (length (string-trim " " before))) (keyed before 16))))
      (cond
        ((equal after-fp (latest-fingerprint name))
         (format nil "skill ~a @~a is the version already recorded: nothing written"
                 name (subseq after-fp 0 10)))
        (t
         (let* ((seen (make-seen asked sources))
                (added (added-text before after)))
           (multiple-value-bind (edges orphans) (compute-edges added seen (common-anchors))
             (multiple-value-bind (rows dropped) (edge-rows edges)
               (let ((known (known-nodes)) (fresh '()) (bodies '()))
                 (dolist (item seen)
                   (destructuring-bind (node cls text sh an) item
                     (declare (ignore sh an))
                     (unless (gethash node known)
                       (setf (gethash node known) t)
                       (push node fresh)
                       (push (nlk:json-object "kind" "source" "node" node "cls" cls
                                               "urls" (coerce (source-urls text) 'vector)
                                               "mentions" #())
                             bodies))))
                 (push (nlk:json-object "kind" "version" "skill" name
                                         "before" (or before-fp "") "after" after-fp
                                         "edges" rows "orphans" orphans
                                         "not_listed" dropped
                                         "at" (nlk:iso-now))
                       bodies)
                 (append-entries (nreverse bodies))
                 (when fresh
                   (let ((fresh-nodes (nreverse fresh)))
                     (note-nodes fresh-nodes)
                     (dolist (item seen)
                       (when (member (first item) fresh-nodes :test #'equal)
                         (note-anchors (fourth item))))))
                 (format nil "recorded skill ~a @~a: ~d edge~:p~@[, ~d orphan~:p~]~@[, ~d context edge~:p not listed~]~%~{~a~^~%~}"
                         name (subseq after-fp 0 10) (length edges)
                         (and (plusp orphans) orphans)
                         (and (plusp dropped) dropped)
                         (mapcar (lambda (edge)
                                   (format nil "  ~a: ~a (~d shingles, ~d anchors)"
                                           (second edge) (short-node (first edge))
                                           (third edge) (fourth edge)))
                                 edges)))))))))))

(define-verb trace (skill &key weak)
  "Where the newest recorded version of SKILL came from: the sources whose
content reached it. WEAK follows context edges too -- sources the session
saw that share nothing measurable."
  (let* ((g (load-graph))
         (name (string-downcase (string-trim " " (princ-to-string (or skill "")))))
         (node (gethash name (graph-latest g))))
    (unless node
      (fail "no recorded live version of skill ~a: (lineage:status) shows what is recorded" name))
    (let ((lines (list (node-label g node)))
          (reached (walk g (list node) nil (kinds-for weak))))
      (if (null reached)
          (push "  (no recorded origin)" lines)
          (dolist (item reached)
            (destructuring-bind (other via edge) item
              (declare (ignore via))
              (push (format nil "  <- ~a   [~a]" (node-label g other) (edge-text edge)) lines))))
      (format nil "~{~a~^~%~}" (nreverse lines)))))

(define-verb descendants (target &key weak mentions)
  "Every skill version that came from TARGET: a source node id, a URL the agent
read, or a skill name. WEAK follows context edges; MENTIONS also starts from
sources that only name the URL."
  (let* ((g (load-graph))
         (text (string-trim " " (princ-to-string (or target ""))))
         (lower (string-downcase text))
         (url (and (or (uiop:string-prefix-p "http://" lower) (uiop:string-prefix-p "https://" lower))
                   (url-hash text)))
         (start (start-nodes g text url mentions)))
    (unless start
      (fail "nothing recorded for ~a" text))
    (let ((live (make-hash-table :test #'equal))
          (lines '()))
      (maphash (lambda (skill node) (declare (ignore skill)) (setf (gethash node live) t))
               (graph-latest g))
      (dolist (node start)
        (when (gethash node (graph-versions g))
          (push (format nil "~a~a" (if (gethash node live) "LIVE " "     ")
                        (node-label g node))
                lines)))
      (dolist (item (walk g start t (kinds-for weak)))
        (destructuring-bind (other via edge) item
          (declare (ignore via))
          (when (gethash other (graph-versions g))
            (push (format nil "~a~a   [~a]"
                          (if (gethash other live) "LIVE " "     ")
                          (node-label g other) (edge-text edge))
                  lines))))
      (if lines
          (format nil "~{~a~^~%~}" (nreverse lines))
          (format nil "no skill versions reached~a" (if weak "" " (try :weak t)"))))))

(define-verb report ()
  "Skill versions carrying operative tokens -- a URL, a command, a path -- that
no recorded source explains."
  (let ((flagged '()))
    (let ((g (load-graph)))
      (maphash (lambda (node body)
                 (let ((orphans (nlk:json-value body :integer "orphans")))
                   (when (and orphans (plusp orphans)) (push (cons node body) flagged))))
               (graph-versions g))
      (if (null flagged)
          "no skill versions with untraceable operative tokens"
          (format nil "~{~a~^~%~}"
                  (mapcar (lambda (pair)
                            (format nil "~a  ~d operative token(s) with no traceable origin (the session saw external content) - try: (lineage:trace ~s :weak t)"
                                    (node-label g (car pair))
                                    (nlk:json-value (cdr pair) :integer "orphans")
                                    (nlk:json-value (cdr pair) :string "skill")))
                          (sort flagged #'string< :key #'car)))))))

(define-verb verify (&key head)
  "Re-walk the chain: every line parses, seq is contiguous, prev links and each
hash recomputes. HEAD, a hash recorded elsewhere, tells truncation from an
intact chain."
  (let ((result (chain-verify :head head)))
    (cond
      ((getf result :bad-seq)
       (format nil "BROKEN at seq ~a: ~a~%entries verified before the break: ~a"
               (getf result :bad-seq) (getf result :reason) (getf result :entries)))
      ((not (getf result :ok))
       (format nil "BROKEN: ~a (~a entries otherwise intact)"
               (getf result :reason) (getf result :entries)))
      ((zerop (getf result :entries))
       (format nil "EMPTY: 0 entries, nothing to verify~%note: an empty chain cannot be told apart from one truncated to zero bytes; pass the head hash you recorded elsewhere to tell them apart"))
      (t
       (format nil "INTACT: ~a entries, head seq ~a hash ~a~@[~%given head found; ~a entries follow it~]"
               (getf result :entries) (getf result :head-seq) (getf result :head-hash)
               (and head (getf result :entries)))))))

(define-verb head ()
  "The newest entry's seq and hash: record it somewhere off this machine, so a
later verify can tell a truncated chain from an intact one."
  (multiple-value-bind (seq hash) (chain-head)
    (format nil "seq ~a hash ~a" seq hash)))

(define-verb status ()
  "Is anything being recorded? The folder, the chain's size and its head."
  (let* ((entries (read-entries))
         (versions (count-if (lambda (entry) (equal (entry-kind entry) "version")) entries))
         (sources (count-if (lambda (entry) (equal (entry-kind entry) "source")) entries))
         (resets (count-if (lambda (entry) (equal (entry-kind entry) "key_reset")) entries))
         (last (car (last entries))))
    (format nil "skill-lineage in ~a~%chain: ~:[empty - nothing recorded yet~;~:*~d entries, head seq ~d, ~d skill version~:p, ~d source~:p~]~@[~%WARNING: the hashing key was reset ~d time(s): sources seen before a reset cannot link to writes after it~]"
            (namestring (data-dir))
            (if entries (length entries) nil)
            (if last (entry-seq last) 0)
            versions sources
            (and (plusp resets) resets))))
