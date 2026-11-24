;;;; guard.lisp --- the one place that decides whether recall may read a path.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every read asks this file first -- the indexer's walk, RECALL_OPEN and
;;;; the ambient hint -- and it asks about the RESOLVED path, so a symlink
;;;; named notes.txt pointing at an ssh key cannot launder a secret through an
;;;; innocent name. Three rules, in order:
;;;;
;;;;   1. the organism's own home is never read: its cells, its secrets, its
;;;;      store and this index live there, and a document assistant has no
;;;;      business quoting them into a prompt;
;;;;   2. virtual filesystems are never read: /proc/self/environ is a readable
;;;;      file whose contents are the process environment;
;;;;   3. the credential-store denylist applies to the resolved name and to
;;;;      the resolved parents.
;;;;
;;;; The rules are the cell's: a setting may add patterns, never remove one.

(in-package #:nodecode-recall)

(defparameter *secret-globs*
  '("*.env" ".env" ".env.*" "*.pem" "*.key" "*.p12" "*.pfx" "*.keystore"
    "id_rsa*" "id_ed25519*" "id_ecdsa*" "*.kdbx" "*.asc" "*.gpg"
    ".netrc" ".pgpass" ".git-credentials" "credentials" "credentials.json"
    "auth.json" "secrets.json" "*.secret" ".npmrc" ".pypirc" "state.db*"
    "key.txt" "keys.txt" "tokens.json" "api.json" "*apikey*" "*api*key*"
    "*recovery*code*" "*backup*code*" "*seed*phrase*" "*mnemonic*"
    "*keystore*" "*secret*" "*credential*" "*.ovpn" "*.ppk" "wallet*.dat"
    "*.wallet" "mcp-tokens" "mcp-tokens.json" "webhook_subscriptions.json"
    "dashboard_session*" "session_token*" "*.session-token" "*_key.json"
    "*_keys.json" "vault.json" ".htpasswd" "keepass*" "*.psafe3" "*.kdb")
  "File names recall never indexes nor opens, whatever the settings say.
The list can never be complete -- a password inside meeting-notes.txt is
still a document -- which is why rule 1 in the header is the one that holds.")

(defparameter *secret-dir-parts*
  '(".ssh" ".gnupg" ".aws" ".kube" ".password-store" ".azure" "mcp-tokens"
    ".docker" ".config")
  "A resolved path with one of these folders in it is a credential store.")

(defparameter *virtual-roots* '("/proc" "/sys" "/dev" "/run/user")
  "Filesystems that expose the process, not documents.")

(defparameter *skip-dirs*
  '(".git" ".hg" ".svn" "node_modules" "__pycache__" ".venv" "venv" "env"
    ".mypy_cache" ".pytest_cache" ".ruff_cache" ".tox" ".next" ".nuxt" "dist"
    "build" "target" ".gradle" ".idea" ".vscode" "site-packages" ".cache"
    ".Trash" "Trash" "$RECYCLE.BIN" "System Volume Information" "Library"
    "AppData" ".local")
  "Folders the walk never descends into, on top of the credential stores.")

(defun expand-path (path)
  "PATH -- a string or a pathname -- as a pathname with a leading ~ read
against the operator's home."
  (if (pathnamep path)
      path
      (handler-case (nlk:expand-home (string path))
        (error () (pathname (string path))))))

(defun resolve-path (path)
  "PATH with every symbolic link resolved, tolerating one that does not exist."
  (let ((expanded (expand-path path)))
    (or (ignore-errors (uiop:resolve-symlinks expanded)) expanded)))

(defun within-p (path root)
  "Whether PATH is ROOT or sits under it, judged component by component: the
prefix test would call /a/bc inside /a/b."
  (and path root
       (ignore-errors
        (uiop:subpathp (expand-path path)
                       (uiop:ensure-directory-pathname (expand-path root))))))

(defun organism-homes ()
  "The folders that are the organism's own: this home and ~/.nodecode."
  (remove nil
          (list (ignore-errors (uiop:ensure-directory-pathname (expand-path (nlk:home))))
                (ignore-errors (uiop:ensure-directory-pathname
                                (expand-path (merge-pathnames ".nodecode/"
                                                              (user-homedir-pathname))))))))

(defun glob-match-p (pattern text)
  "Whether TEXT matches PATTERN with * and ? wildcards, case-insensitively."
  (let ((pattern (string-downcase pattern))
        (text (string-downcase text)))
    (labels ((match (i j)
               (declare (type fixnum i j))
               (cond ((>= i (length pattern)) (>= j (length text)))
                     ((char= (char pattern i) #\*)
                      (or (match (1+ i) j)
                          (and (< j (length text)) (match i (1+ j)))))
                     ((>= j (length text)) nil)
                     ((or (char= (char pattern i) #\?)
                          (char= (char pattern i) (char text j)))
                      (match (1+ i) (1+ j)))
                     (t nil))))
      (match 0 0))))

(defun secret-name-p (path)
  "Whether PATH's file name matches the credential-store denylist."
  (let ((name (string-downcase (or (ignore-errors (file-namestring path)) ""))))
    (and (plusp (length name))
         (some (lambda (pattern) (glob-match-p pattern name)) *secret-globs*))))

(defun secret-dir-p (path)
  "Whether a folder of the resolved PATH is a credential store."
  (let ((parts (pathname-directory (resolve-path path))))
    (and parts
         (some (lambda (part)
                 (and (stringp part) (member (string-downcase part) *secret-dir-parts*
                                             :test #'equal)))
               parts))))

(defun virtual-p (path)
  "Whether the resolved PATH is on a virtual filesystem."
  (let ((text (string-downcase (or (ignore-errors (uiop:native-namestring path)) ""))))
    (some (lambda (root)
            (or (equal text root)
                (and (> (length text) (length root))
                     (uiop:string-prefix-p (concatenate 'string root "/") text))))
          *virtual-roots*)))

(defun guard-path (path &key directory)
  "Read PATH through the cell's policy => (values RESOLVED REASON); a NIL
REASON means the file may be read. DIRECTORY asks the folder question:
the name denylist is not applied to a folder, so secret-plans/ is walked and
its files are judged one by one."
  (let* ((given (expand-path path))
         (real (resolve-path given)))
    (flet ((refuse (why) (values real why)))
      (cond
        ((some (lambda (home) (within-p real home)) (organism-homes))
         (refuse "refused: inside Nodecode's own home, where its cells, secrets and store live"))
        ((virtual-p real)
         (refuse "refused: virtual filesystem -- /proc and /sys expose the process, not documents"))
        ((secret-dir-p real)
         (refuse "refused: a folder in the path is a credential store"))
        ((and (not directory) (or (secret-name-p real) (secret-name-p given)))
         (refuse "refused: path matches the credential-store denylist"))
        (t (values real ""))))))

(defun skip-dir-p (name)
  "Whether a folder called NAME is one the walk never enters."
  (let ((name (string-downcase name)))
    (or (member name *skip-dirs* :test #'equal)
        (uiop:string-prefix-p ".git" name)
        (member name *secret-dir-parts* :test #'equal))))

(defun user-excluded-p (path patterns)
  "Whether PATH matches one of the operator's own exclude patterns: a glob on
the name, a glob on the whole path, or a plain substring of it."
  (let ((text (string-downcase (or (ignore-errors (uiop:native-namestring path)) "")))
        (name (string-downcase (or (ignore-errors (file-namestring path)) ""))))
    (some (lambda (pattern)
            (let ((pattern (string-downcase pattern)))
              (or (glob-match-p pattern name)
                  (glob-match-p pattern text)
                  (and (not (find #\* pattern)) (not (find #\? pattern))
                       (search pattern text :test #'char=)))))
          patterns)))
