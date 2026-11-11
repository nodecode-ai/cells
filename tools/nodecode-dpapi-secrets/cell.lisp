;;;; cell.lisp --- the primer, the six tools, /dpapi-secrets, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes secret source: at startup the orchestrator
;;;; called fetch(), which decrypted one DPAPI-secret file and handed the
;;;; mapped values back as environment variables, so provider keys never sat
;;;; in plaintext in .env or auth.json. Nodecode has no secret-source seam, so
;;;; the same store is read where a session needs it -- in the conversation:
;;;; six tools, one command and a manual topic -- and a form can read one
;;;; value without the transcript through (SECRET "name").
;;;;
;;;; Nothing is cached and nothing is written into the environment: the store
;;;; is read when a tool asks for it, and only /dpapi-secrets set (or the
;;;; dpapi_set tool) writes it.

(in-package #:nodecode-dpapi-secrets)

(defparameter +primer+
  "A DPAPI secret store is reachable through the nodecode-dpapi-secrets cell: one file holding a named
value per entry, encrypted at rest with the Windows Data Protection API under the current Windows
user, so it is readable by that user on that machine and by nobody else. Nothing is sent anywhere,
nothing is cached, and the store is read when a tool asks for it.
  dpapi_list    the entry names, and which environment variable each is mapped to -- no value.
  dpapi_show    one entry's value, by its name inside the store.
  dpapi_env     the mapping the original plugin injected at startup, as NAME=value lines: every
                `map` line of the section, resolved, with a warning for each entry the store
                cannot answer. A plain read of every value, so prefer dpapi_show for one key.
  dpapi_status  where the store is, how it is protected on this platform, how many entries it
                holds, and how the mapping stands. Reads no value.
  dpapi_set     write one entry: its name inside the store, and its value.
  dpapi_unset   remove one entry.
The store is one file, `file` in the dpapi-secrets section. On Windows it is a DPAPI blob and only
the account that wrote it can read it; on a platform without DPAPI it is the same map under a header
of its own, a 0600 file, and dpapi_status says so. `/dpapi-secrets` types the verbs: list, show,
set NAME (the value is asked), unset NAME, env, path, status.
How to work: a value read here is as sensitive as the key it is. Put it where the operator asked
for it, and do not echo it into a channel, a commit or a file they did not name. The `map` line a
variable is named in is what a provider lane's api_key_env reads, so a key stored once serves every
lane that names it. (secret \"name\") reads the same entry in this image without a tool call.
ERROR: DPAPI-SECRETS-ERROR names the problem: no store yet, no entry of that name, a file this
account cannot decrypt, or DPAPI missing on this platform."
  "What (help :dpapi-secrets) answers while the cell runs.")

;;; --- small words -------------------------------------------------------------

(defun trim (text)
  "TEXT without the blank at either end, or \"\" for NIL."
  (if (stringp text) (nlk:trimmed text) ""))

(defun clip (text)
  "TEXT, cut at `max_chars' with a note saying where."
  (let ((limit (setting :max-chars)))
    (if (> (length text) limit)
        (format nil "~a~%[cut at ~d of ~d characters]" (subseq text 0 limit) limit (length text))
        text)))

(defun words (text)
  "TEXT split on runs of whitespace, in order, with no shell in between."
  (let ((words '())
        (start nil))
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (if (member character '(#\Space #\Tab #\Newline #\Return))
                 (when start (push (subseq text start index) words) (setf start nil))
                 (unless start (setf start index))))
    (when start (push (subseq text start) words))
    (nreverse words)))

(defun env-char-p (character)
  "Whether CHARACTER may stand in an environment variable's name: ASCII only,
 as the original's ^[A-Za-z_][A-Za-z0-9_]*$ said."
  (or (char<= #\A character #\Z) (char<= #\a character #\z)
      (char<= #\0 character #\9) (char= character #\_)))

(defun valid-env-name-p (text)
  "Whether TEXT could be an environment variable's name."
  (and (stringp text)
       (plusp (length text))
       (let ((first (char text 0)))
         (and (or (char<= #\A first #\Z) (char<= #\a first #\z) (char= first #\_))
              (every #'env-char-p text)))))

(defun clean-name (text)
  "TEXT as a store entry's name: trimmed, with the original's dpapi:// prefix
 taken off, or NIL when nothing is left."
  (let ((name (trim text)))
    (when (uiop:string-prefix-p "dpapi://" (string-downcase name))
      (setf name (trim (subseq name 8))))
    (and (plusp (length name)) name)))

;;; --- the boundaries, declared before their readers ------------------------------

(nlk:define-contract entry
  (:doc "One entry of the DPAPI secret store, by its name inside the store.")
  ("name" :text :required t
          :doc "the entry's name inside the store, such as openai; it is not the value"))

(nlk:define-contract entry-write
  (:doc "An entry to write into the DPAPI secret store: its name inside the store, and the value it holds. The value is in this call, so it is in the transcript of it.")
  ("name" :text :required t
          :doc "the entry's name inside the store, such as openai")
  ("value" :text :required t
           :doc "the secret itself; the store answers with it and keeps it encrypted at rest"))

;;; --- the section read as the store it names -----------------------------------

(defun store-path ()
  "The store's pathname: the section's `file', relative to this home."
  (let ((text (trim (setting :file))))
    (cond
      ((zerop (length text)) (nlk:home "secrets.dpapi"))
      ((uiop:string-prefix-p "~/" text)
       (merge-pathnames (subseq text 2) (user-homedir-pathname)))
      ((uiop:absolute-pathname-p (uiop:parse-native-namestring text))
       (uiop:parse-native-namestring text))
      (t (nlk:home text)))))

(defun protection ()
  "How a write protects the store here: :DPAPI on Windows, else :FILE. A
 forced `protection` this platform cannot honour refuses, naming the way out."
  (let ((chosen (setting :protection)))
    (cond
      ((equal chosen "dpapi")
       (unless (dpapi-available-p)
         (fail "`protection` is dpapi, and Windows DPAPI is only available on Windows; set it to auto, or to file, in the dpapi-secrets section"))
       :dpapi)
      ((equal chosen "file") :file)
      ((dpapi-available-p) :dpapi)
      (t :file))))

(defun protection-line (protection)
  "PROTECTION in the words the operator needs: what it is, and what it is not."
  (ecase protection
    (:dpapi "dpapi (Windows DPAPI, this Windows account only)")
    (:file "0600 file, unencrypted (this platform has no DPAPI, so every account that can read the file can read every entry)")))

(defun parse-map (lines)
  "The section's NAME=entry lines => (values ALIST WARNINGS). Nothing here
 raises on a malformed line, as the original's parse_map did not: a bad line
 is a warning and the rest still resolve."
  (let ((mapping '())
        (warnings '()))
    (dolist (line lines)
      (let* ((text (trim line))
             (break (position #\= text)))
        (cond
          ((zerop (length text)) nil)
          ((null break)
           (push (format nil "skipping ~s: a mapping is written NAME=entry" text) warnings))
          (t
           (let ((name (trim (subseq text 0 break)))
                 (entry (clean-name (subseq text (1+ break)))))
             (cond
               ((not (valid-env-name-p name))
                (push (format nil "skipping ~s: not a valid environment variable name" name) warnings))
               ((null entry)
                (push (format nil "skipping ~a: the store name after = is empty" name) warnings))
               (t (push (cons name entry) mapping))))))))
    (values (nreverse mapping) (nreverse warnings))))

(defun mapped-entries ()
  "A hash-table from store entry name to the variables mapped to it."
  (let ((marks (make-hash-table :test #'equal)))
    (multiple-value-bind (mapping warnings) (parse-map (setting :map))
      (declare (ignore warnings))
      (dolist (pair mapping)
        (push (car pair) (gethash (cdr pair) marks))))
    marks))

(defun store-map ()
  "The store's map, and its path, or a refusal naming the store and how to
 make one."
  (let ((path (store-path)))
    (unless (probe-file path)
      (fail "no DPAPI secret store at ~a yet; /dpapi-secrets set NAME makes one (~a)"
            (namestring path) (protection-line (protection))))
    (values (read-store path) path)))

;;; --- the verbs ---------------------------------------------------------------

(defun entries ()
  "The store's entry names, sorted, and never a value."
  (store-entries (store-map)))

(defun secret (name)
  "One entry of the store, by its name inside it."
  (running-settings)
  (let ((entry (clean-name name)))
    (unless entry
      (fail "which entry? give its name inside the store, as /dpapi-secrets list shows them"))
    (multiple-value-bind (map path) (store-map)
      (multiple-value-bind (value found) (store-entry map entry)
        (unless found
          (fail "no entry named ~s in ~a; /dpapi-secrets list shows what is there"
                entry (namestring path)))
        value))))

(defun resolve-all ()
  "The mapping resolved against the store => (values ALIST WARNINGS)."
  (multiple-value-bind (mapping warnings) (parse-map (setting :map))
    (when (null mapping)
      (fail "nothing is mapped: set `map` in the dpapi-secrets section, one NAME=entry per line, so the store has a variable to answer"))
    (multiple-value-bind (map path) (store-map)
      (declare (ignore path))
      (let ((pairs '()))
        (dolist (pair mapping)
          (multiple-value-bind (value found) (store-entry map (cdr pair))
            (cond
              ((not found)
               (push (format nil "~a: no entry named ~s in the store" (car pair) (cdr pair)) warnings))
              ((zerop (length value))
               (push (format nil "~a: entry ~s is empty" (car pair) (cdr pair)) warnings))
              (t (push (cons (car pair) value) pairs)))))
        (when (null pairs)
          (fail "no mapped entry could be read from the DPAPI secret store~@[ (~{~a~^; ~})~]"
                (nreverse warnings)))
        (values (nreverse pairs) (nreverse warnings))))))

(defun resolve ()
  "The mapped variables the store answers, as an alist: a form's way to read
 what the original injected at startup, without a tool call."
  (running-settings)
  (resolve-all))

(defun write-entry (name value)
  "NAME set to VALUE in the store, which is created or replaced."
  (let ((entry (clean-name name)))
    (unless entry
      (fail "an entry needs a name inside the store, as /dpapi-secrets list shows them"))
    (when (zerop (length (or value "")))
      (fail "refusing to store an empty value for ~a" entry))
    (when (find (code-char 0) value)
      (fail "the value for ~a contains a NUL byte and could not be an environment variable" entry))
    (let* ((chosen (protection))
           (path (store-path))
           (map (if (probe-file path)
                    (read-store path)
                    (make-hash-table :test #'equal))))
      (setf (gethash entry map) value)
      (write-store path map chosen)
      (format nil "stored ~a in ~a (~a entr~:p, ~a)" entry (namestring path)
              (hash-table-count map) (protection-line chosen)))))

(defun remove-entry (name)
  "NAME taken out of the store."
  (let ((entry (clean-name name)))
    (unless entry
      (fail "an entry needs a name inside the store, as /dpapi-secrets list shows them"))
    (multiple-value-bind (map path) (store-map)
      (unless (nth-value 1 (store-entry map entry))
        (fail "no entry named ~s in ~a; /dpapi-secrets list shows what is there"
              entry (namestring path)))
      (remhash entry map)
      (write-store path map (protection))
      (format nil "removed ~a from ~a (~a entr~:p left)" entry (namestring path)
              (hash-table-count map)))))

;;; --- what the tools answer -----------------------------------------------------

(defun marked (name marks)
  "NAME as one line of a listing: the entry, and the variables mapped to it."
  (let ((variables (gethash name marks)))
    (if variables
        (format nil "~a  env: ~{~a~^, ~}" name (sort variables #'string<))
        (format nil "~a  (no variable maps to it)" name))))

(defun listing-text ()
  "The entries, names only, each with the variables mapped to it."
  (multiple-value-bind (map path) (store-map)
    (let ((names (store-entries map))
          (marks (mapped-entries)))
      (if (null names)
          (format nil "the store at ~a is empty; /dpapi-secrets set NAME writes the first entry"
                  (namestring path))
          (clip (format nil "~a entr~:p in ~a (~a; names only, no value):~%~{~a~%~}"
                        (length names) (namestring path)
                        (protection-line (protection))
                        (mapcar (lambda (name) (marked name marks)) names)))))))

(defun status-text ()
  "What the store is and how it stands: never a value."
  (let ((path (store-path)))
    (with-output-to-string (answer)
      (format answer "dpapi-secrets: ~a~%store: ~a~@[ (missing: /dpapi-secrets set NAME makes one)~]~%"
              (protection-line (protection)) (namestring path) (null (probe-file path)))
      (if (null (probe-file path))
          (format answer "entries: 0~%")
          (handler-case
              (multiple-value-bind (map found) (read-store path)
                (declare (ignore found))
                (format answer "entries: ~a~@[ (~{~a~^, ~})~]~%"
                        (hash-table-count map) (store-entries map)))
            (dpapi-secrets-error (condition)
              (format answer "entries: unreadable (~a)~%" condition))))
      (multiple-value-bind (mapping warnings) (parse-map (setting :map))
        (format answer "map: ~a~@[ (~{~a~^, ~})~]~@[; ~a mapping~:p refused: ~{~a~^; ~}~]~%"
                (if mapping
                    (format nil "~a variable~:p" (length mapping))
                    "none set, so nothing is resolved")
                (mapcar (lambda (pair) (format nil "~a=~a" (car pair) (cdr pair))) mapping)
                (length warnings) warnings)))))

(defun list-entries (arguments)
  "dpapi_list: the entry names, and the variables mapped to them."
  (declare (ignore arguments))
  (running-settings)
  (listing-text))

(defun read-secret (report)
  "dpapi_show: one entry's value, the value alone."
  (running-settings)
  (secret (entry-name report)))

(defun store-secret (report)
  "dpapi_set: one entry written into the store."
  (running-settings)
  (write-entry (entry-write-name report) (entry-write-value report)))

(defun drop-secret (report)
  "dpapi_unset: one entry removed from the store."
  (running-settings)
  (remove-entry (entry-name report)))

(defun bulk-read (arguments)
  "dpapi_env: the mapping as NAME=value lines, and every warning it raised."
  (declare (ignore arguments))
  (running-settings)
  (multiple-value-bind (pairs warnings) (resolve-all)
    (clip (with-output-to-string (answer)
            (format answer "~a secret~:p as NAME=value, resolved from ~a (a value's own line breaks are kept):~%"
                    (length pairs) (namestring (store-path)))
            (dolist (pair pairs)
              (format answer "~a=~a~%" (car pair) (cdr pair)))
            (when warnings
              (format answer "~%[~a warning~:p: ~{~a~^; ~}]" (length warnings) warnings))))))

(defun status (arguments)
  "dpapi_status: the store, its protection, its count, and the mapping."
  (declare (ignore arguments))
  (running-settings)
  (status-text))

;;; --- /dpapi-secrets ----------------------------------------------------------

(defun ask-row (label ask line &key placeholder verb)
  "A row whose Enter asks for one line of text under ASK and runs LINE with
 it; written out here rather than asked of NLE:ASK-ROW, so a release from
 before that helper still loads this cell."
  (list :label label
        :ask (list :label ask :line line
                   :placeholder (or placeholder "") :verb (or verb ""))))

(defun panel ()
  "The dialog bare /dpapi-secrets answers: the store, over the two things a
 person does to it."
  (nle:list-dialog
   :title "DPAPI secrets"
   :summary (status-text)
   :rows (list (ask-row "Store a value" "Entry name" "/dpapi-secrets set "
                        :placeholder "openai" :verb "store")
               (ask-row "Show a value" "Entry name" "/dpapi-secrets show "
                        :placeholder "openai" :verb "show"))))

(defun unset-panel ()
  "A dialog asking which entry to remove."
  (nle:list-dialog
   :title "Remove an entry"
   :summary "the entry leaves the store; its value is not kept anywhere"
   :rows (list (ask-row "Entry name" "Entry name" "/dpapi-secrets unset "
                        :placeholder "openai" :verb "remove"))))

(defun set-panel (name)
  "A dialog asking for NAME's value, for a store whose protection is CHOSEN."
  (let ((chosen (protection)))
    (nle:list-dialog
     :title "Store a value"
     :summary (format nil "the value for ~a; the store keeps it as ~a" name (protection-line chosen))
     :rows (list (ask-row "Value" "Value" (format nil "/dpapi-secrets set ~a " name)
                          :placeholder "the secret" :verb "store")))))

(defun run-command (args session-id)
  "The /dpapi-secrets command: list, show, set, unset, env, path, status."
  (declare (ignore session-id))
  (running-settings)
  (let* ((typed (trim (or args "")))
         (parts (words typed))
         (verb (first parts))
         (rest (rest parts)))
    (cond
      ((null parts)
       (values (status-text) (panel)))
      ((member verb '("list" "ls") :test #'equal)
       (values (listing-text)))
      ((member verb '("status" "check") :test #'equal)
       (values (status-text)))
      ((equal verb "path")
       (values (namestring (store-path))))
      ((equal verb "env")
       (values (bulk-read nil)))
      ((member verb '("show" "get") :test #'equal)
       (if (first rest)
           (values (secret (first rest)))
           (values "which entry? /dpapi-secrets list shows the names"
                   (nle:list-dialog
                    :title "Show an entry" :summary "the value alone, into this conversation"
                    :rows (list (ask-row "Entry name" "Entry name" "/dpapi-secrets show "
                                          :placeholder "openai" :verb "show"))))))
      ((member verb '("set" "add") :test #'equal)
       (let ((name (first rest))
             (value (and (rest rest) (trim (format nil "~{~a~^ ~}" (rest rest))))))
         (cond
           ((null name)
            (values "which entry? name it, and the value is asked for next"
                    (nle:list-dialog
                     :title "Store an entry"
                     :summary "the value is asked next, and the store never echoes it back"
                     :rows (list (ask-row "Entry name" "Entry name" "/dpapi-secrets set "
                                           :placeholder "openai" :verb "next")))))
           (t
            (if value
                (values (write-entry name value))
                (values (format nil "storing ~a: the value is asked next" name)
                        (set-panel name)))))))
      ((equal verb "unset")
       (if (first rest)
           (values (remove-entry (first rest)))
           (values "which entry?"
                   (unset-panel))))
      (t
       (values (format nil "dpapi-secrets: ~a is not one of list, show, set, unset, env, path, status~%~%~a"
                       verb (status-text)))))))

;;; --- the start -----------------------------------------------------------------

(defun start ()
  "Say once, on the board, what stands between the mapping and the store: no
 store yet, or entries the store cannot answer. A section with no map is the
 original's disabled source and says nothing."
  (when (setting :map)
    (let ((problem
            (cond
              ((not (probe-file (store-path)))
               (format nil "dpapi-secrets: no store at ~a yet; /dpapi-secrets set NAME makes one"
                       (namestring (store-path))))
              (t
               (handler-case
                   (multiple-value-bind (pairs warnings) (resolve-all)
                     (declare (ignore pairs))
                     (when warnings
                       (format nil "dpapi-secrets: ~{~a~^; ~}" warnings)))
                 (dpapi-secrets-error (condition)
                   (format nil "~a" condition)))))))
      (when problem
        (nle:notice problem :level :warning :key "nodecode-dpapi-secrets")
        (nle:on-stop (lambda () (nle:notice nil :key "nodecode-dpapi-secrets")))))))

;;; --- the boundaries ---------------------------------------------------------------

(defun no-arguments (description)
  "The schema a tool that takes no arguments registers."
  (list :description description
        :parameters (nlk:json-object "type" "object"
                                      "properties" (nlk:json-object))))

(nle:define-cell dpapi-secrets
  (:section ("dpapi-secrets")
    (:guide "create the store with /dpapi-secrets set NAME (the value is asked); map a variable to an entry with one NAME=entry line in `map`; on Windows the file is encrypted with DPAPI, elsewhere it is a 0600 file and status says so")
    ("file" :path :default "secrets.dpapi"
            :doc "the store file, relative to this home; a store the Hermes plugin wrote is read unchanged")
    ("protection" :choice :options '("auto" "dpapi" "file") :default "auto"
                  :doc "how the store is protected: auto is DPAPI on Windows and a 0600 file elsewhere")
    ("map" :list :default '()
           :doc "the variables to resolve, as NAME=entry, for example [\"OPENAI_API_KEY=openai\"]")
    ("max_chars" :integer :default 20000 :min 200
                 :doc "how much of one answer the model is given"))
  (:tool "dpapi_list"
         (no-arguments "The store's entry names, and the environment variable each is mapped to by the section's `map`. Names only: not one value.")
         'list-entries)
  (:tool "dpapi_show" (nlk:contract 'entry) 'read-secret)
  (:tool "dpapi_set" (nlk:contract 'entry-write) 'store-secret)
  (:tool "dpapi_unset" (nlk:contract 'entry) 'drop-secret)
  (:tool "dpapi_env"
         (no-arguments "The mapping the original plugin injected at startup, as one NAME=value line per `map` line the store answers, with a warning for each it cannot. A plain read of every value, so prefer dpapi_show for one key.")
         'bulk-read)
  (:tool "dpapi_status"
         (no-arguments "Where the store is, how it is protected on this platform, how many entries it holds and how the mapping stands. Reads no value.")
         'status)
  (:help :dpapi-secrets
         "dpapi-secrets: one DPAPI-encrypted store of named secrets, resolved by name"
         +primer+)
  (:command "dpapi-secrets" 'run-command
            :description "The DPAPI secret store: list, show, set, unset, env, path, status"
            :argument-hint "list")
  (:start #'start))
