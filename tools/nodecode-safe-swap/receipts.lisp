;;;; receipts.lisp --- what is kept where, and the POSIX that puts it back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A receipt is a directory, not a row: <quarantine>/<id> and
;;;; <quarantine>/<id>.<n>, one per rewritten command of a compound one. Every
;;;; file in it is plain (manifest.tsv, stash.sha, branches.tsv, clean.tar,
;;;; stash.shas, original, kind, stats), so a person can read -- and finish --
;;;; an undo by hand, without this cell.

(in-package #:nodecode-safe-swap)

(defun expand-quarantine (directory)
  "DIRECTORY as a filesystem path: a leading ~ is the operator's home."
  (if (uiop:string-prefix-p "~/" directory)
      (namestring (merge-pathnames (subseq directory 2) (user-homedir-pathname)))
      directory))

(defun quarantine-dir ()
  "The quarantine this cell runs on, resolved now."
  (expand-quarantine (setting :quarantine-dir)))

(defun valid-id-p (id)
  "Whether ID is a receipt id: six lowercase hex digits."
  (and (stringp id) (= 6 (length id))
       (every (lambda (character) (find character "0123456789abcdef")) id)))

(defun checked-id (id)
  "ID, or a refusal naming it."
  (unless (valid-id-p id)
    (fail "~s is not a receipt id: six hex digits, as /safe-swap list shows them" id))
  id)

(defun slot-directories (base id)
  "The quarantine directories receipt ID owns, smallest first."
  (let ((root (uiop:ensure-directory-pathname base)))
    (sort (remove-if-not
           (lambda (path)
             (let ((name (car (last (pathname-directory path)))))
               (and (stringp name)
                    (or (string= name id)
                        (and (uiop:string-prefix-p (cat id ".") name)
                             (every (lambda (character) (find character "0123456789"))
                                    (subseq name (1+ (length id)))))))))
           (uiop:subdirectories root))
          #'string< :key #'namestring)))

(defun slot-file (directory name)
  "The text of NAME in DIRECTORY, trimmed, or NIL."
  (let ((path (merge-pathnames name (uiop:ensure-directory-pathname directory))))
    (when (probe-file path)
      (string-trim '(#\Space #\Tab #\Newline #\Return)
                   (uiop:read-file-string path)))))

(defun slot-words (directory)
  "The two numbers of the slot's stats file: files and bytes."
  (let ((text (slot-file directory "stats")))
    (when text
      (let* ((parts (uiop:split-string text :separator " "))
             (files (ignore-errors (parse-integer (first parts) :junk-allowed t)))
             (bytes (ignore-errors (parse-integer (or (second parts) "0") :junk-allowed t))))
        (values (or files 0) (or bytes 0))))))

(defun receipt-ids (base)
  "Every receipt id under BASE, newest first."
  (let ((seen '()) (entries '()))
    (dolist (directory (uiop:subdirectories (uiop:ensure-directory-pathname base)))
      (let ((name (car (last (pathname-directory directory)))))
        (when (and (stringp name) (>= (length name) 6) (valid-id-p (subseq name 0 6)))
          (let ((id (subseq name 0 6)))
            (unless (member id seen :test #'string=)
              (push id seen)
              (push (cons id (or (ignore-errors (file-write-date directory)) 0)) entries))))))
    (mapcar #'car (sort entries #'> :key #'cdr))))

(defun receipt-plist (base id)
  "What the list says of receipt ID: its kind, command, age and blast radius."
  (let* ((slots (slot-directories base id))
         (first (first slots))
         (files 0) (bytes 0))
    (dolist (slot slots)
      (multiple-value-bind (slot-files slot-bytes) (slot-words slot)
        (incf files slot-files)
        (incf bytes slot-bytes)))
    (list :id id
          :kind (or (and first (slot-file first "kind")) "?")
          :original (or (and first (slot-file first "original")) "?")
          :written (or (and first (ignore-errors (file-write-date first))) 0)
          :files files :bytes bytes)))

(defun age-text (written now)
  "HOW long ago WRITTEN was, in one or two characters, or 'now'."
  (let ((seconds (max 0 (- now written))))
    (cond ((< seconds 90) "now")
          ((< seconds 5400) (format nil "~dm" (round seconds 60)))
          ((< seconds 172800) (format nil "~dh" (round seconds 3600)))
          (t (format nil "~dd" (round seconds 86400))))))

(defun human-bytes (bytes)
  "BYTES as a person reads them: 812 B, 10.2 MB."
  (let ((size (coerce bytes 'double-float)))
    (loop for unit in '("KB" "MB" "GB")
          do (setf size (/ size 1024))
             (when (or (< size 1024) (string= unit "GB"))
               (return (if (>= size 9.95)
                           (format nil "~d ~a" (round size) unit)
                           (format nil "~,1f ~a" size unit))))
          finally (return (format nil "~d B" bytes)))))

(defun blast-text (files bytes)
  "FILES and BYTES as the note says them."
  (format nil "~d file~:p, ~a" files (human-bytes bytes)))

(defun receipt-table (base &key (limit 20))
  "The receipt list, newest first, as the operator reads it."
  (let ((ids (receipt-ids base)))
    (if (null ids)
        (format nil "No receipts. Destructive commands run since this cell started a swap are listed here.~%")
        (with-output-to-string (out)
          (format out "~6,'0@a  ~5a ~8a ~18a ~a~%" "id" "age" "kind" "size" "command")
          (dolist (id (subseq ids 0 (min limit (length ids))))
            (let ((receipt (receipt-plist base id)))
              (format out "~6a  ~5a ~8a ~18a ~a~%"
                      id (age-text (getf receipt :written) (get-universal-time))
                      (getf receipt :kind)
                      (if (plusp (getf receipt :files))
                          (blast-text (getf receipt :files) (getf receipt :bytes))
                          "-")
                      (getf receipt :original))))
          (format out "Undo with /safe-swap undo <id> (or /safe-swap undo last); /safe-swap purge frees the space.~%")))))


;;; --- putting it back ---------------------------------------------------------

(defparameter +restore-tail+
  (cat "done
"
       "[ \"$SS_ANY\" = 1 ] || echo 'nothing to restore'
"
       "if [ \"$SS_RC\" = 0 ]; then true; else false; fi
")
  "How every restore script ends: the status of the whole undo.")

(defun restore-script (base id &key force)
  "The POSIX that puts back everything receipt ID quarantined or snapshotted."
  (declare (ignore force))
  (let ((quoted (sh-quote (namestring (uiop:ensure-directory-pathname base)))))
    (cat "SS_B=" quoted "; SS_RC=0; SS_ANY=0; SS_TAB=$(printf '\\t')
"
         "for SS_Q in \"$SS_B/" id "\" \"$SS_B/" id "\".*; do
"
         "  [ -d \"$SS_Q\" ] || continue
"
         "  if [ -f \"$SS_Q/manifest.tsv\" ]; then
"
         "    SS_ANY=1; SS_OK=1
"
         "    while IFS=\"$SS_TAB\" read -r SS_N SS_O; do
"
         "      [ -n \"$SS_N\" ] && [ -n \"$SS_O\" ] || continue
"
         "      if [ ! -e \"$SS_Q/$SS_N\" ] && [ ! -L \"$SS_Q/$SS_N\" ]; then echo \"missing in quarantine: $SS_O\"; SS_OK=0; continue; fi
"
         "      if [ -e \"$SS_O\" ] || [ -L \"$SS_O\" ]; then echo \"exists, not overwritten: $SS_O\"; SS_OK=0; continue; fi
"
         "      if mkdir -p -- \"$(dirname -- \"$SS_O\")\" && mv -- \"$SS_Q/$SS_N\" \"$SS_O\"; then echo \"restored: $SS_O\"; else echo \"failed: $SS_O\"; SS_OK=0; fi
"
         "    done < \"$SS_Q/manifest.tsv\"
"
         "    [ \"$SS_OK\" = 1 ] || SS_RC=1
"
         "  fi
"
         "  if [ -f \"$SS_Q/gitdir\" ]; then
"
         "    SS_G=$(cat \"$SS_Q/gitdir\")
"
         "    if [ -f \"$SS_Q/head\" ]; then
"
         "      SS_ANY=1; SS_H=$(cat \"$SS_Q/head\"); SS_NOW=$(cd -- \"$SS_G\" && git rev-parse HEAD 2>/dev/null)
"
         "      if [ \"$SS_H\" != \"$SS_NOW\" ]; then
"
         "        if (cd -- \"$SS_G\" && git merge-base --is-ancestor \"$SS_NOW\" \"$SS_H\" && git diff --quiet HEAD -- && git reset --hard --quiet \"$SS_H\"); then
"
         "          echo \"moved HEAD back to $SS_H\"
"
         "        else echo \"note: HEAD was $SS_H before the command (not moved: the tree or history changed since)\"; fi
"
         "      fi
"
         "    fi
"
         "    if [ -f \"$SS_Q/stash.sha\" ]; then
"
         "      SS_ANY=1
"
         "      if (cd -- \"$SS_G\" && git stash apply --quiet \"$(cat \"$SS_Q/stash.sha\")\"); then echo \"restored working-tree changes in $SS_G\"; mv -- \"$SS_Q/stash.sha\" \"$SS_Q/stash.sha.applied\"; else echo \"git stash apply failed in $SS_G (conflicts?)\"; SS_RC=1; fi
"
         "    fi
"
         "    if [ -f \"$SS_Q/branches.tsv\" ]; then
"
         "      SS_ANY=1
"
         "      while IFS=\"$SS_TAB\" read -r SS_BN SS_BS; do
"
         "        [ -n \"$SS_BN\" ] && [ -n \"$SS_BS\" ] || continue
"
         "        if (cd -- \"$SS_G\" && git branch \"$SS_BN\" \"$SS_BS\" >/dev/null); then echo \"restored branch: $SS_BN at $SS_BS\"; else echo \"failed to restore branch: $SS_BN\"; SS_RC=1; fi
"
         "      done < \"$SS_Q/branches.tsv\"
"
         "      mv -- \"$SS_Q/branches.tsv\" \"$SS_Q/branches.tsv.restored\"
"
         "    fi
"
         "    if [ -f \"$SS_Q/stash.shas\" ]; then
"
         "      SS_ANY=1; SS_OK=1
"
         "      while read -r SS_H; do
"
         "        [ -n \"$SS_H\" ] || continue
"
         "        SS_M=$(cd -- \"$SS_G\" && git log -1 --format=%s \"$SS_H\" 2>/dev/null)
"
         "        if (cd -- \"$SS_G\" && git stash store -m \"$SS_M\" \"$SS_H\" 2>/dev/null); then echo \"restored stash entry: $SS_H\"; else echo \"failed to restore stash entry: $SS_H\"; SS_OK=0; fi
"
         "      done < \"$SS_Q/stash.shas\"
"
         "      if [ \"$SS_OK\" = 1 ]; then mv -- \"$SS_Q/stash.shas\" \"$SS_Q/stash.shas.restored\"; else SS_RC=1; fi
"
         "    fi
"
         "    if [ -f \"$SS_Q/clean.tar\" ]; then
"
         "      SS_ANY=1
"
         "      if tar -xkf \"$SS_Q/clean.tar\" -C \"$SS_G\"; then echo \"restored the files git clean deleted in $SS_G\"; mv -- \"$SS_Q/clean.tar\" \"$SS_Q/clean.tar.restored\"; else echo \"could not restore the files git clean deleted\"; SS_RC=1; fi
"
         "    fi
"
         "  fi
"
         "done
"
         +restore-tail+)))

(defun purge-script (base id)
  "The POSIX that deletes receipt ID's quarantine; the only deletion this cell does."
  (cat "SS_B=" (sh-quote (namestring (uiop:ensure-directory-pathname base))) "; "
       "for SS_D in \"$SS_B/" id "\" \"$SS_B/" id "\".*; do [ -d \"$SS_D\" ] || continue; "
       "if [ -f \"$SS_D/gitdir\" ]; then (cd -- \"$(cat \"$SS_D/gitdir\")\" && git update-ref -d \"refs/safe-swap/${SS_D##*/}\") 2>/dev/null; fi; "
       "rm -rf -- \"$SS_D\"; done; true"))

(defun run-script (script &key (seconds 120))
  "Run SCRIPT through sh; its answer and status."
  (multiple-value-bind (output errors status)
      (nlk:run-bounded (list "sh" "-c" script) :seconds seconds)
    (values (string-trim '(#\Space #\Newline)
                         (cat (or output "") (or errors "")))
            status)))
