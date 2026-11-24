;;;; library.lisp --- the saved prompts: one JSON file under the home, and the
;;;; five enhancers the original ships.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original keeps its library in the plugin host's storage: folders of
;;;; prompts, each a name, a body and a glyph, seeded with the five enhancers
;;;; below, and read and written by the composer dialog. A cell has no dialog,
;;;; so the library is a file the operator and the model both reach: the same
;;;; folders, the same names, the same bodies, carried as JSON and read and
;;;; written whole. Nothing here reaches the network; the enhancers are data,
;;;; so a layer can reword them, and the file is the truth once it exists.

(in-package #:nodecode-prompt-enhance)

(defparameter +library-version+ 1
  "What the library file says it is, so a later shape can tell itself apart.")

(defparameter +enhancer-folder+ "enhancers"
  "The folder the seeded enhancers live in: the original's locked one.")

(defparameter +prompt-folder+ "prompts"
  "The folder a prompt is saved in when the caller names none.")

(defparameter +enhancers+
  '(("clarify" . "Make the ask unambiguous. Keep the intent. Do not add requirements.")
    ("tighten" . "Cut fluff. Keep the ask and the constraints already in the draft.")
    ("grammar" . "Fix grammar and style. Leave the meaning alone.")
    ("expand" . "Turn the draft into an ordered plan. Keep it concrete.")
    ("summarize" . "Summarize the draft. Keep the decisions."))
  "The five enhancers the original seeds, name (lowercase, as DEFAULTS there)
and instruction unchanged: an enhancer is instruction text sent to the model
with the draft, so the words are the behaviour.")

(defparameter +draft-limit+ 40000
  "The longest draft one rewrite takes, in characters: past it the endpoint
pays for the whole composer and the answer is rarely worth it.")

(defvar *library-path* nil
  "The file the library is read from and written to; NIL means
<home>/prompt-enhance/library.json. A test binds it to a temp file.")

;;; --- where ---------------------------------------------------------------------------

(defun library-file ()
  "The library's file: the one bound, else under the home."
  (or *library-path* (nlk:home "prompt-enhance/library.json")))

(defun nonblank (value)
  "VALUE trimmed when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value)))

;;; --- reading and writing ----------------------------------------------------------

(defun seed-library ()
  "The library a home with no file yet starts from: the original's five
enhancers, in their folder, in their order."
  (loop for (name . body) in +enhancers+
        collect (list :name name :folder +enhancer-folder+ :body body)))

(defun read-library-file (path)
  "Every prompt PATH holds, oldest first, skipping an entry without a name or
a body; a file that is not readable JSON is a refusal naming the file."
  (let ((object (handler-case (nlk:decode-json (uiop:read-file-string path))
                  (error (condition)
                    (fail "~a is not readable JSON (~a): fix it or move it aside, and the next save writes a fresh one"
                          path condition)))))
    (loop for entry across (or (nlk:json-value object :array "prompts") #())
          for name = (nlk:json-value entry :text "name")
          for body = (nlk:json-value entry :string "body")
          when (and name body (plusp (length (nlk:trimmed body))))
            collect (list :name name
                          :folder (or (nlk:json-value entry :text "folder") +prompt-folder+)
                          :body body))))

(defun read-library ()
  "Every saved prompt: the file's when it exists, else the seeded enhancers."
  (let ((path (library-file)))
    (if (probe-file path)
        (read-library-file path)
        (seed-library))))

(defun write-library (prompts)
  "Write PROMPTS to the library's file, making its folder first. => the path."
  (let* ((path (library-file))
         (object (nlk:make-json-object
                  "version" +library-version+
                  "prompts" (coerce (loop for prompt in prompts
                                           collect (nlk:make-json-object
                                                    "name" (getf prompt :name)
                                                    "folder" (getf prompt :folder)
                                                    "body" (getf prompt :body)))
                                     'vector))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
      (write-string (nlk:encode-json-object object) out)
      (terpri out))
    path))

;;; --- the verbs over it -----------------------------------------------------------

(defun check-name (name)
  "NAME as a prompt's: trimmed, 1 to 80 characters, one line, or a refusal."
  (let ((name (nlk:trimmed (or name ""))))
    (cond ((zerop (length name))
           (fail "a saved prompt needs a name"))
          ((> (length name) 80)
           (fail "a prompt name is at most 80 characters, and ~s is ~d" name (length name)))
          ((find-if (lambda (character) (member character '(#\Newline #\Return #\Tab))) name)
           (fail "a prompt name is one line: ~s carries a line break" name))
          (t name))))

(defun check-body (body)
  "BODY as a prompt's: trimmed, and not empty."
  (let ((body (nlk:trimmed (or body ""))))
    (if (zerop (length body))
        (fail "a saved prompt needs a body: the text it puts in the composer")
        body)))

(defun check-folder (folder)
  "FOLDER as a prompt's folder: trimmed, the default when blank, one line."
  (let ((folder (nlk:trimmed (or folder ""))))
    (cond ((zerop (length folder)) +prompt-folder+)
          ((> (length folder) 60)
           (fail "a folder name is at most 60 characters, and ~s is ~d" folder (length folder)))
          ((find-if (lambda (character) (member character '(#\Newline #\Return #\Tab))) folder)
           (fail "a folder name is one line: ~s carries a line break" folder))
          (t folder))))

(defun find-prompt (name)
  "The prompt called NAME, case-insensitively; the enhancers win a tie with a
prompt folder's, as the original's Enhance menu did."
  (let ((name (nlk:trimmed (or name ""))))
    (when (plusp (length name))
      (let ((matches (remove-if-not (lambda (prompt)
                                       (string-equal (getf prompt :name) name))
                                     (read-library))))
        (or (find-if (lambda (prompt) (string-equal (getf prompt :folder) +enhancer-folder+)) matches)
            (first matches))))))

(defun save-prompt (name body &optional folder)
  "Save BODY under NAME in FOLDER, replacing that folder's prompt of the same
name, and answer the prompt saved."
  (let* ((name (check-name name))
         (body (check-body body))
         (folder (check-folder folder))
         (saved (list :name name :folder folder :body body))
         (prompts (read-library))
         (replaced (find-if (lambda (prompt)
                              (and (string-equal (getf prompt :name) name)
                                   (string-equal (getf prompt :folder) folder)))
                            prompts))
         (next (if replaced
                   (mapcar (lambda (prompt) (if (eq prompt replaced) saved prompt)) prompts)
                   (append prompts (list saved)))))
    (write-library next)
    (values saved (not (null replaced)))))

(defun delete-prompt (name)
  "Remove every prompt called NAME and answer how many went; none is a refusal."
  (let* ((name (check-name name))
         (prompts (read-library))
         (kept (remove-if (lambda (prompt) (string-equal (getf prompt :name) name)) prompts))
         (gone (- (length prompts) (length kept))))
    (when (zerop gone)
      (fail "no saved prompt is called ~s; prompt_list shows the ones that are" name))
    (write-library kept)
    gone))

;;; --- what a prompt looks like in a line ---------------------------------------------

(defun preview (body &optional (limit 90))
  "BODY as one line of at most LIMIT characters, its line breaks folded to
spaces, so a listing shows the shape of a prompt and not the whole of it."
  (let* ((flat (nlk:trimmed (substitute #\Space #\Newline (substitute #\Space #\Return body))))
         (limit (max 8 limit)))
    (if (> (length flat) limit)
        (concatenate 'string (subseq flat 0 (- limit 3)) "...")
        flat)))

(defun prompt-line (prompt)
  "One saved prompt as the line a listing carries: its name, its folder and
the first of its body."
  (format nil "~a  [~a]  ~a" (getf prompt :name) (getf prompt :folder)
          (preview (getf prompt :body))))
