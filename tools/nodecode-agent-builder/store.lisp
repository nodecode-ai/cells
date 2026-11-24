;;;; store.lisp --- a draft is one JSON file under the home.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original kept drafts in a profile-scoped sqlite table with an
;;;; optimistic revision check (builder.py:139 DraftStore). One Nodecode home
;;;; is one store, so a draft is a file: agent-builder/<id>.json under
;;;; (nlk:home), written atomically, named by the draft's own 32 hex id. A
;;;; revision still rides every draft so a caller that read it can say which
;;;; one it edited; the file is the whole authority and nothing else is
;;;; written anywhere.

(in-package #:nodecode-agent-builder)

(defun store-directory ()
  "The folder drafts live in, under the home the cell is serving."
  (nlk:home "agent-builder/"))

(defun draft-path (id)
  "Where draft ID is written."
  (nlk:home (format nil "agent-builder/~a.json" id)))

(defun new-id ()
  "A fresh 32 hex character draft id: the tail of a digest, which is hex and
carries no separator."
  (let ((digest (nlk:sha256-text (format nil "~a-~a-~a-~a"
                                         (get-universal-time) (get-internal-real-time)
                                         (random most-positive-fixnum) (gensym)))))
    (subseq digest (- (length digest) 32))))

(defun draft-id-p (value)
  "VALUE is a draft id this store could have written."
  (and (stringp value)
       (= 32 (length value))
       (loop for char across value always (digit-char-p char 16))))

(defun require-draft-id (value)
  "VALUE as a draft id, or a refusal."
  (unless (draft-id-p value)
    (fail "~s is not a draft id: 32 hex characters, as /agent-builder list writes them."
          (or value "nothing")))
  value)

(defun draft-id (draft) (nlk:json-value draft :string "id"))
(defun draft-version (draft) (or (nlk:json-value draft :integer "version") 1))
(defun draft-revision (draft) (or (nlk:json-value draft :integer "revision") 0))
(defun draft-plan (draft) (nlk:json-value draft :object "plan"))
(defun draft-brief (draft) (or (nlk:json-value draft :string "brief") ""))

(defun references-of (draft)
  "DRAFT's reference files as (NAME . CONTENT), in the order they were attached."
  (let ((items (nlk:json-value draft :array "references")))
    (and items
         (loop for item across items
               for name = (nlk:json-value item :string "name")
               for content = (nlk:json-value item :string "content")
               when (and name content) collect (cons name content)))))

(defun references-json (references)
  "REFERENCES, a list of (NAME . CONTENT), as the JSON array a draft stores."
  (coerce (loop for (name . content) in references
                 collect (nlk:make-json-object "name" name "content" content))
          'vector))

(defun load-draft (id)
  "Draft ID from the store, or a refusal saying it is not there."
  (let ((path (draft-path (require-draft-id id))))
    (unless (probe-file path)
      (fail "No draft ~a in this home: /agent-builder list names the ones that are." id))
    (let ((draft (handler-case (nlk:decode-json (uiop:read-file-string path))
                   (error () nil))))
      (unless (hash-table-p draft)
        (fail "Draft ~a is not readable: delete it with /agent-builder drop ~a." id id))
      draft)))

(defun save-draft (draft)
  "DRAFT written where the store keeps it, its revision one past what was
there. => DRAFT."
  (let ((previous (ignore-errors (load-draft (draft-id draft)))))
    (setf (gethash "revision" draft) (1+ (if previous (draft-revision previous) 0))
          (gethash "updated" draft) (get-universal-time))
    (nlk:write-file-atomically (draft-path (draft-id draft))
                                (nlk:encode-json-object draft))
    draft))

(defun stored-drafts ()
  "Every draft in this home, newest first."
  (let ((files (and (uiop:directory-exists-p (store-directory))
                    (uiop:directory-files (store-directory) "*.json"))))
    (sort (loop for file in files
                for draft = (handler-case (nlk:decode-json (uiop:read-file-string file))
                              (error () nil))
                when (hash-table-p draft) collect draft)
          #'> :key (lambda (draft) (or (nlk:json-value draft :integer "updated") 0)))))

(defun forget-draft (id)
  "Draft ID's file, gone. => T."
  (let ((path (draft-path (require-draft-id id))))
    (unless (probe-file path)
      (fail "No draft ~a in this home." id))
    (delete-file path)
    t))
