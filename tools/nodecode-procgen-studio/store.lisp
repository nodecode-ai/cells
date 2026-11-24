;;;; store.lisp --- a render is a folder: the image, the source, the seed, one meta.json.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original kept every render in $HERMES_HOME with the source and a
;;;; reproducible seed beside the image, so a later /procgen reroll re-rendered
;;;; without a model call and /procgen source showed what was drawn. The same
;;;; here under the Nodecode home: plugin-data/procgen-studio/renders/<id>/,
;;;; one folder a render, holding image.png, its source and meta.json; plus a
;;;; copy into the section's output_dir when one is set, for a project that
;;;; wants the assets in its own tree.

(in-package #:nodecode-procgen-studio)

(defun data-dir ()
  "Where this cell keeps its renders and its set: under the Nodecode home."
  (nlk:home "plugin-data/procgen-studio/"))

(defun renders-dir ()
  "The folder of one folder a render."
  (merge-pathnames "renders/" (data-dir)))

(defun record-dir (id)
  "The folder render ID lives in."
  (merge-pathnames (format nil "~a/" id) (renders-dir)))

(defun slugify (text &optional (limit 40))
  "TEXT as a folder-safe word: lowercase letters, digits and dashes."
  (let* ((down (string-downcase (string-trim '(#\Space #\Tab) text)))
         (clean (with-output-to-string (out)
                  (loop for c across down
                        do (cond ((or (alphanumericp c) (find c "-_")) (write-char c out))
                                 ((member c '(#\Space #\/ #\. #\,) :test #'char=)
                                  (write-char #\- out))))))
         (trimmed (string-trim "-" clean)))
    (if (plusp (length trimmed)) (subseq trimmed 0 (min limit (length trimmed))) "set")))

(defun next-render-id ()
  "The next render's id: r0001, r0002, one counter for the home."
  (let* ((file (merge-pathnames "counter" (data-dir)))
         (last (or (ignore-errors (parse-integer (nlk:trimmed (or (nlk:read-text file) "0"))
                                                 :junk-allowed t))
                   0))
         (next (1+ last)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
      (format out "~d~%" next))
    (format nil "r~4,'0d" next)))

;;; --- the record: a plist, its rendering a meta.json -----------------------------------

(defun record-object (record)
  "RECORD as the JSON object its meta.json is."
  (nlk:json-object
   "format" 1
   "id" (getf record :id)
   "prompt" (getf record :prompt)
   "seed" (getf record :seed)
   "width" (getf record :width)
   "height" (getf record :height)
   "renderer" (getf record :renderer)
   "style" (getf record :style)
   "palette" (getf record :palette)
   "set" (getf record :set)
   "frames" (getf record :frames)
   "created" (getf record :created)
   "source" (namestring (getf record :source))
   "image" (namestring (getf record :image))))

(defun save-record (record)
  "Write RECORD's meta.json beside its image, and answer RECORD."
  (ensure-directories-exist (getf record :image))
  (with-open-file (out (merge-pathnames "meta.json" (getf record :dir))
                        :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string (nlk:encode-json-object (record-object record)) out))
  record)

(defun load-record (id)
  "The render ID as its record, read back from its meta.json."
  (let* ((dir (record-dir id))
         (path (merge-pathnames "meta.json" dir))
         (object (and (uiop:file-exists-p path) (ignore-errors (nlk:decode-json (nlk:read-text path))))))
    (unless (hash-table-p object)
      (fail "no render ~a in ~a" id (namestring (renders-dir))))
    (list :id (or (nlk:json-value object :string "id") id)
          :prompt (or (nlk:json-value object :string "prompt") "")
          :seed (nlk:json-value object :integer "seed")
          :width (nlk:json-value object :integer "width")
          :height (nlk:json-value object :integer "height")
          :renderer (or (nlk:json-value object :string "renderer") "?")
          :style (or (nlk:json-value object :string "style") "?")
          :palette (or (nlk:json-value object :string "palette") "?")
          :set (or (nlk:json-value object :string "set") "")
          :frames (nlk:json-value object :integer "frames")
          :created (or (nlk:json-value object :string "created") "")
          :source (merge-pathnames (or (nlk:json-value object :string "source") "source.svg") dir)
          :image (merge-pathnames (or (nlk:json-value object :string "image") "image.png") dir)
          :dir dir)))

(defun render-ids ()
  "Every render id this home holds, newest first."
  (let ((dir (renders-dir)))
    (if (uiop:directory-exists-p dir)
        (sort (loop for path in (uiop:subdirectories dir)
                    for name = (car (last (pathname-directory path)))
                    when (and (stringp name) (uiop:string-prefix-p "r" name))
                      collect name)
              #'string>)
        '())))

(defun all-renders ()
  "Every render record, newest first."
  (remove nil (mapcar (lambda (id) (ignore-errors (load-record id))) (render-ids))))

(defun latest-renders (count &key set)
  "The newest COUNT records, of SET's when one is named."
  (let* ((wanted (and set (plusp (length set)) set))
         (records (if wanted
                      (remove-if-not (lambda (record) (string= wanted (getf record :set)))
                                     (all-renders))
                      (all-renders))))
    (subseq records 0 (min count (length records)))))

(defun resolve-render (ref)
  "The render REF names -- an id, or `last' for the newest -- or a refusal."
  (let ((ref (string-downcase (nlk:trimmed (or ref "")))))
    (cond
      ((member ref '("" "last") :test #'string=)
       (or (car (all-renders)) (fail "no renders yet: draw one first")))
      ((uiop:string-prefix-p "r" ref) (load-record ref))
      (t (fail "~s is no render id: /procgen-studio list shows them, `last' is the newest" ref)))))

;;; --- the active set, and where a render is published ----------------------------------

(defun active-set ()
  "The set new renders join, or \"\"."
  (or (nlk:read-text (merge-pathnames "set" (data-dir))) ""))

(defun (setf active-set) (name)
  "Group the next renders under NAME, or clear it with NIL."
  (ensure-directories-exist (data-dir))
  (with-open-file (out (merge-pathnames "set" (data-dir))
                        :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string (or name "") out))
  name)

(defun publish (record)
  "Copy RECORD into the section's output-dir, when one is set, and answer the
image path a person should open."
  (let ((folder (ignore-errors (setting :output-dir))))
    (if (and (stringp folder) (plusp (length folder)))
        (let* ((dir (uiop:ensure-directory-pathname
                     (if (uiop:absolute-pathname-p (uiop:ensure-pathname folder))
                         folder
                         (merge-pathnames folder (default-directory)))))
               (id (getf record :id))
               (image (merge-pathnames (format nil "~a.png" id) dir))
               (source (merge-pathnames (format nil "~a~a" id (pathname-type (getf record :source)))
                                         (uiop:ensure-directory-pathname dir))))
          (declare (ignore source))
          (ensure-directories-exist image)
          (uiop:copy-file (getf record :image) image)
          (uiop:copy-file (getf record :source)
                          (merge-pathnames (format nil "~a.~a" id (pathname-type (getf record :source))) dir))
          (uiop:copy-file (merge-pathnames "meta.json" (getf record :dir))
                          (merge-pathnames (format nil "~a.procgen.json" id) dir))
          (namestring image))
        (namestring (getf record :image)))))
