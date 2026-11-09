;;;; data.lisp --- the shipped reference guides and the style atlas, read back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin carries references/*.md and data/atlas.json beside its
;;;; code and reads them on every call. The cell carries the same files under
;;;; data/, unchanged, and reads them from there: the atlas is decoded once and
;;;; kept, a guide is read when its topic is asked for. Nothing here reaches
;;;; outside the folder, and no path comes from the caller.

(in-package #:nodecode-hardlight)

(defparameter *data-directory*
  (merge-pathnames
   "data/"
   (or (ignore-errors (asdf:system-source-directory "nodecode-hardlight"))
       (and *load-truename* (make-pathname :name nil :type nil :defaults *load-truename*))
       *default-pathname-defaults*))
  "The cell's own data/: twelve reference guides, the style atlas and the platform ledger.")

(defun data-file (name)
  "The shipped file NAME, inside the cell's own data/."
  (merge-pathnames name *data-directory*))

(defun read-octets (path)
  "PATH's octets, whole."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence data in)
      data)))

(defun read-text (path)
  "PATH decoded as UTF-8, or NIL when it is not there or not UTF-8."
  (handler-case (sb-ext:octets-to-string (read-octets path) :external-format :utf-8)
    (error () nil)))

;;; --- the atlas ---------------------------------------------------------------

(defvar *atlas* nil
  "The style atlas, decoded once: schema_version, styles and sources.")

(defun atlas ()
  (or *atlas*
      (setf *atlas* (nlk:decode-json (read-octets (data-file "atlas.json")) :whole t))))

(defun atlas-styles () (gethash "styles" (atlas)))
(defun atlas-sources () (gethash "sources" (atlas)))

(defun style-ids ()
  "Every exact atlas ID, in the atlas's own order."
  (map 'list (lambda (style) (gethash "id" style)) (atlas-styles)))

(defun find-style (id)
  "The atlas style whose ID is ID, or NIL."
  (find id (atlas-styles) :test #'equal :key (lambda (style) (gethash "id" style))))

(defun style-sources (style)
  "The source records the style names, in atlas order, as a list."
  (let ((ids (gethash "source_ids" style)))
    (coerce (remove-if-not (lambda (source) (find (gethash "id" source) ids :test #'equal))
                           (atlas-sources))
            'list)))

;;; --- rendering a record as prose ------------------------------------------------

(defun json-line (value)
  "VALUE as one line of JSON, for a record that has no prose of its own."
  (with-output-to-string (out) (shasht:write-json value out)))

(defun write-field (out label value)
  "LABEL and VALUE as the guide and the packet both render a field: a string
on its line, a list as bullets, an object as JSON."
  (cond ((null value) nil)
        ((eq value :null) nil)
        ((stringp value) (format out "~a: ~a~%" label value))
        ((hash-table-p value) (format out "~a: ~a~%" label (json-line value)))
        ((vectorp value)
         (if (zerop (length value))
             nil
             (progn (format out "~a:~%" label)
                    (loop for item across value
                          do (format out "  - ~a~%" (if (stringp item) item (json-line item)))))))
        (t (format out "~a: ~a~%" label value))))

(defparameter +style-fields+
  '(("thesis" "thesis") ("composition" "composition") ("typography" "typography")
    ("palette" "palette") ("material" "material") ("motion" "motion")
    ("interaction" "interaction") ("mobile" "mobile") ("signature" "signature")
    ("use when" "use_when") ("avoid when" "avoid_when"))
  "The atlas fields a reference or a candidate renders, in reading order.")

(defun style-text (style)
  "STYLE as the grammar a designer reads: its head, its fields and its sources."
  (with-output-to-string (out)
    (format out "~a — ~a~%family: ~a~%surfaces: ~{~a~^, ~}~2%"
            (gethash "id" style) (gethash "name" style) (gethash "family" style)
            (coerce (gethash "surfaces" style) 'list))
    (loop for (label key) in +style-fields+
          do (write-field out label (gethash key style)))
    (let ((sources (style-sources style)))
      (when sources
        (format out "~%sources (reference grammar only; do not copy a source site):~%")
        (loop for source in sources
              do (format out "  - ~a — ~a~%" (gethash "title" source) (gethash "url" source)))))))

(defparameter +reference-topics+
  '("index" "atlas" "style" "workflow" "navigation" "states" "typography" "color"
    "motion" "review" "systems" "browser" "layout" "platform" "performance")
  "Every topic hardlight_reference answers, as the original plugin lists them.")

(defun index-text ()
  "What the cell can be asked for: its topics and every style ID."
  (with-output-to-string (out)
    (format out "Hardlight UI reference topics:~%  ~{~a~^, ~}~%" +reference-topics+)
    (format out "~%exact style IDs (hardlight_reference topic=style, style=ID):~%")
    (loop for style across (atlas-styles)
          do (format out "  ~a — ~a (~a)~%" (gethash "id" style) (gethash "name" style)
                      (gethash "family" style)))))

(defun reference-text (topic style)
  "The guide or grammar TOPIC names, with STYLE the exact atlas ID topic style
needs; (values TEXT NIL) or (values NIL WHY) when the ask cannot be answered."
  (cond ((not (member topic +reference-topics+ :test #'equal))
         (values nil (format nil "unknown topic; choose one of: ~{~a~^, ~}" +reference-topics+)))
        ((equal topic "index") (values (index-text) nil))
        ((equal topic "atlas")
         (values (with-output-to-string (out) (shasht:write-json (atlas) out)) nil))
        ((equal topic "style")
         (let ((found (and style (find-style style))))
           (cond (found (values (style-text found) nil))
                 ((or (null style) (zerop (length style)))
                  (values nil (format nil "topic style needs an exact style ID; choose one of: ~{~a~^, ~}"
                                      (style-ids))))
                 (t (values nil (format nil "unknown style ID; choose one of: ~{~a~^, ~}"
                                        (style-ids)))))))
        (t
         (let ((content (read-text (data-file (format nil "references/~a.md" topic)))))
           (if (null content)
               (values nil (format nil "the shipped guide for ~a is missing; reinstall the cell" topic))
               (values (cond ((equal topic "platform")
                              (format nil "# ~a~%~%~a~%## platform feature ledger~%~a~%"
                                      topic content
                                      (or (read-text (data-file "platform-features.json")) "")))
                             (t (format nil "# ~a~%~%~a" topic content)))
                       nil))))))
