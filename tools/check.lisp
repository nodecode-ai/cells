;;;; check.lisp --- the one form `nodecode trial' evaluates for check.sh.
;;;;
;;;; The index is read the way every operator's /setup reads it
;;;; (NLK:READ-HUB-INDEX, which refuses a loose entry), then each entry this
;;;; platform runs is installed with what it stands on and loaded, in the
;;;; scratch home check.sh made. The install proves the clone: HEAD is the pin,
;;;; the primary system is the entry's name with the index's description and
;;;; depends_on, no symbolic link. Answers 1 when every entry loads, else 0; the
;;;; report goes to $HUB_CHECK_REPORT, because the trial silences a form's
;;;; own output.

(let ((report (open (uiop:getenv "HUB_CHECK_REPORT") :direction :output
                                                       :if-exists :supersede
                                                       :if-does-not-exist :create))
      (nlk::*addons-directory* (uiop:ensure-directory-pathname (uiop:getenv "HUB_CHECK_ADDONS")))
      (failed nil))
  (flet ((fail (control &rest arguments)
           (setf failed t)
           (format report "FAIL ~?~%" control arguments)))
    (unwind-protect
         (handler-case
             (let* ((text (uiop:read-file-string (uiop:getenv "HUB_CHECK_INDEX")))
                    (items (coerce (nlk:json-array (nlk:decode-json text) "addons") 'list))
                    (names (mapcar (lambda (item) (nlk:json-value item :string "name")) items))
                    (offers (nlk:read-hub-index text))
                    (all (append (nlk:shipped-addons) offers)))
               (unless (equal names (sort (remove-duplicates (copy-list names) :test #'equal) #'string<))
                 (fail "index: entries are sorted by name, each name once"))
               (dolist (item items)
                 (let ((name (nlk:json-value item :string "name")))
                   (unless (nlk:json-value item :text "license")
                     (fail "~a: no license" name))
                   (unless (find name offers :key #'nlk:offer-name :test #'equal)
                     (format report "skip ~a: ~:[not offered on this platform~;this release ships it~]~%"
                             name (find name (nlk:shipped-addons) :key #'nlk:offer-name :test #'equal)))))
               (dolist (offer offers)
                 (let ((name (nlk:offer-name offer)))
                   (handler-case
                       (let ((closure (nlk:offer-closure (list name) all)))
                         (dolist (folder closure)
                           (nlk:install-addon folder :offers all))
                         (dolist (record (apply #'nlk:load-addon closure))
                           (unless (eq :loaded (nlk:addon-state record))
                             (fail "~a: ~a ~s" (nlk:addon-name record) (nlk:addon-state record)
                                   (nlk:addon-failures record))))
                         (format report "ok ~a at ~a~%" name (nlk:offer-commit offer)))
                     (error (condition) (fail "~a: ~a" name condition))))))
           (error (condition) (fail "index: ~a" condition)))
      (close report)))
  (if failed 0 1))
