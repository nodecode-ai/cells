;;;; cell.lisp --- the manual, the section, the fifteen tools, /yadisk.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Config, a sibling top-level key next to `websearch':
;;;;   "yandex-disk": {"token": "...", "token_env": "YANDEX_DISK_OAUTH_TOKEN",
;;;;                   "root": "/Backup", "actions": ["read", "write"],
;;;;                   "timeout_seconds": 30}
;;;; The token is the one member no call can work without, pasted or named by
;;;; a variable -- as the original read YANDEX_DISK_OAUTH_TOKEN from the
;;;; environment -- and a section naming neither refuses the start.

(in-package #:nodecode-yandex-disk)

;;; --- the manual --------------------------------------------------------------
;;; (help :yandex-disk) answers it while the cell runs; a request carries the
;;; one line the :HELP clause below gives, never this text.

(defparameter *manual*
  "Yandex Disk is available through the nodecode-yandex-disk cell: fifteen model
tools, and the same calls in the YADISK package from eval. Every path is a Disk
path -- /Documents/report.pdf, or disk:/Documents/report.pdf -- and a path a
tool answers can be passed straight back to another. When the section sets root,
paths are relative to that folder and nothing outside it can be reached.
  yadisk_disk_info                                   quota, bin size, account
  yadisk_list        {path, limit, offset, sort}      a folder, or one file's metadata
  yadisk_search      {query, path, media_type, limit} hits by name or by contents
  yadisk_read_file   {path, encoding, max_bytes}      a text file into the conversation
  yadisk_download    {path, save_to, overwrite}       a file onto this machine
  yadisk_upload      {path, local_path|url, overwrite} a file from here, or a public link
  yadisk_write_file  {path, content, encoding, overwrite}
  yadisk_mkdir       {path}                           the folder and its parents
  yadisk_copy        {source, destination, overwrite}  on Yandex's side
  yadisk_move        {source, destination, overwrite}  a rename is a move
  yadisk_publish     {path, unpublish}                a public link, or its removal
  yadisk_delete      {path, permanently}              to the bin, or gone
  yadisk_trash_list  {limit, offset}                  the bin, and where each item came from
  yadisk_trash_restore {path, name, overwrite}        back to where it was deleted from
  yadisk_trash_empty {path}                           destroy one item, or the whole bin
Read before you write: yadisk_list answers every child's path, and yadisk_delete
takes a path rather than a guess. A delete without permanently is recoverable in
the bin; yadisk_trash_empty is not recoverable at all, so say what you are about
to destroy. yadisk_search may be refused for a token that is not permitted to
search -- every other tool still works, so list the folder instead.
ERROR: YANDEX-DISK-ERROR names the problem in Yandex's own words."
  "What (help :yandex-disk) answers while the cell runs.")

;;; --- the actions the section permits ------------------------------------------

(defparameter +all-actions+
  '("disk_info" "list" "search" "read_file" "trash_list" "download"
    "mkdir" "write_file" "upload" "copy" "move" "trash_restore"
    "publish" "delete" "trash_empty")
  "Every action a tool takes, as the original listed them.")

(defparameter +action-groups+
  '(("all" . ("disk_info" "list" "search" "read_file" "trash_list" "download"
              "mkdir" "write_file" "upload" "copy" "move" "trash_restore"
              "publish" "delete" "trash_empty"))
    ("read" . ("disk_info" "list" "search" "read_file" "trash_list"))
    ("download" . ("download"))
    ("write" . ("mkdir" "write_file" "upload" "copy" "move" "trash_restore"))
    ("share" . ("publish"))
    ("delete" . ("delete" "trash_empty")))
  "The shorthand the original's ACTION_GROUPS carried: a deployment names a
group, or a single action, and nothing else is offered.")

(defparameter +tool-actions+
  '(("yadisk_disk_info" . "disk_info")
    ("yadisk_list" . "list")
    ("yadisk_search" . "search")
    ("yadisk_read_file" . "read_file")
    ("yadisk_trash_list" . "trash_list")
    ("yadisk_download" . "download")
    ("yadisk_mkdir" . "mkdir")
    ("yadisk_write_file" . "write_file")
    ("yadisk_upload" . "upload")
    ("yadisk_copy" . "copy")
    ("yadisk_move" . "move")
    ("yadisk_trash_restore" . "trash_restore")
    ("yadisk_publish" . "publish")
    ("yadisk_delete" . "delete")
    ("yadisk_trash_empty" . "trash_empty"))
  "Each tool and the action that gates it, so a withheld action withholds the
tool: a tool that was never registered cannot be talked into being called.")

;;; --- reading the section -------------------------------------------------------

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun read-actions (declared)
  "DECLARED -- the section's list of action names and groups -- as the
canonical action names it permits. An empty list permits everything, and an
unknown name is dropped rather than expanded, so a typo can only withhold a
tool, never grant one."
  (let ((names (remove-if-not #'stringp declared)))
    (if (null names)
        (copy-list +all-actions+)
        (let ((out '()))
          (dolist (item names (nreverse out))
            (let* ((trimmed (string-downcase (nlk:trimmed item)))
                   (spelled (substitute #\_ #\- trimmed))
                   (key (if (uiop:string-prefix-p "yadisk_" spelled)
                            (subseq spelled 7)
                            spelled))
                   (group (assoc key +action-groups+ :test #'string=)))
              (cond (group (dolist (action (cdr group) out)
                             (pushnew action out :test #'string=)))
                    ((member key +all-actions+ :test #'string=)
                     (pushnew key out :test #'string=)))))))))

(defun read-settings (values table)
  "The declared VALUES, with the token and the root read here, once: the
inline token, else the variable token_env names, else a refusal naming what is
missing. base_url must be https -- the token goes wherever it points."
  (declare (ignore table))
  (let* ((plist (copy-list values))
         (env (nonblank (getf values :token-env)))
         (token (or (nonblank (getf values :token))
                    (and env (nonblank (nle::credential-env env)))))
         (base (string-right-trim "/" (or (nonblank (getf values :base-url)) +api-base+))))
    (unless token
      (nlk:config-error "no OAuth token: paste a Yandex Disk OAuth token into token, or name ~
the variable holding it in token_env~@[ (~a is unset or empty)~]. A token is made at ~
https://yandex.ru/dev/disk/poligon/ with the scopes cloud_api:disk.info, ~
cloud_api:disk.read and cloud_api:disk.write"
                        env))
    (unless (and (uiop:string-prefix-p "https://" base) (plusp (length base)))
      (nlk:config-error "base_url must be an https:// URL; unset it to use Yandex Disk's own ~
API at ~a" +api-base+))
    (setf (getf plist :token) token
          (getf plist :base-url) base
          (getf plist :root) (normalize-root (getf values :root))
          (getf plist :actions) (read-actions (getf values :actions)))
    plist))

(defun action-allowed-p (action)
  "Whether the section's actions permits ACTION."
  (and (member action (setting :actions) :test #'string=) t))

(defun allow (action)
  "Refuse unless the section permits ACTION -- the second half of the gate the
original applied at registration, in case the tools were installed anyway."
  (unless (action-allowed-p action)
    (fail "~a is not enabled in this install: the yandex-disk section's actions does not list it."
          action))
  t)

(defun filter-actions ()
  "Withhold the tools the section's actions does not list, so a read-only
install never shows a write tool -- what the original did at registration
rather than at call time."
  (dolist (entry +tool-actions+)
    (unless (action-allowed-p (cdr entry))
      (nle:unregister-tool (car entry)))))

;;; --- reading what Yandex answered ---------------------------------------------

(defun field (item name)
  "ITEM's member NAME, or NIL: a decoded resource is a hash table and a
missing member reads exactly like a null one."
  (and (hash-table-p item) (nlk:json-value item :any name)))

(defun item-path (item)
  "ITEM's path as the model is shown it, the sandbox prefix hidden."
  (display-path (or (field item "path") "") (setting :root)))

(defun item-kind (item)
  "ITEM as `dir' or `file', with the count or the size that makes it
actionable -- what the original's _shape kept of a resource."
  (let ((kind (field item "type"))
        (size (field item "size"))
        (count (nlk:json-value item :integer "_embedded" "total")))
    (cond ((equal kind "dir")
           (if (and (integerp count) (plusp count))
               (format nil "dir, ~:d item~:p" count)
               "dir"))
          ((and (integerp size) (plusp size)) (format nil "file, ~a" (nlk:size-text size)))
          (t "file"))))

(defun item-extras (item)
  "The fields that change what a resource can do, as one clause each."
  (let ((extras '()))
    (macrolet ((note (value format &rest arguments)
                 `(let ((text ,value))
                    (when (and text (stringp text) (plusp (length text)))
                      (push (format nil ,format ,@arguments) extras)))))
      (note (field item "modified") "modified ~a" (field item "modified"))
      (note (field item "mime_type") "~a" (field item "mime_type"))
      (note (field item "md5") "md5 ~a" (field item "md5"))
      (note (field item "search_scope") "matched in ~a" (field item "search_scope"))
      (note (field item "public_url") "public: ~a" (field item "public_url"))
      (note (field item "deleted") "deleted ~a" (field item "deleted"))
      (note (field item "origin_path") "from ~a"
            (display-path (field item "origin_path") (setting :root))))
    (nreverse extras)))

(defun item-line (item &key (indent "  "))
  "One resource as a line a model reads: its path, what it is, and the few
fields that matter. The path comes first, so it can be quoted back."
  (format nil "~a~a -- ~a~@[; ~{~a~^; ~}~]"
          indent (item-path item) (item-kind item) (item-extras item)))

(defun items-of (payload)
  "The children PAYLOAD embeds, as a list of hash tables."
  (let ((items (nlk:json-array payload "_embedded" "items")))
    (if (vectorp items)
        (remove-if-not #'hash-table-p (coerce items 'list))
        '())))

(defun clamp (value default maximum &key (minimum 1))
  "VALUE as an integer between MINIMUM and MAXIMUM, DEFAULT when it is absent
or not a number -- as the original's _clamp bounded a page."
  (let ((number (if (integerp value) value default)))
    (max minimum (min number maximum))))



;;; --- where a path lands on this machine ---------------------------------------

(defparameter +encodings+
  '(("utf-8" . :utf-8) ("utf8" . :utf-8)
    ("ascii" . :ascii) ("us-ascii" . :ascii)
    ("latin-1" . :latin-1) ("latin1" . :latin-1) ("iso-8859-1" . :latin-1)
    ("cp1251" . :cp1251) ("windows-1251" . :cp1251)
    ("koi8-r" . :koi8-r) ("koi8r" . :koi8-r))
  "The encodings a text file is read or written in, by the name a caller says.")

(defun external-format-of (name)
  "NAME -- an encoding a caller named -- as an external format."
  (let* ((key (string-downcase (or (nonblank name) "utf-8")))
         (entry (assoc key +encodings+ :test #'string=)))
    (or (cdr entry)
        (fail "~a is not an encoding this cell reads or writes: use utf-8, ascii, latin-1, cp1251 or koi8-r."
              name))))

(defun expand-local (path)
  "PATH on this machine, its leading ~ expanded."
  (nlk:expand-home (nlk:trimmed (or path ""))))

(defun write-octets (path octets)
  "OCTETS written at PATH, the folders above it made first."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
    (write-sequence octets stream))
  (length octets))

(defun read-octets (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

;;; --- read ---------------------------------------------------------------------

(defun disk-info ()
  "The account's quota and owner, as one answer a model reads."
  (allow "disk_info")
  (let* ((info (account-info))
         (total (field info "total_space"))
         (used (field info "used_space"))
         (trash (field info "trash_size"))
         (largest (field info "max_file_size"))
         (user (nlk:json-value info :string "user" "login")))
    (with-output-to-string (out)
      (format out "Yandex Disk: ~a of ~a used, ~a free~%"
              (nlk:size-text used) (nlk:size-text total)
              (nlk:size-text (max 0 (- (or total 0) (or used 0)))))
      (format out "  bin: ~a; largest file: ~a; account: ~a~@[ (~a)~]~%"
              (nlk:size-text trash) (nlk:size-text largest)
              (or user "unknown")
              (if (eq t (field info "is_paid")) "paid disk" "free tier"))
      (let ((root (setting :root)))
        (format out "  root: ~a~@[ (every path below is relative to it)~]~%"
                (if (zerop (length root)) "disk:/" (format nil "disk:~a" root))
                (plusp (length root)))))))

(defun list-path (path &key limit offset sort)
  "A folder's children, or one file's metadata, as an answer a model reads."
  (allow "list")
  (let* ((root (setting :root))
         (absolute (resolve-path path :root root))
         (payload (get-meta absolute
                            :limit (clamp limit 50 1000)
                            :offset (and offset (clamp offset 0 100000 :minimum 0))
                            :sort (nonblank sort))))
    (if (equal (field payload "type") "dir")
        (with-output-to-string (out)
          (let ((items (items-of payload))
                (total (nlk:json-value payload :integer "_embedded" "total")))
            (format out "~a -- ~a~%" (display-path absolute root) (item-kind payload))
            (dolist (item items) (format out "~a~%" (item-line item)))
            (when (and (integerp total) (> total (length items)))
              (format out "  ~:d entr~:@p in the folder; page on with offset~%" total))))
        (format nil "~a~%" (item-line payload :indent "")))))

(defparameter +search-scan+ 1000
  "How many server hits a confined search scans to fill a page, as the
original's DEFAULT_SEARCH_SCAN bounded it.")

(defun under-prefix-p (prefix item)
  (let ((path (or (field item "path") "")))
    (or (zerop (length prefix))
        (string= path prefix)
        (uiop:string-prefix-p (concatenate 'string prefix "/") path))))

(defun confined-search (query media-type prefix limit)
  "Page the server-side search, keeping only hits under PREFIX: the endpoint
ignores any folder scope, so confinement is filtered here."
  (let ((found '()) (scanned 0) (page 100) (done nil))
    (loop while (and (not done) (< (length found) limit) (< scanned +search-scan+))
          do (let ((batch (search-resources query :limit page :offset scanned :media-type media-type)))
               (if (null batch)
                   (setf done t)
                   (progn (incf scanned (length batch))
                          (dolist (item batch) (when (under-prefix-p prefix item) (push item found)))
                          (when (< (length batch) page) (setf done t))))))
    (values (subseq (nreverse found) 0 (min limit (length found)))
            (and (< (length found) limit) (>= scanned +search-scan+)))))

(defun search-file (query &key path media-type limit)
  "Hits by file name or by contents, as an answer a model reads."
  (allow "search")
  (let* ((root (setting :root))
         (wanted (clamp limit 20 100))
         (prefix (cond ((nonblank path) (resolve-path path :root root))
                       ((plusp (length root)) (format nil "disk:~a" root))
                       (t ""))))
    (handler-case
        (multiple-value-bind (hits truncated)
            (if (plusp (length prefix))
                (confined-search query (nonblank media-type) prefix wanted)
                (values (let ((hits (search-resources query :limit wanted
                                                               :media-type (nonblank media-type))))
                          (subseq hits 0 (min wanted (length hits))))
                        nil))
          (declare (ignorable truncated))
          (with-output-to-string (out)
            (format out "search ~s~@[ under ~a~] -- ~:d hit~:p~%"
                    query (and (plusp (length prefix)) (display-path prefix root)) (length hits))
            (dolist (item hits) (format out "~a~%" (item-line item)))
            (when truncated
              (format out "  more hits were scanned than shown; narrow the query~%"))))
      (yandex-disk-error (condition)
        (if (cl:search "HTTP 403" (princ-to-string condition))
            (format nil "the token is not permitted to search Yandex Disk; every other tool ~
still works, so list the folder instead")
            (error condition))))))

;;; --- read a text file ------------------------------------------------------------

(defun read-file (path &key encoding max-bytes)
  "A text file's bytes decoded into the conversation."
  (allow "read_file")
  (let* ((root (setting :root))
         (absolute (resolve-path path :root root))
         (cap (clamp max-bytes (setting :max-read-bytes) (setting :max-read-bytes)))
         (format (external-format-of encoding))
         (octets (download-octets absolute :max-bytes cap))
         (text (handler-case (sb-ext:octets-to-string octets :external-format format)
                 (error () nil))))
    (unless text
      (fail "~a is not decodable as ~a text. Use yadisk_download to fetch it as a file instead."
            (display-path absolute root) (or (nonblank encoding) "utf-8")))
    (format nil "~a -- ~a~%~a"
            (display-path absolute root) (nlk:size-text (length octets)) text)))

(defun write-file (path content &key encoding overwrite)
  "TEXT written to PATH, the folders above it made first."
  (allow "write_file")
  (let* ((root (setting :root))
         (absolute (resolve-path path :root root))
         (octets (sb-ext:string-to-octets (or content "") :external-format (external-format-of encoding))))
    (ensure-folder (parent-of absolute))
    (upload-bytes absolute octets :overwrite overwrite)
    (format nil "~a -- ~a written" (display-path absolute root) (nlk:size-text (length octets)))))

(defun mkdir (path)
  "PATH and every missing folder above it."
  (allow "mkdir")
  (let* ((root (setting :root))
         (absolute (resolve-path path :root root))
         (made (ensure-folder absolute)))
    (if made
        (format nil "~a -- created, with ~{~a~^, ~}" (display-path absolute root)
                (mapcar (lambda (one) (display-path one root)) made))
        (format nil "~a -- already existed" (display-path absolute root)))))

;;; --- transfers in and out ---------------------------------------------------------

(defun target-file (save-to name)
  "Where a download lands: SAVE-TO itself, or NAME inside it when SAVE-TO
names a folder."
  (let* ((text (nlk:trimmed (or save-to "")))
         (path (expand-local text))
         (folder (and (plusp (length text))
                      (or (uiop:string-suffix-p text "/")
                          (and (probe-file path)
                               (uiop:directory-pathname-p (probe-file path)))))))
    (if folder
        (merge-pathnames name (uiop:ensure-directory-pathname path))
        (pathname path))))

(defun download (path save-to &key overwrite)
  "A file from the disk onto this machine; it lands complete or not at all."
  (allow "download")
  (let* ((root (setting :root))
         (absolute (resolve-path path :root root))
         (meta (get-meta absolute :limit 0 :fields "name,type,size"))
         (target (target-file save-to (or (field meta "name") "download")))
         (staging (pathname (format nil "~a.part" (namestring target)))))
    (when (and (probe-file target) (not overwrite))
      (fail "~a already exists; pass overwrite=true to replace it." (namestring target)))
    (let ((octets (download-octets absolute :max-bytes (setting :max-download-bytes))))
      (write-octets staging octets)
      (uiop:rename-file-overwriting-target staging target)
      (format nil "~a -- ~a saved to ~a"
              (display-path absolute root) (nlk:size-text (length octets))
              (namestring target)))))

(defun upload (path &key local-path url overwrite)
  "A file from this machine, or a public link Yandex fetches itself."
  (allow "upload")
  (let* ((root (setting :root))
         (absolute (resolve-path path :root root))
         (local (nonblank local-path))
         (remote (nonblank url)))
    (when (eq (and local t) (and remote t))
      (fail "Give exactly one of local_path and url."))
    (ensure-folder (parent-of absolute))
    (cond
      (local
       (let ((source (expand-local local)))
         (unless (probe-file source)
           (fail "~a is not a file on this machine." source))
         (let ((octets (read-octets source)))
           (upload-bytes absolute octets :overwrite overwrite)
           (format nil "~a -- ~a uploaded from ~a"
                   (display-path absolute root) (nlk:size-text (length octets)) source))))
      (remote
       (let ((replacing (folder-exists-p absolute)))
         (when (and replacing (not overwrite))
           (fail "~a already exists; pass overwrite=true to replace it."
                 (display-path absolute root)))
         (if replacing
             (let ((staging (staging-path absolute)))
               (handler-case
                   (progn (upload-from-url staging remote)
                          (transfer-resource "move" staging absolute :overwrite t))
                 (error (condition)
                   (ignore-errors (delete-resource staging :permanently t))
                   (error condition)))
               (format nil "~a -- replaced from ~a" (display-path absolute root) remote))
             (progn
               (upload-from-url absolute remote)
               (format nil "~a -- fetched from ~a" (display-path absolute root) remote)))))
      (t (fail "Give exactly one of local_path and url.")))))

(defun copy (source destination &key overwrite)
  "A copy made on Yandex's side."
  (allow "copy")
  (let* ((root (setting :root))
         (from (resolve-path source :root root))
         (to (resolve-path destination :root root)))
    (transfer-resource "copy" from to :overwrite overwrite)
    (format nil "~a -- copied to ~a" (display-path from root) (display-path to root))))

(defun move (source destination &key overwrite)
  "A move, which is how a rename is spelled."
  (allow "move")
  (let* ((root (setting :root))
         (from (resolve-path source :root root))
         (to (resolve-path destination :root root)))
    (transfer-resource "move" from to :overwrite overwrite)
    (format nil "~a -- moved to ~a" (display-path from root) (display-path to root))))

(defun delete (path &key permanently)
  "To the bin, or gone."
  (allow "delete")
  (let* ((root (setting :root))
         (absolute (resolve-path path :root root)))
    (delete-resource absolute :permanently permanently)
    (format nil "~a -- ~a"
            (display-path absolute root)
            (if permanently
                "deleted permanently; nothing can restore it"
                "moved to the bin; yadisk_trash_restore can bring it back"))))

(defun publish (path &key unpublish)
  "A public link, or its removal."
  (allow "publish")
  (let* ((root (setting :root))
         (absolute (resolve-path path :root root)))
    (if unpublish
        (progn (unpublish-resource absolute)
               (format nil "~a -- unpublished; the link no longer opens" (display-path absolute root)))
        (let ((meta (publish-resource absolute)))
          (format nil "~a -- published: ~a~@[ (public key ~a)~]"
                  (display-path absolute root)
                  (or (field meta "public_url") "(Yandex answered no link)")
                  (field meta "public_key"))))))

;;; --- the bin ---------------------------------------------------------------------

(defun trash-target (path root)
  "PATH as a trash:/ address, refusing one whose origin lies outside the root."
  (let ((absolute (resolve-path path :root "" :scheme +trash-scheme+)))
    (when (plusp (length root))
      (let ((origin (or (field (trash-meta absolute) "origin_path") "")))
        (unless (inside-root-p origin root)
          (fail "~a was deleted from ~a, which is outside the configured root disk:~a."
                absolute origin root))))
    absolute))

(defun trash-list (&key limit offset)
  "The bin, newest first, with where each item came from."
  (allow "trash_list")
  (let* ((root (setting :root))
         (payload (trash-page :limit (clamp limit 50 1000)
                              :offset (and offset (clamp offset 0 100000 :minimum 0))))
         (items (remove-if-not (lambda (item) (inside-root-p (or (field item "origin_path") "") root))
                               (items-of payload))))
    (with-output-to-string (out)
      (format out "the bin -- ~:d item~:p~%" (length items))
      (dolist (item items) (format out "~a~%" (item-line item)))
      (when (null items) (format out "  nothing in it"))
      (when (plusp (length root))
        (format out "  (only items deleted from disk:~a and below)~%" root)))))

(defun trash-restore (path &key name overwrite)
  "An item back where it was deleted from."
  (allow "trash_restore")
  (let* ((root (setting :root))
         (absolute (trash-target path root)))
    (restore-resource absolute :name (nonblank name) :overwrite overwrite)
    (format nil "~a -- restored~@[ as ~a~]" absolute (nonblank name))))

(defun trash-empty (&key path)
  "One bin item destroyed, or the whole bin."
  (allow "trash_empty")
  (let* ((root (setting :root))
         (given (nonblank path)))
    (cond
      ((null given)
       (when (plusp (length root))
         (fail "root is set to disk:~a, so the whole bin cannot be emptied from here: it holds ~
items deleted from outside that folder. Pass one path from yadisk_trash_list instead." root))
       (destroy-trash)
       "the bin -- emptied; nothing in it can be restored now")
      (t (let ((absolute (trash-target given root)))
           (destroy-trash absolute)
           (format nil "~a -- destroyed permanently; it cannot be restored now" absolute))))))

;;; --- what a model calls ------------------------------------------------------------

(defun disk-info-tool (request)
  (declare (ignore request))
  (disk-info))

(defun list-tool (request)
  (list-path (list-request-path request)
             :limit (list-request-limit request)
             :offset (list-request-offset request)
             :sort (list-request-sort-order request)))

(defun search-tool (request)
  (search-file (search-request-query request)
               :path (search-request-path request)
               :media-type (search-request-media-type request)
               :limit (search-request-limit request)))

(defun read-tool (request)
  (read-file (read-file-request-path request)
             :encoding (read-file-request-encoding request)
             :max-bytes (read-file-request-max-bytes request)))

(defun download-tool (request)
  (download (download-request-path request)
            (download-request-save-to request)
            :overwrite (download-request-overwrite request)))

(defun mkdir-tool (request)
  (mkdir (mkdir-request-path request)))

(defun write-tool (request)
  (write-file (write-file-request-path request)
              (write-file-request-content request)
              :encoding (write-file-request-encoding request)
              :overwrite (write-file-request-overwrite request)))

(defun upload-tool (request)
  (upload (upload-request-path request)
          :local-path (upload-request-local-path request)
          :url (upload-request-url request)
          :overwrite (upload-request-overwrite request)))

(defun copy-tool (request)
  (copy (copy-request-source request)
        (copy-request-destination request)
        :overwrite (copy-request-overwrite request)))

(defun move-tool (request)
  (move (move-request-source request)
        (move-request-destination request)
        :overwrite (move-request-overwrite request)))

(defun delete-tool (request)
  (delete (delete-request-path request)
          :permanently (delete-request-permanently request)))

(defun publish-tool (request)
  (publish (publish-request-path request)
           :unpublish (publish-request-unpublish request)))

(defun trash-list-tool (request)
  (trash-list :limit (trash-list-request-limit request)
              :offset (trash-list-request-offset request)))

(defun trash-restore-tool (request)
  (trash-restore (trash-restore-request-path request)
                 :name (trash-restore-request-name request)
                 :overwrite (trash-restore-request-overwrite request)))

(defun trash-empty-tool (request)
  (trash-empty :path (trash-empty-request-path request)))

;;; --- the operator's own --------------------------------------------------------------

(defun run-slash (args session-id)
  "One /yadisk invocation: a path lists that folder, no argument answers the
disk's quota."
  (declare (ignore session-id))
  (let ((path (nlk:trimmed (or args ""))))
    (if (zerop (length path))
        (disk-info)
        (list-path path :limit 50))))

;;; --- the declaration ----------------------------------------------------------------

(nle:define-cell yandex-disk
  (:section ("yandex-disk")
    (:guide "make an OAuth token for Yandex Disk at https://yandex.ru/dev/disk/poligon/ with the scopes cloud_api:disk.info, cloud_api:disk.read and cloud_api:disk.write, then paste it into token or name the variable holding it in token_env; root confines every path the tools reach to one folder on the disk; actions lists what they may do (all, read, download, write, share, delete, or single names such as list, upload) and anything left out is withheld")
    ("token" :secret
             :doc "the Yandex Disk OAuth token, written here; left empty, the variable token_env names is read")
    ("token_env" :env :default "YANDEX_DISK_OAUTH_TOKEN"
                 :doc "the environment variable holding the token, read when token is empty")
    ("root" :string :default ""
            :doc "confine every Disk path to this folder, e.g. /Backup; empty is the whole disk")
    ("actions" :list
               :doc "the actions the tools may take: all, read, download, write, share, delete, or single names such as list or upload; empty means all")
    ("base_url" :string :default "https://cloud-api.yandex.net/v1/disk"
                :doc "where the Yandex Disk API answers; change it only for a gateway of your own")
    ("timeout_seconds" :integer :default 30 :min 1
                       :doc "seconds one request may take")
    ("max_read_bytes" :integer :default 1048576 :min 1
                      :doc "the largest file yadisk_read_file reads, and the ceiling on a call's own max_bytes")
    ("max_download_bytes" :integer :default 268435456 :min 1
                          :doc "the largest file yadisk_download writes to this machine"))
  (:settings #'read-settings)
  (:tool "yadisk_disk_info"
         (list :description "Show the Yandex Disk account: total, used and bin space, the largest upload it takes, and the account login. Use it to answer how much space is left on my disk."
               :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)))
         #'disk-info-tool)
  (:tool "yadisk_list" (nlk:contract 'list-request) #'list-tool)
  (:tool "yadisk_search" (nlk:contract 'search-request) #'search-tool)
  (:tool "yadisk_read_file" (nlk:contract 'read-file-request) #'read-tool)
  (:tool "yadisk_download" (nlk:contract 'download-request) #'download-tool)
  (:tool "yadisk_mkdir" (nlk:contract 'mkdir-request) #'mkdir-tool)
  (:tool "yadisk_write_file" (nlk:contract 'write-file-request) #'write-tool)
  (:tool "yadisk_upload" (nlk:contract 'upload-request) #'upload-tool)
  (:tool "yadisk_copy" (nlk:contract 'copy-request) #'copy-tool)
  (:tool "yadisk_move" (nlk:contract 'move-request) #'move-tool)
  (:tool "yadisk_delete" (nlk:contract 'delete-request) #'delete-tool)
  (:tool "yadisk_publish" (nlk:contract 'publish-request) #'publish-tool)
  (:tool "yadisk_trash_list" (nlk:contract 'trash-list-request) #'trash-list-tool)
  (:tool "yadisk_trash_restore" (nlk:contract 'trash-restore-request) #'trash-restore-tool)
  (:tool "yadisk_trash_empty" (nlk:contract 'trash-empty-request) #'trash-empty-tool)
  (:command "yadisk" 'run-slash
            :description "Browse your Yandex Disk: a path lists that folder"
            :argument-hint "PATH")
  (:help :yandex-disk
   "yandex-disk: Yandex Disk as fifteen tools - list, search, read, upload, download, share, delete and restore, with your own OAuth token - (yadisk:list-path \"/\") from eval"
   *manual*)
  (:start (lambda () (filter-actions))))
