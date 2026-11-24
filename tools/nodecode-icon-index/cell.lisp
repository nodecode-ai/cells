;;;; cell.lisp --- the icon library as three tools, one command and one manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from minutechreview's Hermes Agent plugin (see NOTICE). What the
;;;; original gave a user is kept whole: the same 40-icon catalog (data.lisp),
;;;; the same search weights over title, tags, category and description, the
;;;; same eight preset colours, the same four formats per icon, the same embed
;;;; snippets, a save that never overwrites a file, and /icons to list what
;;;; matches. Search and every URL are arithmetic over the bundled catalog, so
;;;; they need no network; only icon_save fetches, from the pinned CDN path the
;;;; section's asset_base names.

(in-package #:nodecode-icon-index)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Icon Index is a free library of 40 matching 3D icons in one glossy style, CC0: free for
any use, commercial included, no credit needed. Three tools read it, and a person can call
the same three from eval in the icon-index package.
  icon_search {query, category, limit}
      Icons by what they mean - 'security', 'payments', 'launch' - filtered by category
      (Tech, Money, Social, Work) and capped by limit. Reads the bundled catalog, so it
      works offline. The answer carries each icon's id, title, category, meaning and a
      preview PNG.
  icon_get {id, color}
      One icon's download links, embed code and licence. color is one of the eight presets
      (orange, blue, purple, green, pink, red, teal, yellow) or a hex like #0ea5e9; a hex
      works in the viewer iframe only, because the files exist in the presets. The answer
      carries embed.iframe (a 3D viewer you can paste anywhere), embed.markdown (shows the
      icon in this chat), embed.html (model-viewer), embed.react, embed.threejs and
      embed.img, plus every file URL: model.glb (web-ready, about 300 KB, meshopt), the
      uncompressed model-full.glb for Blender, Unity and Unreal, a 1024 px transparent
      icon.png and a 4-second turntable.mp4. Say where the files are; do not paste bytes.
  icon_save {id, folder, formats, color}
      The same files written into a folder of the user's project, created if missing. It
      never overwrites: a file already there is left untouched and the answer says so.
      Only the preset colours have files; for any other colour embed the viewer iframe
      from icon_get instead.
/formats: glb and png unless named; glb_full (Blender, Unity, Unreal) and mp4 exist in
orange only, and a non-orange colour has the GLB and the PNG. On a landing page use one
to three spinning icons and plain PNGs for the rest. Match a brand colour to the nearest
preset. /icons [words] lists what matches, from the operator's side."
  "What (help :icon-index) answers while the cell runs.")

;;; --- the library's fixed words --------------------------------------------------

(defparameter +presets+
  '(("orange" . "ff6200") ("blue" . "3b82f6") ("purple" . "8b5cf6")
    ("green" . "22c55e") ("pink" . "ec4899") ("red" . "ef4444")
    ("teal" . "14b8a6") ("yellow" . "facc15"))
  "The eight accent colours the icon files exist in. Orange is the library's
own accent, and the shipped files' colour.")

(defparameter +files+
  '(("glb" "model.glb"
     "Web-ready 3D model (GLB, meshopt and WebP, about 300 KB) for a website, three.js, model-viewer or React.")
    ("glb_full" "model-full.glb"
     "Full-quality uncompressed 3D model (GLB) for Blender, Unity, Unreal and Figma plugins.")
    ("png" "icon.png"
     "Transparent 1024x1024 PNG render for slides, docs and thumbnails.")
    ("mp4" "turntable.mp4"
     "4-second seamless spinning loop (512x512 MP4) for videos and reels."))
  "One row per format: the name, the file under the icon's folder, and what it
is for.")

(defparameter +colored-files+ '("glb" "png")
  "The formats that exist in the seven non-orange colours. Orange has all four.")

(defparameter +suffixes+ '(("glb" . ".glb") ("glb_full" . "-full.glb")
                            ("png" . ".png") ("mp4" . "-turntable.mp4"))
  "What a saved file is called after its stem, as the original named them.")

(defparameter +weights+ '(("title" . 5) ("tags" . 3) ("category" . 2) ("description" . 1))
  "How much a word in each field is worth in a search, as the original weighed
them.")

(defparameter +model-viewer+
  "https://cdn.jsdelivr.net/npm/@google/model-viewer@4.1.0/dist/model-viewer.min.js"
  "The model-viewer build the HTML and React snippets load, exactly as the
original named it. It is a script the user's own page loads, never one this
cell runs.")

(defparameter +user-agent+ "nodecode-icon-index/0.1.0"
  "What the cell calls itself to the CDN. Nothing of another client's is
impersonated.")

(defparameter +max-bytes+ (* 40 1024 1024)
  "The most one icon file may be, in bytes: the original's ceiling.")

(defparameter +max-limit+ 40
  "The most icons one search answers: the original's cap.")

(defparameter +error-chars+ 300
  "How much of an error's own text a refusal quotes back.")

;;; --- answers, and the one condition ---------------------------------------------

(defmacro answering (&body body)
  "BODY's JSON; a refusal becomes the tool call's failure, never an unhandled
error."
  `(handler-case (progn ,@body)
     (icon-index-error (condition)
       (nle:failure (format nil "ERROR: ~a" (princ-to-string condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: icon-index: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defmacro answering-text (&body body)
  "BODY's text; a refusal becomes a line, for a command or an eval verb."
  `(handler-case (progn ,@body)
     (icon-index-error (condition) (format nil "ERROR: ~a" (princ-to-string condition)))
     (error (condition)
       (format nil "ERROR: icon-index: ~a" (nlk:one-line (princ-to-string condition))))))

;;; --- reading the section --------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun url-parts (url)
  "(values SCHEME HOST USERINFO-P) of an absolute URL, both lowercased, or NILs."
  (let ((mark (and (stringp url) (search "://" url))))
    (when (and mark (plusp mark))
      (let* ((scheme (string-downcase (subseq url 0 mark)))
             (rest (subseq url (+ mark 3)))
             (end (or (position-if (lambda (character) (find character "/?#")) rest)
                      (length rest)))
             (authority (subseq rest 0 end))
             (at (position #\@ authority :from-end t))
             (host (if at (subseq authority (1+ at)) authority)))
        (values scheme (string-downcase host) (and at t))))))

(defun check-https-url (url name)
  "URL with its trailing slashes off, or a refusal naming NAME: both endpoints
this cell reads are https, and neither carries a credential in it."
  (let ((url (nlk:trimmed (or url ""))))
    (when (zerop (length url))
      (nlk:config-error "~a is empty: give the https URL it reads" name))
    (when (find-if (lambda (character)
                     (member character '(#\Space #\Tab #\Newline #\Return)))
                   url)
      (nlk:config-error "~a ~s holds whitespace" name url))
    (multiple-value-bind (scheme host userinfo) (url-parts url)
      (unless scheme
        (nlk:config-error "~a ~s is not an absolute URL: give the scheme too" name url))
      (unless (string= scheme "https")
        (nlk:config-error "~a ~s is ~a, not https: these are the library's own public files, so the URL is https" name url scheme))
      (when (or userinfo (find #\? url) (find #\# url))
        (nlk:config-error "~a ~s carries a query, a fragment or credentials: give the plain base URL" name url))
      (unless (plusp (length host))
        (nlk:config-error "~a ~s names no host" name url))
      (string-right-trim "/" url))))

(defun read-settings (values table)
  "The declared VALUES with both URLs checked and the asset base without its
trailing slashes: read once, here."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (setf (getf plist :asset-base)
          (check-https-url (getf values :asset-base) "asset_base")
          (getf plist :viewer-url)
          (check-https-url (getf values :viewer-url) "viewer_url"))
    plist))

;;; --- the catalog ----------------------------------------------------------------

(defun icon-id (icon)
  (getf icon :id))

(defun find-icon (id)
  "The catalog's entry for ID, ignoring case and space, or NIL."
  (let ((wanted (string-downcase (nlk:trimmed (princ-to-string (or id ""))))))
    (find wanted *catalog* :key #'icon-id :test #'string=)))

(defun categories ()
  "The catalog's categories, in the order it lists them."
  (let ((seen '()))
    (dolist (icon *catalog* (nreverse seen))
      (unless (member (getf icon :category) seen :test #'string-equal)
        (push (getf icon :category) seen)))))

(defun words-of (text)
  "TEXT's lowercase letter and digit runs: how a query and a field are read."
  (let ((words '())
        (start nil)
        (text (string-downcase (or text ""))))
    (flet ((cut (end)
             (when start
               (push (subseq text start end) words)
               (setf start nil))))
      (loop for index from 0 below (length text)
            for character = (char text index)
            do (if (or (digit-char-p character)
                       (<= (char-code #\a) (char-code character) (char-code #\z)))
                   (unless start (setf start index))
                   (cut index)))
      (cut (length text)))
    (nreverse words)))

(defun tag-text (icon)
  (format nil "~{~a~^ ~}" (getf icon :tags)))

(defun score-icon (icon query-words)
  "How well ICON answers QUERY-WORDS: the original's weights, a word counting
once per field, and a field token that starts with the word counting too."
  (let ((score 0))
    (dolist (row +weights+ score)
      (let ((tokens (words-of (if (string= (car row) "tags")
                                   (tag-text icon)
                                   (getf icon (intern (string-upcase (car row)) :keyword))))))
        (dolist (word query-words)
          (when (some (lambda (token)
                        (or (string= token word) (uiop:string-prefix-p word token)))
                      tokens)
            (incf score (cdr row))))))))

(defun search-icons (query &key category (limit 8))
  "The icons QUERY means, in CATEGORY when one is named, at most LIMIT."
  (let* ((query-words (remove-if (lambda (word) (< (length word) 2)) (words-of query)))
         (pool (if (nonblank category)
                   (remove-if-not (lambda (icon)
                                    (string-equal (getf icon :category) (nlk:trimmed category)))
                                  *catalog*)
                   *catalog*))
         (limit (max 1 (min +max-limit+ (or limit 8)))))
    (if (null query-words)
        (subseq pool 0 (min limit (length pool)))
        (let ((scored (loop for icon in pool
                            for score = (score-icon icon query-words)
                            when (plusp score)
                              collect (cons score icon))))
          (mapcar #'cdr
                  (subseq (stable-sort scored #'> :key #'car)
                          0 (min limit (length scored))))))))

(defun suggest (id)
  "Up to four ids a mistyped ID may have meant: a close spelling, or the
icons the words it holds find."
  (let ((near (list)))
    (dolist (icon *catalog*)
      (let ((icon-id (icon-id icon)))
        (when (or (uiop:string-prefix-p (string-downcase id) icon-id)
                  (uiop:string-prefix-p icon-id (string-downcase id))
                  (and (>= (length icon-id) 3) (>= (length (string-downcase id)) 3)
                       (string= (subseq icon-id 0 3)
                                (subseq (string-downcase id) 0 3))))
          (pushnew icon-id near :test #'string=))))
    (dolist (icon (search-icons id :limit 3))
      (pushnew (icon-id icon) near :test #'string=))
    (subseq near 0 (min 4 (length near)))))

(defun require-icon (id)
  "The entry for ID, or a refusal that names what it may have meant."
  (or (find-icon id)
      (fail "no icon with id ~s~@[; did you mean ~{~a~^, ~}~]? icon_search finds one by what it should mean"
            (princ-to-string (or id "")) (suggest (nlk:trimmed (princ-to-string (or id "")))))))

;;; --- colours --------------------------------------------------------------------

(defun hex-string-p (text)
  (and (= 6 (length text)) (every (lambda (character) (digit-char-p character 16)) text)))

(defun parse-color (text)
  "(values NAME HEX): a preset's name and code, or \"\" and the hex of a
custom colour. A name is what the files exist in."
  (let* ((text (string-downcase (nlk:trimmed (or text "orange"))))
         (text (if (and (plusp (length text)) (char= #\# (char text 0)))
                   (subseq text 1)
                   text)))
    (cond ((assoc text +presets+ :test #'string=)
           (values text (cdr (assoc text +presets+ :test #'string=))))
          ((hex-string-p text)
           (let ((preset (rassoc text +presets+ :test #'string=)))
             (values (if preset (car preset) "") text)))
          (t (fail "colour ~s is neither a preset (~{~a~^, ~}) nor a hex like #3b82f6"
                   text (mapcar #'car +presets+))))))

(defun orange-hex ()
  (cdr (assoc "orange" +presets+ :test #'string=)))

;;; --- URLs, and the answer shapes ------------------------------------------------

(defun file-url (id kind color)
  "The URL of KIND for ID, in COLOR's folder when that colour has the format."
  (let ((entry (or (assoc kind +files+ :test #'string=)
                   (fail "unknown format ~s: ~{~a~^, ~}" kind (mapcar #'car +files+))))
        (tinted (and (not (string= color "orange"))
                     (member kind +colored-files+ :test #'string=))))
    (format nil "~a/icons/~a~@[/colors/~a~]/~a"
            (setting :asset-base) id (and tinted color) (second entry))))

(defun viewer-url (id hex)
  "The interactive 3D viewer for ID at HEX, which spins, recolours and offers a
GLB download. A page in a frame or an iframe, never code this cell runs."
  (format nil "~a#id=~a~@[&color=~a~]"
          (setting :viewer-url) id
          (and hex (not (string= hex (orange-hex))) hex)))

(defun json-array (list)
  (coerce list 'vector))

(defun license-object ()
  (nlk:json-object "name" (getf *icon-license* :name)
                    "summary" (getf *icon-license* :summary)
                    "url" (getf *icon-license* :url)))

(defun icon-summary (icon)
  "One row of a search answer."
  (nlk:json-object "id" (icon-id icon)
                    "title" (getf icon :title)
                    "category" (getf icon :category)
                    "description" (getf icon :description)
                    "png" (file-url (icon-id icon) "png" "orange")))

(defun icon-detail (icon color hex)
  "Everything icon_get answers about ICON in COLOR."
  (let* ((id (icon-id icon))
         (alt (format nil "3D ~a icon" (string-downcase (getf icon :title))))
         (tinted (if (plusp (length color)) color "orange"))
         (kinds (if (string= tinted "orange")
                    (mapcar #'car +files+)
                    +colored-files+))
         (files (nlk:json-object))
         (answer (nlk:json-object
                  "id" id
                  "title" (getf icon :title)
                  "category" (getf icon :category)
                  "description" (getf icon :description)
                  "tags" (json-array (getf icon :tags))
                  "png" (file-url id "png" tinted)
                  "viewer_url" (viewer-url id hex)
                  "preset_colors" (json-array (mapcar #'car +presets+))
                  "license" (license-object))))
    (dolist (kind kinds)
      (setf (gethash kind files)
            (nlk:json-object "url" (file-url id kind tinted)
                              "what" (third (assoc kind +files+ :test #'string=)))))
    (setf (gethash "files" answer) files
          (gethash "color" answer) (nlk:json-object
                                     "name" (if (plusp (length color)) color "custom")
                                     "hex" (format nil "#~a" hex))
          (gethash "embed" answer) (embed-code icon color hex alt))
    answer))

(defun embed-code (icon color hex alt)
  "Ready-to-paste snippets. A preset colour points at real files; another hex
only the viewer can render."
  (let* ((id (icon-id icon))
         (name (format nil "~{~a~}"
                       (mapcar #'string-capitalize
                               (uiop:split-string id :separator '(#\-)))))
         (viewer (viewer-url id hex))
         (code (nlk:json-object
                "iframe" (format nil "<iframe src=\"~a\" title=\"~a\" width=\"320\" height=\"320\" style=\"border:0;background:transparent\" loading=\"lazy\"></iframe>"
                                  viewer alt)
                "markdown" (format nil "![~a](~a)" alt (file-url id "png" (if (plusp (length color)) color "orange"))))))
    (when (plusp (length color))
      (let* ((glb (file-url id "glb" color))
             (png (file-url id "png" color))
             (body (format nil "<model-viewer src=\"~a\" poster=\"~a\" alt=\"~a\" auto-rotate camera-controls disable-zoom interaction-prompt=\"none\" shadow-intensity=\"0\" exposure=\"1.1\""
                            glb png alt))
             (tag (format nil "~a style=\"width:320px;height:320px;background:transparent\"></model-viewer>" body))
             (script (format nil "<script type=\"module\" src=\"~a\"></script>"
                             +model-viewer+)))
        (setf (gethash "html" code) (format nil "~a~%~a" script tag)
              (gethash "react" code)
              (format nil "// once, in index.html: ~a~%export function ~aIcon() {~%  return (~a style={{ width: 320, height: 320 }}></model-viewer>)~%}"
                      script name body)
              (gethash "threejs" code)
              (format nil "import { GLTFLoader } from 'three/addons/loaders/GLTFLoader.js'~%import { MeshoptDecoder } from 'three/addons/libs/meshopt_decoder.module.js'~%const loader = new GLTFLoader().setMeshoptDecoder(MeshoptDecoder)~%loader.load('~a', gltf => scene.add(gltf.scene))"
                      glb)
              (gethash "img" code)
              (format nil "<img src=\"~a\" alt=\"~a\" width=\"256\" height=\"256\">" png alt))))
    (when (zerop (length color))
      (setf (gethash "note" code)
            "a custom hex exists only in the viewer iframe; pick a preset colour for the files"))
    code))

;;; --- one file off the CDN --------------------------------------------------------

(defun download (url)
  "URL's bytes, at most +MAX-BYTES-. A dial that fails or a status over 399 is
a refusal, never an unhandled error."
  (multiple-value-bind (body status)
      (nlk:http :get url
                :headers (list (cons "user-agent" +user-agent+)
                               (cons "accept" "*/*"))
                :binary t
                :timeout (setting :timeout)
                :connect-timeout (min 15 (setting :timeout)))
    (unless (and (integerp status) (< status 400))
      (fail "~a answered HTTP ~a~@[ ~a~]" url status
            (and (stringp body) (nlk:clip (nlk:trimmed body) +error-chars+))))
    (unless (vectorp body)
      (fail "~a answered no file" url))
    (when (> (length body) +max-bytes+)
      (fail "~a is larger than ~a bytes" url +max-bytes+))
    body))

(defun write-octets (octets file)
  "OCTETS written at FILE, which the caller has proved absent."
  (with-open-file (stream file :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
    (write-sequence octets stream))
  (length octets))

(defun folder-directory (text)
  "TEXT as a directory pathname that exists afterwards, or a refusal."
  (let ((text (nlk:trimmed (or text ""))))
    (when (or (zerop (length text)) (find (code-char 0) text))
      (fail "give a folder to save into, e.g. \"public/icons\""))
    (let* ((path (nlk:expand-home text))
           (absolute (if (uiop:absolute-pathname-p path)
                         path
                         (merge-pathnames path (uiop:getcwd))))
           (directory (uiop:ensure-directory-pathname absolute)))
      (handler-case (ensure-directories-exist (merge-pathnames "keep" directory))
        (error (condition)
          (fail "could not create ~a: ~a" (namestring directory)
                (nlk:one-line (princ-to-string condition)))))
      directory)))

(defun save-name (id kind color)
  "The file one saved icon is called: the original's names, the colour in the
stem when it is not orange."
  (format nil "~a~a"
          (if (string= color "orange") id (format nil "~a-~a" id color))
          (or (cdr (assoc kind +suffixes+ :test #'string=))
              (fail "unknown format ~s" kind))))

(defun save-one (id kind directory color)
  "KIND for ID in COLOR written under DIRECTORY => its row. A file already
there is left untouched."
  (let* ((file (merge-pathnames (save-name id kind color) directory))
         (row (nlk:json-object "format" kind "path" (namestring file))))
    (cond ((probe-file file)
           (setf (gethash "skipped" row) "already exists, left unchanged"))
          (t
           (handler-case
               (setf (gethash "bytes" row)
                     (write-octets (download (file-url id kind color)) file))
             (icon-index-error (condition)
               (setf (gethash "error" row) (princ-to-string condition)))
             (error (condition)
               (setf (gethash "error" row)
                     (nlk:one-line (princ-to-string condition)))))))
    row))

;;; --- the tools ------------------------------------------------------------------

(defun search-tool (request)
  "icon_search: the icons the query means."
  (answering
    (let* ((category (nlk:trimmed (or (icon-search-request-category request) "")))
           (limit (max 1 (min +max-limit+ (or (icon-search-request-limit request) 8)))))
      (when (and (plusp (length category))
                 (not (member category (categories) :test #'string-equal)))
        (fail "~s is no category: ~{~a~^, ~}" category (categories)))
      (let ((found (search-icons (icon-search-request-query request)
                                 :category category :limit limit)))
        (nlk:encode-json-object
         (nlk:json-object
          "count" (length found)
          "icons" (json-array (mapcar #'icon-summary found))
          "categories" (json-array (categories))
          "hint" "call icon_get for embed code and download links, or icon_save to put the files in the project"))))))

(defun get-tool (request)
  "icon_get: one icon's URLs, embed code and licence."
  (answering
    (let ((icon (require-icon (icon-get-request-id request))))
      (multiple-value-bind (color hex) (parse-color (icon-get-request-color request))
        (nlk:encode-json-object (icon-detail icon color hex))))))

(defun save-tool (request)
  "icon_save: one icon's files written into a folder of the project."
  (answering
    (let* ((icon (require-icon (icon-save-request-id request)))
           (id (icon-id icon))
           (directory (folder-directory (icon-save-request-folder request)))
           (formats (or (coerce (icon-save-request-formats request) 'list)
                        '("glb" "png"))))
      (dolist (kind formats)
        (unless (assoc kind +files+ :test #'string=)
          (fail "~s is no format: ~{~a~^, ~}" kind (mapcar #'car +files+))))
      (multiple-value-bind (color hex) (parse-color (icon-save-request-color request))
        (declare (ignore hex))
        (when (zerop (length color))
          (fail "saved files come in the preset colours only (~{~a~^, ~}): for another colour, embed the viewer iframe icon_get gives, or pick the colour in the viewer"
                (mapcar #'car +presets+)))
        (when (and (not (string= color "orange"))
                   (some (lambda (kind) (not (member kind +colored-files+ :test #'string=)))
                         formats))
          (fail "in ~a only these formats exist: ~{~a~^, ~}" color +colored-files+))
        (nlk:encode-json-object
         (nlk:json-object
          "id" id
          "color" color
          "folder" (namestring directory)
          "files" (json-array (mapcar (lambda (kind) (save-one id kind directory color))
                                       (remove-duplicates formats :test #'string=)))
          "license" (license-object)
          "hint" "reference the saved GLB with <model-viewer> (the embed code icon_get gives) and its local path"))))))

;;; --- /icon-index and the eval verbs ----------------------------------------------

(defun icon-index-command (args session-id)
  "/icon-index [words] typed at a shell: the icons that match, as text.
Typed alone it lists the library."
  (declare (ignore session-id))
  (answering-text
    (let ((text (nlk:trimmed (or args ""))))
      (let ((found (search-icons text :limit 12)))
        (if (null found)
            (format nil "no icon matches ~s. Try: ~{~a~^, ~}." text (categories))
            (format nil "~a~{~%~a~}~%~a"
                    (if (plusp (length text))
                        (format nil "icon index: icons for ~s" text)
                        "icon index: all icons")
                    (loop for icon in found
                          collect (format nil "- ~a (~s, ~a): ~a"
                                          (getf icon :title) (icon-id icon)
                                          (getf icon :category)
                                          (getf icon :description)))
                    "ask for one, e.g. \"put the 3D rocket in my hero section\"."))))))

(defun decode-arguments (contract pairs)
  (nlk:contract-decode contract (apply #'nlk:make-json-object pairs)))

(defun icon-search (query &key category (limit 8))
  "The icon_search tool from eval: what the model gets, as JSON.
  (icon-index:icon-search \"security\")"
  (search-tool (decode-arguments (nlk:contract 'icon-search-request)
                                 (list "query" (or query "")
                                       "category" (or category "")
                                       "limit" limit))))

(defun icon-get (id &key (color "orange"))
  "The icon_get tool from eval, as JSON.
  (icon-index:icon-get \"rocket\" :color \"blue\")"
  (get-tool (decode-arguments (nlk:contract 'icon-get-request)
                              (list "id" (or id "") "color" (or color "orange")))))

(defun icon-save (id folder &key formats (color "orange"))
  "The icon_save tool from eval, as JSON.
  (icon-index:icon-save \"rocket\" \"public/icons\" :formats '(\"glb\" \"png\"))"
  (save-tool (decode-arguments (nlk:contract 'icon-save-request)
                               (list "id" (or id "")
                                     "folder" (or folder "")
                                     "formats" (and formats (json-array formats))
                                     "color" (or color "orange")))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell icon-index
  (:section ("icon-index")
    (:guide "nothing here is required: the icon library ships with the cell; asset_base is where the icon files are fetched from, and only icon_save fetches; next step: ask for a 3D icon")
    ("asset_base" :string
     :default "https://cdn.jsdelivr.net/gh/minutechreview/3d-icons@aee7c0abd462cb20202df81a2cc171e5ab2d3f06"
     :doc "the https base the icon files are fetched from, at the library's pinned commit; the bundled catalog is the same revision")
    ("viewer_url" :string
     :default "https://motion.minutechbuilds.com/icons/viewer.html"
     :doc "the https URL of the 3D viewer the iframe and the markdown embed point at")
    ("timeout" :integer :default 60 :min 1
     :doc "how long one file download may take, in seconds"))
  (:settings #'read-settings)
  (:help :icon-index
         "icon-index: icon_search, icon_get, icon_save: 40 free CC0 3D icons, their embed code, and their files saved into a project"
         *manual*)
  (:tool "icon_search" (nlk:contract 'icon-search-request) #'search-tool)
  (:tool "icon_get" (nlk:contract 'icon-get-request) #'get-tool)
  (:tool "icon_save" (nlk:contract 'icon-save-request) #'save-tool)
  (:command "icon-index" 'icon-index-command
            :description "40 free 3D icons: search them, and their embed code"
            :argument-hint "[WORDS]"))
