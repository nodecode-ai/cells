;;;; cell.lisp --- Fashio's e-commerce fashion photography, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of fashiolabs/fashio-agent-plugin (see NOTICE). The original is
;;;; configuration only: an Agent Plugins v1 package pointing an MCP client at
;;;; Fashio's hosted server, plus one skill. That server is a thin wrapper over
;;;; the public REST API at api.fashiolabs.com, so the cell speaks that API
;;;; directly -- the same account, the same key, the same diamonds -- and offers
;;;; its verbs as tools the model calls.
;;;;
;;;; Every call spends the account's diamonds where the original does: a
;;;; generation and a catalog job are paid, a preset can be deleted for good.
;;;; Each of those asks for confirm: true, which is the skill's "say the price
;;;; and wait for a yes" turned into a refusal the model cannot forget.

(defpackage #:nodecode-fashio
  (:use #:cl)
  (:nicknames #:fashio)
  (:export #:account #:catalog #:upload #:generate #:generation #:templates #:catalog-job
           #:fashio-error #:fashio-error-detail))

(in-package #:nodecode-fashio)

(nlk:define-peripheral fashio :not-running t)

(defparameter +host+ "https://api.fashiolabs.com"
  "Fashio's public API, the host its MCP server calls on the key's behalf.")

(defparameter +default-key-env+ "FASHIO_API_KEY"
  "The variable a key is read from when the section names none: the one the
original and its README use.")

(defparameter +max-upload-bytes+ (* 8 1024 1024)
  "The largest image the upload endpoint takes, in bytes: its own 8 MB ceiling.")

(defparameter +list-limit+ 20
  "How many catalog rows one listing carries unless the caller asks for fewer.")

(defparameter +price-note+
  "A generation and a catalog job deduct diamonds from the key's Fashio account the moment
they are accepted, the same balance the app shows; a failed job is refunded. There is no test
mode. Call fashio_account first, say what will be made and what it costs, and only then call
again with confirm: true. A preset deleted with fashio_templates action \"delete\" is gone for
good. Results arrive as URLs: poll fashio_generation until the status is no longer \"processing\"."
  "What (help :fashio) answers while the cell runs.")

;;; --- the section, read back ---------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, when it is a non-blank string; else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun variable-value (name)
  "What the variable NAME holds, when anything does."
  (nonblank (nle::credential-env name)))

(defun read-settings (values table)
  "The declared VALUES, plus the key they authenticate with and the host they
ask, read once, here: api_key written inline, else the variable api_key_env
names, which is FASHIO_API_KEY unless the operator names another."
  (declare (ignore table))
  (let* ((env (or (nonblank (getf values :api-key-env)) +default-key-env+))
         (inline (nonblank (getf values :api-key)))
         (key (or inline (variable-value env)))
         (base (string-right-trim "/" (or (getf values :base-url) "")))
         (plist (copy-list values)))
    (unless key
      (nlk:config-error "api_key_env names ~a, but nothing is set there: create a Fashio API key at https://fashiolabs.com (Profile -> API Keys; it starts with fio_live_ and is shown once), put it in that variable or write it here as api_key"
                        env))
    (when (plusp (length base))
      (unless (or (uiop:string-prefix-p "https://" base) (uiop:string-prefix-p "http://" base))
        (nlk:config-error "base_url ~s has no http:// or https:// scheme: leave it empty for api.fashiolabs.com, or set it to the host that answers for the key"
                          base)))
    (setf (getf plist :key) key
          (getf plist :base-url) (if (plusp (length base)) base +host+))
    plist))

;;; --- one request ----------------------------------------------------------------

(defun query-string (pairs)
  "PAIRS as a URL query, the empty string when nothing is set: ((\"gender\" . \"female\"))."
  (let ((kept (remove-if-not #'cdr pairs)))
    (if (null kept)
        ""
        (format nil "?~{~a~^&~}"
                (loop for (key . value) in kept
                      collect (format nil "~a=~a" key
                                      (quri:url-encode (princ-to-string value))))))))

(defun api-headers (&optional content-type)
  "The headers a Fashio call carries: the key as a bearer, JSON back, and the
body's own type when it has one."
  (remove nil
          (list (cons "authorization" (format nil "Bearer ~a" (setting :key)))
                (cons "accept" "application/json")
                (and content-type (cons "content-type" content-type)))))

(defun error-note (decoded text)
  "What a failed answer says: Fashio's own `error' member when it decoded, else
the start of the body."
  (or (and (hash-table-p decoded) (nlk:json-value decoded :text "error"))
      (and (nonblank text) (nlk:clip (nlk:one-line text) 300 :disclose t))))

(defun call-api (method path &key query content content-type)
  "METHOD PATH through the configured host => the decoded JSON body. A status
Fashio reports as a failure, and a transport error, both refuse with what was
said rather than signalling past the tool."
  (let* ((url (concatenate 'string (setting :base-url) path (query-string query)))
         (timeout (setting :timeout-seconds))
         (body nil)
         (status nil))
    (handler-case
        (multiple-value-setq (body status)
          (nlk:http method url
                    :headers (api-headers content-type)
                    :content content
                    :timeout timeout
                    :connect-timeout (min 10 timeout)))
      (error (condition)
        (fail "~a did not answer: ~a" url (nlk:one-line (princ-to-string condition)))))
    (let* ((text (nlk:body-text body))
           (decoded (and (nonblank text)
                         (handler-case (nlk:decode-json text) (error () nil)))))
      (unless (and (integerp status) (< status 300))
        (fail "Fashio answered HTTP ~a on ~a~@[: ~a~]" status path (error-note decoded text)))
      (multiple-value-bind (flag present) (if (hash-table-p decoded) (gethash "success" decoded) (values t nil))
        (when (and present (not (eq flag t)))
          (fail "Fashio refused ~a: ~a" path (or (error-note decoded text) "no reason given"))))
      (or decoded
          (fail "Fashio answered ~a with something that is not JSON: ~a"
                path (nlk:clip (nlk:one-line text) 300 :disclose t))))))

;;; --- reading an answer -------------------------------------------------------

(defun rows-of (body)
  "BODY's rows: its first non-empty array member, which is where every Fashio
list endpoint puts them under a name of its own, else the body when it is an
array itself."
  (typecase body
    (vector (if (stringp body) #() body))
    (hash-table
     (let ((found nil))
       (maphash (lambda (key value)
                  (declare (ignore key))
                  (when (and (null found) (vectorp value) (not (stringp value))
                             (plusp (length value)))
                    (setf found value)))
                body)
       (or found #())))
    (t #())))

(defun row-label (row)
  "ROW's human label, when it carries one: a name, a label or a title."
  (when (hash-table-p row)
    (loop for key in '("label" "name" "title" "shotType" "category")
          for text = (nlk:json-value row :text key)
          when text return (nlk:clip (nlk:one-line text) 80))))

(defun row-text (row)
  "ROW as one line: its id, its label and the few attributes a chooser needs."
  (if (null row)
      "?"
      (if (hash-table-p row)
          (let ((parts (list (or (nlk:json-value row :string "id") "?")
                             (row-label row)))
                (extras '()))
            (when (nlk:json-value row :string "gender")
              (push (format nil "gender ~a" (nlk:json-value row :string "gender")) extras))
            (when (nlk:json-value row :string "inputShape")
              (push (format nil "needs ~a" (nlk:json-value row :string "inputShape")) extras))
            (when (nlk:json-value row :integer "shotCount")
              (push (format nil "~d shots" (nlk:json-value row :integer "shotCount")) extras))
            (when (nlk:json-value row :integer "fallbackPriceDiamonds")
              (push (format nil "~d diamonds" (nlk:json-value row :integer "fallbackPriceDiamonds"))
                    extras))
            (format nil "~@[~a ~]~@[-- ~a ~]~@[~{~a~^, ~}~]"
                    (first parts) (second parts) extras))
          (princ-to-string row))))

(defun format-rows (heading rows &key (limit +list-limit+))
  "ROWS under HEADING, at most LIMIT of them, as the lines the model reads."
  (let* ((all (coerce rows 'list))
         (shown (if (> (length all) limit) (subseq all 0 limit) all))
         ;; Rows the endpoint gave without a label are still rows: the label is
         ;; decoration, the id is what a call needs.
         (lines (mapcar #'row-text shown)))
    (if (null all)
        (format nil "~a: nothing came back." heading)
        (format nil "~a (~d~@[, first ~d~]):~%~{  ~a~%~}"
                heading (length all) (and (> (length all) limit) limit) lines))))

(defun format-generation (body)
  "BODY -- one generation or catalog job -- as the lines the model reads."
  (let* ((id (or (nlk:json-value body :string "id") "?"))
         (status (or (nlk:json-value body :string "status") "unknown"))
         (urls (nlk:json-array body "resultUrls"))
         (total (nlk:json-value body :integer "packTotal"))
         (done (nlk:json-value body :integer "packCompleted"))
         (failed (nlk:json-value body :integer "packFailed"))
         (error (nlk:json-value body :text "errorMessage")))
    (with-output-to-string (out)
      (format out "Fashio generation ~a: ~a" id status)
      (when total
        (format out " (~d of ~d shots done~@[, ~d failed~])" (or done 0) total failed))
      (cond ((plusp (length urls))
             (format out ":")
             (loop for url across urls
                   for index from 1
                   do (format out "~% ~d. ~a" index url)))
            ((and (not error) (equal status "processing"))
             (format out " -- still working; poll it again in a few seconds.")))
      (when error
        (format out "~%  what failed: ~a" (nlk:clip (nlk:one-line error) 400 :disclose t))))))

;;; --- the verbs the tools and /fashio call ------------------------------------

(define-verb account ()
  "The account's diamond balance and plan. => the lines the model reads."
  (let* ((body (call-api :get "/v1/account"))
         (balance (nlk:json-value body :integer "diamondBalance"))
         (plan (nlk:json-value body :object "plan"))
         (status (and plan (nlk:json-value plan :string "status")))
         (renews (and plan (nlk:json-value plan :string "renewsAt"))))
    (format nil "Fashio account: ~d diamonds~@[ (plan ~a~@[, renews ~a~])~]. A generation deducts its price from this balance."
            (or balance 0) status renews)))

(define-verb catalog (kind &key gender pose-type limit)
  "One Fashio catalogue: KIND is \"tools\", \"fashion_models\", \"venues\",
\"poses\" or \"ready_sets\". => the rows the model reads."
  (let* ((kind (nonblank kind))
         (limit (max 1 (min 100 (or limit (setting :list-limit))))))
    (unless (member kind '("tools" "fashion_models" "venues" "poses" "ready_sets")
                    :test #'equal)
      (fail "catalog kind ~s is not one of \"tools\", \"fashion_models\", \"venues\", \"poses\", \"ready_sets\""
            kind))
    (let ((body (cond ((equal kind "tools") (call-api :get "/v1/models"))
                      ((equal kind "fashion_models")
                       (call-api :get "/v1/fashion-models"
                                 :query (list (cons "gender" (nonblank gender))
                                              (cons "limit" limit))))
                      ((equal kind "venues")
                       (call-api :get "/v1/venues" :query (list (cons "limit" limit))))
                      ((equal kind "poses")
                       (call-api :get "/v1/poses"
                                 :query (list (cons "type" (nonblank pose-type))
                                              (cons "limit" limit))))
                      (t (call-api :get "/v1/ready-template-sets"
                                   :query (list (cons "limit" limit)))))))
      (format-rows (format nil "Fashio ~a" kind) (rows-of body) :limit limit))))

(defun image-type (bytes)
  "BYTES' media type when they are a JPEG, PNG or WebP image, by signature;
else NIL. The endpoint checks signatures too, so the extension is not read."
  (flet ((matches (text &optional (start 0))
           (and (<= (+ start (length text)) (length bytes))
                (loop for index below (length text)
                      always (= (aref bytes (+ start index)) (char-code (char text index)))))))
    (cond ((and (>= (length bytes) 3)
                (= (aref bytes 0) #xFF) (= (aref bytes 1) #xD8) (= (aref bytes 2) #xFF))
           "image/jpeg")
          ((and (>= (length bytes) 8) (= (aref bytes 0) #x89) (matches "PNG" 1)) "image/png")
          ((and (>= (length bytes) 12) (matches "RIFF") (matches "WEBP" 8)) "image/webp")
          (t nil))))

(defun image-bytes (path)
  "PATH's bytes, read whole."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))

(define-verb upload (path)
  "Upload one local image to Fashio => the URL a generation takes in its images."
  (let ((path (nonblank path)))
    (unless path
      (fail "an upload needs a path: the image file to send (jpeg, png or webp, up to 8 MB)"))
    (unless (probe-file path)
      (fail "there is no file at ~a to upload" path))
    (let* ((bytes (image-bytes path))
           (type (image-type bytes)))
      (unless type
        (fail "~a is not a JPEG, PNG or WebP image (read by signature, as Fashio does); convert it first"
              (file-namestring path)))
      (when (> (length bytes) +max-upload-bytes+)
        (fail "~a is ~d bytes: Fashio's upload takes up to 8 MB"
              (file-namestring path) (length bytes)))
      (let* ((body (call-api :post "/v1/uploads" :content bytes :content-type type))
             (url (nlk:json-value body :text "url")))
        (unless url
          (fail "Fashio's upload answered no url"))
        (format nil "Uploaded ~a (~a, ~d KB): ~a~%  Pass it as a URL in a generation's images."
                (file-namestring path) type (round (length bytes) 1024) url)))))

(defun put-member (table key value &key (test #'nonblank))
  "TABLE with VALUE under KEY when TEST says it is a value."
  (when (funcall test value)
    (setf (gethash key table) value))
  table)

(defun confirm-words (what price)
  "The refusal a paid call answers until the caller says confirm: true. PRICE is
what it will cost, in words, for the model to pass on."
  (fail "~a was not confirmed: it spends the account's diamonds (~a), and there is no test mode. Check the balance with fashio_account, say what will be made and what it costs, then call again with confirm: true"
        what price))

(define-verb generate (tool-id images &key template-set-id pose ratio custom-prompt motion-prompt
                                      target-ratio jewelry-type shot-type style-preset
                                      background-color duration confirm)
  "Start one Fashio generation => the job id to poll. Spends diamonds."
  (let* ((images (images-object images))
         (tool-id (nonblank tool-id))
         (template-set-id (nonblank template-set-id)))
    (unless images
      (fail "a generation needs images: at least one image field (for virtual_model, fashion_model and garment_photo, with URLs from fashio_upload), as an object"))
    (unless (or tool-id template-set-id)
      (fail "a generation needs a tool_id (fashio_catalog kind:\"tools\" lists them, with what each needs) or a template_set_id from fashio_templates"))
    (unless confirm
      (confirm-words "this generation" "fashio_catalog kind:\"tools\" shows each tool's fallback price"))
    (let* ((body (put-member (nlk:json-object "images" images) "toolId" tool-id))
           (answer nil))
      (put-member body "templateSetId" template-set-id)
      (put-member body "pose" pose)
      (put-member body "ratio" ratio)
      (put-member body "customPrompt" custom-prompt)
      (put-member body "motionPrompt" motion-prompt)
      (put-member body "targetRatio" target-ratio)
      (put-member body "jewelryType" jewelry-type)
      (put-member body "shotType" shot-type)
      (put-member body "stylePreset" style-preset)
      (put-member body "backgroundColor" background-color)
      (put-member body "duration" duration :test (lambda (value) (and (realp value) (plusp value))))
      (setf answer (call-api :post "/v1/generations"
                             :content (nlk:encode-json-object body)
                             :content-type "application/json"))
      (format nil "Fashio job ~a: ~a. The diamonds are deducted now. Poll it with fashio_generation id:\"~a\"."
              (or (nlk:json-value answer :string "id") "?")
              (or (nlk:json-value answer :string "status") "processing")
              (or (nlk:json-value answer :string "id") "?")))))

(defun images-object (value)
  "VALUE as an images object -- a hash table or an alist of field to URL or id --
when it carries at least one field; else NIL."
  (let ((table (typecase value
                 (hash-table value)
                 (list (let ((table (make-hash-table :test #'equal)))
                         (loop for (key . item) in value
                               when (and (nonblank key) item)
                                 do (setf (gethash key table) item))
                         table))
                 (t nil))))
    (and table (plusp (hash-table-count table)) table)))

(define-verb generation (id)
  "One Fashio generation or catalog job, by id => its status and results."
  (let ((id (nonblank id)))
    (unless id
      (fail "a generation needs an id: the one create answers with"))
    (format-generation
     (call-api :get (format nil "/v1/generations/~a" (quri:url-encode id))))))

(define-verb templates (action &key id label source-media-id confirm)
  "The account's saved Fashio presets: ACTION is \"list\", \"save\" or \"delete\"."
  (let ((action (nonblank action)))
    (cond
      ((equal action "list")
       (format-rows "Fashio saved presets (reuse one as a generation's template_set_id)"
                    (rows-of (call-api :get "/v1/template-sets"))))
      ((equal action "save")
       (let* ((label (nonblank label))
              (source (nonblank source-media-id)))
         (unless label
           (fail "a save needs a label: a few words naming the look"))
         (unless source
           (fail "a save needs source_media_id: the id of a finished generation whose model, venue, pose and ratio are worth reusing"))
         (let ((answer (call-api :post "/v1/template-sets"
                                 :content (nlk:encode-json-object
                                           (nlk:json-object "label" label "sourceMediaId" source))
                                 :content-type "application/json")))
           (format nil "Saved the preset as ~a. Reuse it with fashio_generate template_set_id."
                   (or (nlk:json-value answer :string "id") "?")))))
      ((equal action "delete")
       (let ((id (nonblank id)))
         (unless id
           (fail "a delete needs an id: the preset's own, from fashio_templates action \"list\""))
         (unless confirm
           (fail "the delete of preset ~a was not confirmed: it is permanent and there is no undo; call again with confirm: true"
                 id))
         (call-api :delete (format nil "/v1/template-sets/~a" (quri:url-encode id)))
         (format nil "Deleted the Fashio preset ~a." id)))
      (t (fail "templates action ~s is not one of \"list\", \"save\", \"delete\"" action)))))

(define-verb catalog-job (ready-template-set-id image &key shot-ids ratio confirm)
  "A whole Fashio photoshoot from one curated ready set => the job to poll. Spends diamonds."
  (let* ((set-id (nonblank ready-template-set-id))
         (image (nonblank image))
         (shots (typecase shot-ids
                  (vector shot-ids)
                  (list (coerce shot-ids 'vector))
                  (t #())))
         (body nil)
         (answer nil))
    (unless set-id
      (fail "a catalog job needs ready_template_set_id: fashio_catalog kind:\"ready_sets\" lists them, with their shot counts"))
    (unless image
      (fail "a catalog job needs image: the product photo's URL (fashio_upload answers one)"))
    (unless confirm
      (confirm-words "this catalog job" "a catalog job is billed per shot, and the ready set's shot count is on the row fashio_catalog listed"))
    (setf body (nlk:json-object "readyTemplateSetId" set-id
                                 "images" (nlk:json-object "garment_photo" image)))
    (when (plusp (length shots))
      (setf (gethash "shotIds" body) shots))
    (put-member body "ratio" ratio)
    (setf answer (call-api :post "/v1/catalog-jobs"
                           :content (nlk:encode-json-object body)
                           :content-type "application/json"))
    (format nil "Fashio catalog job ~a: ~a~@[ (~d shots)~]~@[, ~d diamonds charged~]. Poll it with fashio_generation id:\"~a\"."
            (or (nlk:json-value answer :string "mediaItemId") "?")
            (or (nlk:json-value answer :string "status") "started")
            (nlk:json-value answer :integer "packTotal")
            (nlk:json-value answer :integer "diamondCost")
            (or (nlk:json-value answer :string "mediaItemId") "?"))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (fashio-error (condition)
       (nle:failure (format nil "ERROR: ~a" (fashio-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Fashio: ~a" (nlk:one-line (princ-to-string condition)))))))

;;; --- the tools' boundaries -------------------------------------------------------

(defparameter +account-schema+
  (list :description "The Fashio account behind the API key: its diamond balance and plan. Call it before spending diamonds, and tell the user the balance."
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)))
  "fashio_account's schema: it takes no arguments, so this is the plist
REGISTER-TOOL takes for a shape no contract carries (a contract with no members
types nothing).")

(nlk:define-contract fashio-catalog-call
  (:doc "Browse a Fashio catalogue: the generation tools and their fallback prices, the AI fashion models, the venues, the poses, or Fashio's curated ready photoshoot sets. The ids it answers with are what a generation's images and pose take.")
  ("kind" :text :required t
          :options '("tools" "fashion_models" "venues" "poses" "ready_sets")
          :doc "which catalogue: tools (the toolIds and what each needs), fashion_models, venues, poses, ready_sets")
  ("gender" :string
            :doc "fashion_models only: male or female; unset, both")
  ("pose_type" :string
               :options '("ring" "necklace" "earring" "bracelet")
               :doc "poses only: the jewelry pose set; unset, the default set")
  ("limit" :integer
           :doc "how many rows to list, 1 to 100; unset, the section's list_limit"))

(nlk:define-contract fashio-upload-call
  (:doc "Upload one image from this machine to Fashio and get back the URL a generation's image fields take. JPEG, PNG or WebP, up to 8 MB, read by signature.")
  ("path" :text :required t
          :doc "the image file's path"))

(nlk:define-contract fashio-generate-call
  (:doc "Start a Fashio generation: a garment on an AI model, virtual try-on, jewelry on a model, a flat lay, background removal, upscaling, reframing, an instruction edit or a short product video. It SPENDS the account's diamonds the moment it is accepted, so confirm first. Answers the job id; poll fashio_generation for the result URLs.")
  ("tool_id" :string
             :doc "the generation tool, as fashio_catalog kind:\"tools\" lists them: virtual_model, outfit_photo, jewelry_on_model, model_swap, model_to_flat_lay, amanteur_to_pro, pose_angle_switch, photo_edit, photo_enhance, remove_background, photo_reframe, ai_videos, clothing_ecom_pack, jewelry_ecom_pack. Optional when template_set_id is given")
  ("images" :object :required t
            :doc "the tool's image fields as an object: garment_photo, fashion_model, venue, user_photo, jewelry_image, jewelry_images, pose -- each a URL (from fashio_upload) or a catalog id")
  ("template_set_id" :string
                     :doc "a saved preset from fashio_templates: its model, venue, pose and ratio, replayed on a new product")
  ("pose" :string :doc "a pose id from fashio_catalog kind:\"poses\"")
  ("ratio" :string :slot aspect
           :options '("1:1" "9:16" "16:9" "4:3" "3:4" "4:5" "auto")
           :doc "the output aspect ratio")
  ("custom_prompt" :string
                   :doc "a free-text instruction; required by photo_edit, where it IS the edit")
  ("motion_prompt" :string :doc "ai_videos only: what the garment does in the shot")
  ("target_ratio" :string
                  :options '("1:1" "9:16" "16:9" "4:3" "3:4" "4:5" "auto")
                  :doc "photo_reframe only: the ratio to reframe to")
  ("jewelry_type" :string
                  :options '("ring" "necklace" "earring" "bracelet")
                  :doc "jewelry_on_model only: what the piece is")
  ("shot_type" :string :doc "pro_studio only")
  ("style_preset" :string :doc "pro_studio only")
  ("background_color" :string :doc "jewelry_on_model only: a solid studio background")
  ("duration" :number :doc "ai_videos only: seconds, 5 by default")
  ("confirm" :boolean
             :doc "true only after the user has said yes to spending the diamonds; the call is refused without it"))

(nlk:define-contract fashio-generation-call
  (:doc "One Fashio generation or catalog job by id: its status, its result URLs once it is done, and for a photoshoot pack how many shots have landed.")
  ("id" :text :required t
        :doc "the job id create answered with"))

(nlk:define-contract fashio-templates-call
  (:doc "The account's saved Fashio presets -- a model, venue, pose and ratio kept from a generation and replayed on the next product. List them, save one from a finished generation, or delete one (for good).")
  ("action" :text :required t
            :options '("list" "save" "delete")
            :doc "list, save or delete")
  ("id" :string :doc "delete: the preset to remove")
  ("label" :string :doc "save: a few words naming the look")
  ("source_media_id" :string :doc "save: the id of the finished generation whose styling is kept")
  ("confirm" :boolean :doc "delete only: true, after the user has accepted that it is gone for good"))

(nlk:define-contract fashio-catalog-job-call
  (:doc "Run one of Fashio's curated ready photoshoot sets on a product photo: one image per shot, billed per shot, SPENDING the account's diamonds. Confirm first. Answers the job id; poll fashio_generation for the shots as they land.")
  ("ready_template_set_id" :text :required t
                           :doc "the ready set, as fashio_catalog kind:\"ready_sets\" lists them")
  ("image" :text :required t
           :doc "the product photo's URL (fashio_upload answers one)")
  ("shot_ids" :array :of :string
              :doc "generate only these shots of the set; unset, every shot")
  ("ratio" :string :slot aspect
           :options '("1:1" "9:16" "16:9" "4:3" "3:4" "4:5" "auto")
           :doc "the output aspect ratio")
  ("confirm" :boolean
             :doc "true only after the user has said yes to spending the diamonds; the call is refused without it"))

(defun account-tool (call)
  "The fashio_account tool."
  (declare (ignore call))
  (answering (account)))

(defun catalog-tool (call)
  "The fashio_catalog tool."
  (answering (catalog (fashio-catalog-call-kind call)
                       :gender (fashio-catalog-call-gender call)
                       :pose-type (fashio-catalog-call-pose-type call)
                       :limit (fashio-catalog-call-limit call))))

(defun upload-tool (call)
  "The fashio_upload tool."
  (answering (upload (fashio-upload-call-path call))))

(defun generate-tool (call)
  "The fashio_generate tool."
  (answering (generate (fashio-generate-call-tool-id call)
                        (fashio-generate-call-images call)
                        :template-set-id (fashio-generate-call-template-set-id call)
                        :pose (fashio-generate-call-pose call)
                        :ratio (fashio-generate-call-aspect call)
                        :custom-prompt (fashio-generate-call-custom-prompt call)
                        :motion-prompt (fashio-generate-call-motion-prompt call)
                        :target-ratio (fashio-generate-call-target-ratio call)
                        :jewelry-type (fashio-generate-call-jewelry-type call)
                        :shot-type (fashio-generate-call-shot-type call)
                        :style-preset (fashio-generate-call-style-preset call)
                        :background-color (fashio-generate-call-background-color call)
                        :duration (fashio-generate-call-duration call)
                        :confirm (fashio-generate-call-confirm call))))

(defun generation-tool (call)
  "The fashio_generation tool."
  (answering (generation (fashio-generation-call-id call))))

(defun templates-tool (call)
  "The fashio_templates tool."
  (answering (templates (fashio-templates-call-action call)
                         :id (fashio-templates-call-id call)
                         :label (fashio-templates-call-label call)
                         :source-media-id (fashio-templates-call-source-media-id call)
                         :confirm (fashio-templates-call-confirm call))))

(defun catalog-job-tool (call)
  "The fashio_catalog_job tool."
  (answering (catalog-job (fashio-catalog-job-call-ready-template-set-id call)
                           (fashio-catalog-job-call-image call)
                           :shot-ids (fashio-catalog-job-call-shot-ids call)
                           :ratio (fashio-catalog-job-call-aspect call)
                           :confirm (fashio-catalog-job-call-confirm call))))

;;; --- /fashio ------------------------------------------------------------------

(defun status-line ()
  "What /fashio answers with no argument: which host it asks, and what it offers."
  (format nil "Fashio: ~a, key ~a. Tools: fashio_account, fashio_catalog, fashio_upload, fashio_generate, fashio_generation, fashio_templates, fashio_catalog_job. /fashio GENERATION-ID polls one job."
          (setting :base-url)
          (if (setting :key) "set" "missing")))

(defun run-command (args session-id)
  "/fashio GENERATION-ID: the operator's own look at a job. No argument, where
the cell points and what it offers."
  (declare (ignore session-id))
  (let ((id (nlk:trimmed (or args ""))))
    (if (plusp (length id))
        (answering (generation id))
        (status-line))))

;;; --- the declaration -------------------------------------------------------------

(nle:define-cell fashio
  (:section ("fashio")
    (:guide "one key: a Fashio API key (fio_live_...) from https://fashiolabs.com -> Profile -> API Keys, set in the variable api_key_env names (FASHIO_API_KEY) or written here as api_key; next step: ask for a garment on a model, and say yes to the diamond price first")
    ("api_key" :secret
     :doc "the Fashio API key, written here (optional)")
    ("api_key_env" :env :default "FASHIO_API_KEY"
     :doc "the environment variable holding the key")
    ("base_url" :string :default ""
     :doc "the host the API is asked on; empty, api.fashiolabs.com")
    ("timeout_seconds" :integer :default 60 :min 1
     :doc "one call's ceiling, in seconds: an upload of 8 MB wants room")
    ("list_limit" :integer :default 20 :min 1
     :doc "how many catalogue rows one listing carries"))
  (:settings #'read-settings)
  (:help :fashio
         "fashio: on-model fashion photos, try-on, flat lays and product videos (fashio_generate)"
         +price-note+)
  (:tool "fashio_account" +account-schema+ #'account-tool)
  (:tool "fashio_catalog" (nlk:contract 'fashio-catalog-call) #'catalog-tool)
  (:tool "fashio_upload" (nlk:contract 'fashio-upload-call) #'upload-tool)
  (:tool "fashio_generate" (nlk:contract 'fashio-generate-call) #'generate-tool)
  (:tool "fashio_generation" (nlk:contract 'fashio-generation-call) #'generation-tool)
  (:tool "fashio_templates" (nlk:contract 'fashio-templates-call) #'templates-tool)
  (:tool "fashio_catalog_job" (nlk:contract 'fashio-catalog-job-call) #'catalog-job-tool)
  (:command "fashio" 'run-command
            :description "Fashio: a job's status, or what the cell offers"
            :argument-hint "GENERATION-ID"))
