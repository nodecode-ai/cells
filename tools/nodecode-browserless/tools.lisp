;;;; tools.lisp --- the three tool bodies, over Browserless' REST endpoints.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One call is one page: POST /content answers the rendered HTML,
;;;; /screenshot a PNG and /pdf a PDF, each with the launch options in the
;;;; query string (stealth, the residential proxy, ad blocking, captcha
;;;; solving) and the page's own options in the JSON body. The token never
;;;; reaches a tool's answer: it rides the query the cell builds.
;;;;
;;;; The model reads the endpoints' failures as one sentence naming what to
;;;; fix, and a transport failure as ERROR: BROWSERLESS-ERROR.

(in-package #:nodecode-browserless)

;;; --- the page as words ----------------------------------------------------

(defparameter +noise+
  '(("(?is)<script\\b[^>]*>.*?</script>" . " ")
    ("(?is)<style\\b[^>]*>.*?</style>" . " ")
    ("(?is)<noscript\\b[^>]*>.*?</noscript>" . " ")
    ("(?is)<svg\\b[^>]*>.*?</svg>" . " ")
    ("(?s)<!--.*?-->" . " "))
  "What is not a page's words: scripts, styles, SVG and comments, taken out whole.")

(defparameter +block-ends+
  '("(?i)<br\\b[^>]*>" "(?i)</p>" "(?i)</div>" "(?i)</li>" "(?i)</tr>"
    "(?i)</h[1-6]>" "(?i)</section>" "(?i)</article>" "(?i)</pre>" "(?i)</blockquote>")
  "Tags that end a block, which become a line break before the tags go.")

(defparameter +entities+
  '(("&nbsp;" . " ") ("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
    ("&quot;" . "\"") ("&#39;" . "'") ("&apos;" . "'") ("&mdash;" . "—")
    ("&ndash;" . "–") ("&hellip;" . "...") ("&rsquo;" . "'") ("&lsquo;" . "'")
    ("&ldquo;" . "\"") ("&rdquo;" . "\""))
  "The entities a page's words carry, written back as the characters they are.")

(defun page-text (html)
  "HTML's words: scripts and tags gone, block ends kept as line breaks, spaces collapsed."
  (let ((text (or html "")))
    (loop for (pattern . replacement) in +noise+
          do (setf text (cl-ppcre:regex-replace-all pattern text replacement)))
    (dolist (pattern +block-ends+)
      (setf text (cl-ppcre:regex-replace-all pattern text (format nil "~%~%"))))
    (setf text (cl-ppcre:regex-replace-all "<[^>]*>" text " "))
    (loop for (entity . char) in +entities+
          do (setf text (cl-ppcre:regex-replace-all (cl-ppcre:quote-meta-chars entity) text char)))
    (setf text (cl-ppcre:regex-replace-all "[ \\t\\x0B\\f\\r]+" text " "))
    (setf text (cl-ppcre:regex-replace-all " *\\n *" text (string #\Newline)))
    (setf text (cl-ppcre:regex-replace-all "\\n{3,}" text (format nil "~%~%")))
    (nlk:trimmed text)))

;;; --- browserless_fetch ----------------------------------------------------

(defun content-body (url wait-selector wait-ms)
  "The JSON body POST /content reads: the page, how long it may take, what to wait for."
  (let ((members (list "url" url
                        "gotoOptions" (goto-options)
                        "bestAttempt" t)))
    (when (nonblank wait-selector)
      (setf members (append members (list "waitForSelector" (selector-wait wait-selector)))))
    (when (and wait-ms (plusp wait-ms))
      (setf members (append members (list "waitForTimeout" wait-ms))))
    (apply #'nlk:make-json-object members)))

(defun fetch-answer (url format content)
  "The tool's answer: the page, what was asked for, the words, and what was cut."
  (let* ((limit (setting :content-chars))
         (asked (if (equal format "html") content (page-text content)))
         (cut (> (length asked) limit))
         (shown (if cut (nlk:clip asked limit) asked)))
    (answer "url" url
            "format" format
            "chars" (length asked)
            "truncated" cut
            "note" (if cut
                       (format nil "Cut at ~d of ~d characters: raise browserless.content_chars, ~
ask for a narrower page (a selector-driven view), or read the html with a fetch tool on a URL that ~
carries the part you need." limit (length asked))
                       (format nil "~d characters." (length asked)))
            "content" shown)))

(defun run-fetch (arguments)
  "browserless_fetch: render one page and answer what is on it."
  (let* ((url (page-fetch-url arguments))
         (format (let ((asked (page-fetch-page-format arguments)))
                   (if (member asked +formats+ :test #'equal) asked "text")))
         (timeout (or (page-fetch-timeout-seconds arguments) (setting :timeout-seconds)))
         (body (content-body url
                             (page-fetch-wait-for-selector arguments)
                             (page-fetch-wait-ms arguments))))
    (multiple-value-bind (content status)
        (call :post "/content" :object body :timeout timeout)
      (require-status status content url)
      (fetch-answer url format (if (stringp content) content (nlk:body-text content))))))

;;; --- browserless_screenshot -----------------------------------------------

(defun screenshot-body (url full-page width height wait-selector)
  "The JSON body POST /screenshot reads."
  (let ((members (list "url" url
                        "gotoOptions" (goto-options)
                        "options" (nlk:make-json-object
                                   "type" "png"
                                   "fullPage" (and full-page t)
                                   "omitBackground" nil)
                        "bestAttempt" t)))
    (when (or width height)
      (let ((viewport (list)))
        (when width (setf viewport (append viewport (list "width" width))))
        (when height (setf viewport (append viewport (list "height" height))))
        (setf members (append members (list "viewport" (apply #'nlk:make-json-object viewport))))))
    (when (nonblank wait-selector)
      (setf members (append members (list "waitForSelector" (selector-wait wait-selector)))))
    (apply #'nlk:make-json-object members)))

(defun run-screenshot (arguments)
  "browserless_screenshot: render one page, save the PNG, answer where."
  (let* ((url (page-shot-url arguments))
         (body (screenshot-body url
                                (page-shot-full-page arguments)
                                (page-shot-width arguments)
                                (page-shot-height arguments)
                                (page-shot-wait-for-selector arguments))))
    (multiple-value-bind (octets status)
        (call :post "/screenshot" :object body
                                   :timeout (+ 30 (setting :timeout-seconds)) :binary t)
      (require-status status (and (stringp octets) octets)
                      (format nil "No PNG was saved for ~a: a screenshot is bytes, and ~
Browserless said this instead." url))
      (unless (vectorp octets)
        (fail "Browserless answered ~a for the screenshot of ~a, not an image. ~a"
              (or (ignore-errors (nlk:body-text octets)) "an empty body") url +account-help+))
      (let ((path (save-bytes octets (file-name-for url) :type "png")))
        (answer "success" t
                "url" url
                "screenshot_path" path
                "bytes" (length octets)
                "note" (format nil "Look at the screenshot with (look ~s) and answer from what ~
you see." path))))))

;;; --- browserless_pdf ------------------------------------------------------

(defun pdf-body (url landscape wait-selector)
  "The JSON body POST /pdf reads."
  (let ((members (list "url" url
                        "gotoOptions" (goto-options)
                        "options" (nlk:make-json-object
                                   "landscape" (and landscape t)
                                   "printBackground" t
                                   "format" "A4")
                        "bestAttempt" t)))
    (when (nonblank wait-selector)
      (setf members (append members (list "waitForSelector" (selector-wait wait-selector)))))
    (apply #'nlk:make-json-object members)))

(defun run-pdf (arguments)
  "browserless_pdf: print one page, save the PDF, answer where."
  (let* ((url (page-pdf-url arguments))
         (body (pdf-body url (page-pdf-landscape arguments) (page-pdf-wait-for-selector arguments))))
    (multiple-value-bind (octets status)
        (call :post "/pdf" :object body :timeout (+ 30 (setting :timeout-seconds)) :binary t)
      (require-status status (and (stringp octets) octets)
                      (format nil "No PDF was saved for ~a: a PDF is bytes, and Browserless ~
said this instead." url))
      (unless (vectorp octets)
        (fail "Browserless answered ~a for the PDF of ~a, not a file. ~a"
              (or (ignore-errors (nlk:body-text octets)) "an empty body") url +account-help+))
      (let ((path (save-bytes octets (file-name-for url) :type "pdf")))
        (answer "success" t
                "url" url
                "pdf_path" path
                "bytes" (length octets)
                "note" (format nil "The PDF is at ~a; open it with (look ~s) or name it to the ~
operator." path path))))))

;;; --- the command ----------------------------------------------------------

(defun status-text ()
  "What the cell will do, in the operator's own terms: never the token."
  (format nil "Browserless: ~a~%token: ~a~%stealth: ~a   proxy: ~a~@[ (~a)~]   block ads: ~a   ~
solve captchas: ~a~%timeout: ~ds   page text up to ~d characters~%tools: browserless_fetch, ~
browserless_screenshot, browserless_pdf"
          (base-url)
          (if (plusp (length (or (setting :token) ""))) "set" "not set")
          (if (setting :stealth) "on" "off")
          (let ((proxy (setting :proxy))) (if (plusp (length proxy)) proxy "Browserless's own"))
          (let ((country (setting :proxy-country))) (and (plusp (length country)) country))
          (if (setting :block-ads) "on" "off")
          (if (setting :solve-captchas) "on" "off")
          (setting :timeout-seconds)
          (setting :content-chars)))

(defun run-command (args session-id)
  "The /browserless command: the cell's state, or one page rendered as its words."
  (declare (ignore session-id))
  (let ((url (nonblank (nlk:trimmed (or args "")))))
    (if (null url)
        (status-text)
        (let* ((body (content-body url nil nil)))
          (multiple-value-bind (content status) (call :post "/content" :object body)
            (require-status status content url)
            (let* ((text (page-text (if (stringp content) content (nlk:body-text content))))
                   (cut (> (length text) 1500)))
              (format nil "~a~%~%~a~@[~%~%[…] cut at 1500 of ~d characters~]"
                      url (if cut (nlk:clip text 1500) text) (and cut (length text)))))))))
