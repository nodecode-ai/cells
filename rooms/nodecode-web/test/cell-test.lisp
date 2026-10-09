;;;; cell-test.lisp --- the page on the gateway's port, and nothing beside it.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun web-get (port path)
  "GET PATH on PORT's gateway, following no redirect => (values BODY STATUS HEADERS)."
  (nlk:http :get (format nil "http://127.0.0.1:~d~a" port path)))

(deftest web-cell-serves-the-page-and-nothing-beside-it (with-temp-gateway (port))
  (with-cell-stop ((web-start))
    (multiple-value-bind (body status headers) (web-get port "/web/")
      (is (= 200 status) "the page answers without the operator token")
      (is (search "text/html" (gethash "content-type" headers)))
      (is (search "<title>Nodecode</title>" body) "it is index.html")
      (is (search "default-src 'self'" (gethash "content-security-policy" headers)) "loads only its own")
      (is (search "frame-ancestors 'none'" (gethash "content-security-policy" headers)) "never framed"))
    (loop for (name type) in '(("app.js" "text/javascript") ("md.js" "text/javascript")
                               ("control.js" "text/javascript") ("theme.js" "text/javascript")
                               ("i18n.js" "text/javascript") ("zh.js" "text/javascript")
                               ("app.css" "text/css") ("mark.svg" "image/svg+xml")
                               ("geist.woff2" "font/woff2") ("martian-mono.woff2" "font/woff2"))
          do (multiple-value-bind (body status headers) (web-get port (format nil "/web/~a" name))
               (declare (ignore body))
               (is (= 200 status) name)
               (is (search type (gethash "content-type" headers)) type)))
    (dolist (outside '("/web/cell.lisp" "/web/../cell.lisp" "/web/.hidden" "/web/test/support.lisp"
                       "/web/nodecode-web.asd"))
      (is (= 404 (raw-gateway-status port outside (format nil "Host: 127.0.0.1:~d" port))) outside))
    (is (= 302 (raw-gateway-status port "/web" (format nil "Host: 127.0.0.1:~d" port))) "/web leads on")
    (funcall (shiftf stop nil))
    (is (= 404 (raw-gateway-status port "/web/" (format nil "Host: 127.0.0.1:~d" port))) "stopped: gone")))

(deftest web-cell-keeps-the-page-it-serves-on
    (with-saved-globals ((nlk::*cells* (list (nlk::make-cell :name "nodecode-web"
                                                                :systems '("nodecode-web"))))))
  ;; The page's own switch cannot take the page away: the folder says why
  ;; through the off question's advice, while it runs and only for itself.
  (is (null (nle::cell-off-refusal "nodecode-web")) "stopped, it says nothing")
  (with-cell-stop ((web-start))
    (is (search "serves this page" (nle::cell-off-refusal "nodecode-web")))
    (is (null (nle::cell-off-refusal "nodecode-other")) "another folder is not its to keep"))
  (is (null (nle::cell-off-refusal "nodecode-web")) "and stopped again, its advice is gone"))

;; The page's colors. app.css draws every color through a token of :root, and
;; the light block (theme.js sets data-theme on the root) redefines every one of
;; them: a literal anywhere else stays dark in light, and a token the light
;; block forgot does too.

(defun web-page-text (name)
  "The page's file NAME, read whole."
  (uiop:read-file-string (merge-pathnames name nodecode-web::*page-directory*)))

(defun web-css-declarations (css)
  "CSS's declarations, comments dropped, each as (SELECTOR . DECLARATION): the
selector of the innermost rule it is written in, both trimmed."
  (let ((text (with-output-to-string (out)
                (loop with at = 0
                      for open = (search "/*" css :start2 at)
                      do (write-string css out :start at :end (or open (length css)))
                         (unless open (return))
                         (setf at (+ 2 (search "*/" css :start2 (+ 2 open)))))))
        (rules '()) (declarations '()) (pending (make-string-output-stream)))
    (flet ((segment () (string-trim '(#\Space #\Tab #\Newline) (get-output-stream-string pending))))
      (loop for char across text
            do (case char
                 (#\{ (push (segment) rules))
                 ((#\; #\}) (let ((declaration (segment)))
                              (when (plusp (length declaration))
                                (push (cons (first rules) declaration) declarations)))
                            (when (char= char #\}) (pop rules)))
                 (t (write-char char pending)))))
    (nreverse declarations)))

(defun web-color-literal-p (text)
  "True when TEXT spells a color itself: a hex color, or rgb(), rgba(), hsl(), hsla()."
  (or (some (lambda (fn) (search fn text)) '("rgb(" "rgba(" "hsl(" "hsla("))
      (loop for hash = (position #\# text) then (position #\# text :start (1+ hash))
            while hash
            thereis (let ((end (or (position-if-not (lambda (char) (digit-char-p char 16)) text :start (1+ hash))
                                   (length text))))
                      (member (- end hash 1) '(3 4 6 8))))))

(defun web-root-colors (declarations selector)
  "The custom properties SELECTOR declares with a color in their value, by name."
  (loop for (rule . declaration) in declarations
        for colon = (position #\: declaration)
        when (and (string= rule selector) colon (eql 0 (search "--" declaration))
                  (web-color-literal-p (subseq declaration colon)))
          collect (subseq declaration 0 colon)))

(deftest web-cell-theme-colors-are-root-tokens ()
  (let* ((declarations (web-css-declarations (web-page-text "app.css")))
         (roots '(":root" ":root[data-theme=\"light\"]"))
         (dark (web-root-colors declarations (first roots)))
         (light (web-root-colors declarations (second roots))))
    (is (null (loop for (rule . declaration) in declarations
                    when (and (not (member rule roots :test #'string=)) (web-color-literal-p declaration))
                      collect (format nil "~a { ~a }" rule declaration))) "no color outside the root tokens")
    (is (member "--accent" dark :test #'string=) "the dark tokens are read")
    (is (null (set-difference dark light :test #'string=)) "light redefines every color token")
    (is (null (set-difference light dark :test #'string=)) "and declares none dark lacks")))

;; The page's text. Five tones read as words -- primary down to tertiary -- and
;; each holds WCAG AA (4.5:1) on every surface a run of text stands on: the
;; grounds, a row under the pointer, a field lit by typing. --faint and --dim
;; are the tones of a rule, a dot, an underline, and --accent-fill of a lamp:
;; marks, never a color:.

(defun web-css-color (text)
  "TEXT, a #rrggbb or rgba() color, as (R G B ALPHA), the channels 0-255."
  (let ((hash (position #\# text)) (*read-default-float-format* 'double-float))
    (if hash
        (append (loop for at from (1+ hash) by 2 repeat 3 collect (parse-integer text :start at :end (+ at 2) :radix 16)) '(1))
        (mapcar #'read-from-string (cl-ppcre:all-matches-as-strings "[0-9.]+" text)))))

(defun web-contrast (a b)
  "The WCAG contrast ratio of two colors."
  (flet ((luminance (color)
           (loop for channel in color repeat 3 for weight in '(0.2126d0 0.7152d0 0.0722d0)
                 sum (* weight (let ((v (/ channel 255d0)))
                                 (if (<= v 0.03928d0) (/ v 12.92d0) (expt (/ (+ v 0.055d0) 1.055d0) 2.4d0)))))))
    (let ((x (luminance a)) (y (luminance b)))
      (/ (+ (max x y) 0.05d0) (+ (min x y) 0.05d0)))))

(defun web-over (color ground)
  "COLOR, its alpha its fourth channel, laid over the opaque GROUND."
  (append (loop for c in color repeat 3 for g in ground collect (+ (* c (fourth color)) (* g (- 1 (fourth color))))) '(1)))

(defun web-token (declarations light name)
  "The color the theme gives NAME: the light block's when LIGHT and it sets one, else the root's."
  (flet ((declared (selector)
           (loop for (rule . declaration) in declarations
                 when (and (string= rule selector) (eql 0 (search (format nil "~a:" name) declaration)))
                   return (web-css-color (subseq declaration (1+ (length name)))))))
    (or (and light (declared ":root[data-theme=\"light\"]")) (declared ":root"))))

(defun web-surfaces (declarations light)
  "Every surface text stands on, each as (NAME . COLOR): the grounds, and what a row under
the pointer and a field lit by typing lay over the ones they stand on."
  (flet ((token (name) (web-token declarations light name)))
    (append (loop for name in '("--void" "--canvas" "--inset" "--subtle" "--selected" "--stage"
                                "--column-ground" "--slab-subtle" "--slab-inset" "--raised")
                  collect (cons name (token name)))
            (loop for fill in '("--field" "--field-lit" "--hover")
                  append (loop for ground in '("--column-ground" "--stage" "--raised")
                               collect (cons (format nil "~a on ~a" fill ground) (web-over (token fill) (token ground))))))))

(deftest web-cell-text-holds-aa-on-every-surface ()
  (let ((declarations (web-css-declarations (web-page-text "app.css"))) (short '()))
    (dolist (light '(nil t))
      (flet ((token (name) (web-token declarations light name))
             (check (what tone ground)
               (when (< (web-contrast tone ground) 4.5)
                 (push (format nil "~a~:[~; (light)~]" what light) short))))
        (loop for (name . surface) in (web-surfaces declarations light)
              do (dolist (tone '("--primary" "--foreground" "--secondary" "--muted" "--tertiary" "--accent" "--danger-lit"))
                   (check (format nil "~a on ~a" tone name) (token tone) surface)))
        ;; A flag stands on a wash of its own tone (13%), a diff line on its fill.
        (dolist (base '("--stage" "--raised" "--slab-inset" "--slab-subtle"))
          (dolist (tone '("--accent" "--danger-lit"))
            (check (format nil "~a on its wash over ~a" tone base) (token tone)
                   (web-over (append (subseq (token tone) 0 3) '(0.13d0)) (token base))))
          (loop for (tone fill) in '(("--diff-add" "--diff-add-fill") ("--diff-del" "--diff-del-fill"))
                do (check (format nil "~a on ~a over ~a" tone fill base) (token tone) (web-over (token fill) (token base)))))))
    (is (null short) (format nil "text under 4.5:1: ~{~a~^, ~}" short))))

(deftest web-cell-text-tones-step-down ()
  (let ((declarations (web-css-declarations (web-page-text "app.css"))))
    (dolist (light '(nil t))
      (let ((ratios (loop with stage = (web-token declarations light "--stage")
                          for tone in '("--primary" "--foreground" "--secondary" "--muted" "--tertiary")
                          collect (web-contrast (web-token declarations light tone) stage))))
        (is (loop for (above below) on ratios while below always (>= (/ above below) 1.15d0))
            (format nil "each text tone a step below the last~:[~; (light)~]: ~{~,2f~^ ~}" light ratios))))))

(deftest web-cell-rule-tones-are-never-text ()
  (let* ((declarations (web-css-declarations (web-page-text "app.css")))
         (painted (loop for (rule . declaration) in declarations
                        when (and (eql 0 (search "color:" declaration))
                                  (some (lambda (tone) (search tone declaration)) '("var(--faint)" "var(--dim)" "var(--accent-fill)")))
                          collect (format nil "~a { ~a }" rule declaration)))
         (faded (loop for (rule . declaration) in declarations
                      when (and (or (search ".dimmed" rule) (search ".jl.same" rule)) (search "opacity" declaration))
                        collect rule)))
    (is (null painted) "no color: takes a rule or fill tone")
    (is (null faded) "what is dimmed is dimmed by tone, never by opacity")))

(deftest web-cell-theme-is-set-before-the-first-paint ()
  ;; A module runs after the page is parsed, and the page may paint before it:
  ;; the theme is a classic script in the head, ahead of the stylesheet.
  (let* ((page (web-page-text "index.html"))
         (theme (search "<script src=\"theme.js\"></script>" page)))
    (is theme "theme.js is a classic script, neither module nor deferred")
    (is (and theme (< theme (search "</head>" page)) (< theme (search "app.css" page))) "in the head, before the stylesheet")
    (is (search "data-choice=\"system\"" page) "the switch offers System")))

;; The transcript while a turn runs, and when it ends. What a running turn
;; takes away (an earlier turn's Rewind and Fork, a failed turn's Retry) keeps
;; its room, so nothing above the answer moves when the turn ends; and whether
;; the reader follows the end is theirs, from their own scrolling, re-applied
;; after every change of size: a reader at the end still sees the answer's last
;; lines after the turn ends, and one away from it has the way back, shown only
;; with the transcript.

(deftest web-cell-a-running-turn-keeps-the-transcripts-room ()
  (let* ((declarations (web-css-declarations (web-page-text "app.css")))
         (busy (remove-if-not (lambda (rule) (search "[data-busy]" (car rule))) declarations)))
    (is busy "app.css has rules for a running turn")
    ;; No rule of a running turn takes a box of the transcript's room away;
    ;; the composer's foot is one row whatever it holds.
    (is (null (loop for (rule . declaration) in busy
                    when (and (not (search ".composer" rule))
                              (cl-ppcre:scan "^(display|height|max-height|margin|padding)" declaration))
                      collect (format nil "~a { ~a }" rule declaration))))
    ;; An earlier turn's Rewind and Fork are hidden in place, and its number stays.
    (is (find '(".app[data-busy] .ask button.rewind" . "visibility: hidden") busy :test #'equal))))

(deftest web-cell-the-reader-at-the-end-stays-there ()
  (let* ((app (web-page-text "app.js"))
         (place (subseq app (search "function placeJump()" app)))
         (place (subseq place 0 (search "}" place))))
    (is (cl-ppcre:scan "new ResizeObserver\\(follow\\)" app) "a change of size re-follows")
    (is (cl-ppcre:scan "settling\\.observe\\(rowsEl\\)" app) "the rows growing, a picture loading")
    (is (cl-ppcre:scan "settling\\.observe\\(ledger\\)" app) "the transcript getting shorter as the box grows")
    ;; Where the reader is comes from their own scrolling, and one distance
    ;; decides both following and the way back.
    (is (cl-ppcre:scan "following = ledger\\.scrollHeight - ledger\\.scrollTop - ledger\\.clientHeight < NEAR_END" app))
    (is (= 1 (length (cl-ppcre:all-matches-as-strings "ledger\\.scrollHeight - ledger\\.scrollTop" app))))
    (is (search "ledger.hidden" place) "the way back shows only with the transcript")
    (is (search "following" place) "and only to a reader away from the end")))

;; An answer's type. Each heading level is a step of size above the next and a
;; weight above the text; emphasis is weight and slant, never tone alone; running
;; text keeps a measure of about 70 characters while a table or a fence takes
;; the lane; and code is never in the dimmest tone. Geist at the body's 15 px
;; sets 6.94 px a character on average (measured on an answer's paragraph), so
;; the measure's pixels say its characters.

(defparameter *web-geist-average-em* 0.4625d0
  "Geist's average advance per character, in ems, over English prose.")

(defun web-css-value (declarations selector property)
  "What SELECTOR's rule gives PROPERTY, trimmed, or NIL."
  (loop for (rule . declaration) in declarations
        when (and (string= rule selector) (eql 0 (search (format nil "~a:" property) declaration)))
          return (string-trim " " (subseq declaration (1+ (length property))))))

(deftest web-cell-an-answer-reads-as-structure ()
  (let* ((declarations (web-css-declarations (web-page-text "app.css")))
         (body (web-css-px declarations ".prose" "font-size"))
         (sizes (mapcar (lambda (level) (web-css-px declarations level "font-size"))
                        '(".prose h1" ".prose h2" ".prose h3" ".prose h4, .prose h5, .prose h6")))
         (measure ".prose > :is(p, ul, ol, blockquote, h1, h2, h3, h4, h5, h6)")
         (width (web-css-px declarations measure "max-width")))
    (is (= 15 body))
    (is (and (every #'integerp sizes) (apply #'> sizes) (>= (car (last sizes)) body)) (format nil "each level a step down: ~a" sizes))
    (is (<= 600 (web-css-px declarations ".prose h1, .prose h2, .prose h3, .prose h4, .prose h5, .prose h6" "font-weight")))
    (is (<= 600 (web-css-px declarations ".prose strong" "font-weight")))
    ;; The page refuses synthesized faces and Geist has no italic of its own:
    ;; emphasis asks for the slant back.
    (is (equal "italic" (web-css-value declarations ".prose em" "font-style")))
    (is (equal "style" (web-css-value declarations ".prose em" "font-synthesis")))
    (is (and width (<= 60 (/ width (* body *web-geist-average-em*)) 80)) (format nil "a measure of 60 to 80 characters: ~a px" width))
    (is (notany (lambda (wide) (search wide measure)) '("pre" "table" "div" "figure")) "a fence, a table and a picture keep the lane")
    (dolist (code '(".prose pre" ".prose code"))
      (is (member (web-css-value declarations code "color") '("var(--foreground)" "var(--secondary)") :test #'equal) code))))

;; The page's words. English is the source (i18n.js): every word the page hands
;; t() as a literal and every word its HTML marks has its Chinese in zh.js, a
;; {name} in one is a {name} in the other, and zh.js keeps nothing the page no
;; longer says. A word handed to t() as anything but a literal is a word this
;; cannot see, so a template never is: its values ride {name}.

(defun web-js-string (text start)
  "The JavaScript string literal whose quote opens at START in TEXT, read."
  (let ((out (make-string-output-stream)) (quote (char text start)) (at (1+ start)))
    (loop for char = (char text at)
          do (cond ((char= char quote) (return (get-output-stream-string out)))
                   ((char= char #\\)
                    (let ((next (char text (incf at))))
                      (case next
                        (#\n (write-char #\Newline out))
                        (#\u (write-char (code-char (parse-integer text :start (1+ at) :end (+ at 5) :radix 16)) out)
                         (incf at 4))
                        (t (write-char next out)))))
                   (t (write-char char out)))
             (incf at))))

(defun web-calls (text)
  "Where TEXT calls t(: each position after its parenthesis."
  (loop for at = (search "t(" text) then (search "t(" text :start2 (1+ at))
        while at
        unless (and (plusp at) (let ((before (char text (1- at))))
                                 (or (alphanumericp before) (find before "_$."))))
          collect (+ at 2)))

(defun web-attribute (tag name)
  "The value TAG gives the attribute NAME, or NIL."
  (let ((at (search (format nil " ~a=\"" name) tag)))
    (when at
      (let ((start (+ at (length name) 3)))
        (subseq tag start (position #\" tag :start start))))))

(defun web-html-text (text)
  "TEXT with the HTML entities the page writes read."
  (loop for (entity . char) in '(("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"") ("&#39;" . "'") ("&amp;" . "&"))
        do (setf text (cl-ppcre:regex-replace-all entity text char))
        finally (return text)))

(defun web-html-words (page)
  "Every word PAGE marks: the text of an element with data-i18n, and each
attribute its data-i18n-attrs names."
  (loop for open = (position #\< page) then (position #\< page :start close)
        for close = (and open (position #\> page :start open))
        while close
        append (let ((tag (subseq page open close)))
                 (append (when (or (search " data-i18n " tag) (search " data-i18n>" (concatenate 'string tag ">")))
                           (list (web-html-text (string-trim '(#\Space #\Newline)
                                                             (subseq page (1+ close) (position #\< page :start close))))))
                         (loop for name in (uiop:split-string (or (web-attribute tag "data-i18n-attrs") "") :separator " ")
                               for value = (and (plusp (length name)) (web-attribute tag name))
                               when value collect (web-html-text value))))))

(defun web-placeholders (text)
  "The {name}s TEXT carries, sorted."
  (sort (cl-ppcre:all-matches-as-strings "\\{\\w+\\}" text) #'string<))

(deftest web-cell-every-word-the-page-says-has-its-chinese ()
  (let* ((zh (web-page-text "zh.js"))
         (catalog (nlk:decode-json (subseq zh (position #\{ zh :start (search "export const ZH" zh))
                                           (1+ (position #\} zh :from-end t)))
                                   :whole t))
         (scripts (loop for file in (directory (merge-pathnames "*.js" nodecode-web::*page-directory*))
                        unless (string= (file-namestring file) "zh.js")
                          collect (uiop:read-file-string file)))
         (calls (loop for text in scripts
                      append (loop for at in (web-calls text) collect (cons text at))))
         (words (remove-duplicates
                 (append (web-html-words (web-page-text "index.html"))
                         (loop for (text . at) in calls
                               when (find (char text at) "\"'") collect (web-js-string text at)))
                 :test #'string=)))
    (let ((templates (loop for (text . at) in calls
                           when (char= (char text at) #\`) collect (subseq text at (min (length text) (+ at 40)))))
          (missing (remove-if (lambda (word) (gethash word catalog)) words))
          (stale (loop for word being the hash-keys of catalog
                       unless (member word words :test #'string=) collect word))
          (unmatched (loop for word being the hash-keys of catalog using (hash-value chinese)
                           unless (equal (web-placeholders word) (web-placeholders chinese)) collect word)))
      (is (null templates) "no t() of a template")
      (is (null missing) "every word the page says has its Chinese")
      (is (null stale) "zh.js keeps nothing the page no longer says")
      (is (null unmatched) "a {name} in the English is one in the Chinese"))))

;; The page asks in the page. A browser's own dialog takes any string, in no
;; style of the page's, and checks nothing: the folder a new session opens in
;; was asked that way, and sessions opened in folders that were not there. The
;; opener names its folder before the first message, and asks the gateway of
;; one typed (GET /api/gateway/folder) before it takes it.

(deftest web-cell-asks-in-the-page-and-names-the-folder-first ()
  (let ((dialogs (loop for file in (directory (merge-pathnames "*.js" nodecode-web::*page-directory*))
                       when (cl-ppcre:scan "\\b(?:window\\.)?(?:prompt|confirm|alert)\\(" (uiop:read-file-string file))
                         collect (file-namestring file))))
    (is (null dialogs) (format nil "no browser dialog: ~{~a~^, ~}" dialogs))
    (is (search "data-i18n>Folder:</span>" (web-page-text "index.html")) "the opener labels its folder in words")
    (is (search "/api/gateway/folder" (web-page-text "app.js")) "and asks the gateway of one typed")))

;; A call's card speaks to the operator (the 2026-09-30 review): its result is
;; the gateway's reading of it (tool.shown), never the text the model read --
;; the offer to set a watch, the background handoff's instructions, a stop's
;; compiler report -- and a stop reads as whose it was, never `failed'.

(deftest web-cell-a-call-card-says-the-operators-words ()
  (let* ((app (web-page-text "app.js"))
         (start (search "function toolAct(row)" app))
         (end (and start (search (format nil "~%}~%") app :start2 start)))
         (body (and end (subseq app start end))))
    (is body "app.js draws a call's card in toolAct")
    (is (search "tool.shown" body) "its result is the gateway's reading")
    (is (not (search "row.text" body)) "never the text the model read")
    (is (search "status === \"stopped\"" body) "a stop is its own end")
    (dolist (word '("Stopped by you after {time}" "Stopped: the gateway shut down while this ran"
                    "moved to the background" "Its output is over the message box while it runs, and under the turn once it ends."))
      (is (search (format nil "t(~s" word) app) word))))

;; What the page keeps in this browser -- a theme, a draft, what each session
;; showed when it was last on screen -- is a convenience: a private window, a
;; preview or site data turned off makes the storage throw, and the page must
;; still draw. Every call on localStorage or sessionStorage stands inside a try
;; block (safeGet is one).

(defun web-guarded-p (text at)
  "Whether a block opened by `try' encloses position AT of TEXT."
  (loop with depth = 0
        for i from (1- at) downto 0
        do (case (char text i)
             (#\} (incf depth))
             (#\{ (if (plusp depth)
                      (decf depth)
                      (let ((head (string-right-trim '(#\Space #\Tab #\Newline)
                                                     (subseq text (max 0 (- i 12)) i))))
                        (when (and (>= (length head) 3)
                                   (string= "try" head :start2 (- (length head) 3)))
                          (return t))))))))

(defun web-unguarded-storage (text)
  "Each localStorage or sessionStorage call in TEXT, a script of the page, that
no enclosing try block holds, as the line it is on."
  (loop for at in (cl-ppcre:all-matches
                   "(?:local|session)Storage\\.(?:getItem|setItem|removeItem|clear)\\(" text)
          by #'cddr
        unless (web-guarded-p text at)
          collect (string-trim " " (subseq text (1+ (or (position #\Newline text :end at :from-end t) -1))
                                        (or (position #\Newline text :start at) (length text))))))

(deftest web-cell-a-refused-storage-never-breaks-the-page ()
  (let ((unguarded (loop for file in (directory (merge-pathnames "*.js" nodecode-web::*page-directory*))
                         append (web-unguarded-storage (uiop:read-file-string file)))))
    (is (null unguarded) (format nil "every storage call is inside a try: ~{~a~^; ~}" unguarded)))
  (let ((bare (format nil "function f() {~%  return localStorage.getItem(KEY);~%}")))
    (is (equal '("return localStorage.getItem(KEY);") (web-unguarded-storage bare)) "and one outside is seen")))

;; /help lists the gateway's commands, every shell's, and the page adds the
;; keys its box answers (KEYS in app.js). Every key the box's keydown handler
;; reads is named there, a modifier it reads with Enter too: a key the page
;; answers and /help never names is how Alt+Enter steered unseen.

(defparameter *web-key-names*
  '(("Enter" . "Enter") ("Tab" . "Tab") ("Escape" . "Esc") ("ArrowUp" . "↑") ("ArrowDown" . "↓"))
  "A keydown event's key -> how /help names it.")

(deftest web-cell-help-names-every-key-the-box-answers ()
  (let* ((js (web-page-text "app.js"))
         (at (search "promptEl.addEventListener(\"keydown\"" js))
         (handler (and at (subseq js at (search (format nil "~%});") js :start2 at))))
         (table (let ((start (search "const KEYS = [" js)))
                  (and start (subseq js start (search (format nil "~%];") js :start2 start)))))
         (named (let ((out '()))
                  (cl-ppcre:do-register-groups (label) ("\\[\"([^\"]+)\"," (or table "")) (push label out))
                  out))
         (answered (remove-duplicates
                    (append (let ((out '()))
                              (cl-ppcre:do-register-groups (key) ("event\\.key === \"([^\"]+)\"" (or handler ""))
                                (push (or (cdr (assoc key *web-key-names* :test #'string=)) key) out))
                              out)
                            (and handler (search "event.altKey" handler) '("Alt+Enter"))
                            (and handler (search "event.shiftKey" handler) '("Shift+Enter")))
                    :test #'string=))
         (unnamed (remove-if (lambda (key) (some (lambda (label) (search key label)) named)) answered)))
    (is handler "the box has its keydown handler")
    (is (subsetp '("Enter" "Tab" "Esc" "↑" "Alt+Enter") answered :test #'string=) "the keys the shell's contract names")
    (is (null unnamed) (format nil "/help names every key the box answers: ~{~a~^, ~} unnamed" unnamed))
    (is (search "helpKeys(line)" js) "and /help's answer carries them")))

;; The phone's hit areas. A mark is 28 px and a fold row 13, under the 32 a
;; finger wants at least and the 44 its primary controls want. A block of app.css
;; grows them, and only behind (pointer: coarse), (max-width: 720px): a desktop
;; with a mouse matches neither and is not moved a pixel. This reads that block,
;; from its marker to its end's: every rule of it stands inside the two
;; conditions, and each control the operator named has at least its size, by
;; the property that sets it.

(defparameter *web-hit-marker* "/* --- a finger's hit areas")
(defparameter *web-hit-end* "/* --- end of a finger's hit areas")

(defparameter *web-hit-areas*
  '((44 ".strip .icon" "width" "height")             ; More, the sessions toggle
    (44 ".ask .under .icon" "width" "height")        ; Rewind, Fork
    (44 ".round" "width" "height")                   ; send, stop, attach, the jump pill, a picture's close
    (44 ".chip" "min-height")                        ; the model
    (44 ".row" "min-height")                         ; a session, New session, Dashboard, Control
    (44 ".work > summary" "min-height")              ; the fold rows: Worked,
    (44 ".act > summary" "min-height")               ; a call,
    (44 ".recap-fold > summary" "min-height")        ; a recap,
    (44 ".card-diff-more > summary" "min-height")    ; a diff's rest,
    (44 ".exec > summary, .exec-line, .bg-open" "min-height") ; an exec, ended or in the band
    (44 ".exec-stop" "width" "height")
    (44 ".quiet, .solid" "min-height")
    (44 ".pop .choice" "min-height")
    (44 ".history .pick, .cell" "min-height")
    (44 ".resume button" "min-height")
    (32 ".icon" "width" "height")                    ; every other mark
    (32 ".looks .icon" "width" "height")             ; the foot's link, look and language
    (32 ".views .view" "min-width" "height")
    (32 ".sw" "min-width" "min-height")
    (32 ".word" "min-width" "min-height")
    (32 ".prose pre .copy" "width" "height")
    (32 ".toast .shut" "width" "height")
    (32 ".attachment .remove" "width" "height")
    (32 ".attach-retry" "min-height")                ; a file that did not go up, sent again
    (32 ".track > button.call" "height")
    (32 ".pill" "width" "height")
    (32 ".cell-link" "min-height")
    (32 ".fs-crumbs button" "min-height"))
  "(MINIMUM SELECTOR PROPERTY...): the block gives SELECTOR each PROPERTY at MINIMUM px or more.")

(defun web-css-preludes (css)
  "What CSS opens each of its top-level blocks with (comments already dropped), in order."
  (let ((depth 0) (start 0) (preludes '()))
    (loop for char across css
          for at from 0
          do (case char
               (#\{ (when (zerop depth)
                      (push (string-trim '(#\Space #\Tab #\Newline) (subseq css start at)) preludes))
                    (incf depth))
               (#\} (decf depth)
                (when (zerop depth) (setf start (1+ at))))))
    (nreverse preludes)))

(defun web-css-px (declarations selector property)
  "The whole pixels SELECTOR's rule gives PROPERTY, or NIL."
  (loop for (rule . declaration) in declarations
        when (and (string= rule selector)
                  (cl-ppcre:scan (format nil "^~a\\s*:\\s*-?[0-9]" property) declaration))
          return (parse-integer declaration :start (1+ (position #\: declaration)) :junk-allowed t)))

(defun web-hit-shortfalls (declarations)
  "What *WEB-HIT-AREAS* asks and the block does not give, one sentence each."
  (loop for (minimum selector . properties) in *web-hit-areas*
        append (loop for property in properties
                     for px = (web-css-px declarations selector property)
                     unless (and px (>= px minimum))
                       collect (format nil "~a ~a is ~a, wants ~d" selector property (or px "unset") minimum))))

(deftest web-cell-the-phones-controls-are-a-fingers-size ()
  (let* ((css (web-page-text "app.css"))
         (at (search *web-hit-marker* css))
         (end (and at (search *web-hit-end* css :start2 at)))
         (hits (and end (subseq css at end)))
         (preludes (and hits (web-css-preludes (cl-ppcre:regex-replace-all "(?s)/\\*.*?\\*/" hits ""))))
         (shortfalls (and hits (web-hit-shortfalls (web-css-declarations hits)))))
    (is at "app.css has the hit-area block")
    (is end "and says where it ends")
    (is (equal preludes '("@media (pointer: coarse), (max-width: 720px)" "@media (max-width: 720px)")) "every rule of it is behind the finger or the phone width: a mouse on a wide window moves not a pixel")
    (is (and hits (null shortfalls)) (format nil "each control the operator named has its size: ~{~a~^; ~}" shortfalls))))

;; A model or reasoning pick in the chat drawer is its session's own (webfix4
;; flow-03, first-03, chat-11, flow-05; 2026-09-30): it once moved the default,
;; every session that followed it and every scheduled job, and said nothing.
;; Control is the one place the page moves the default; the drawer says where
;; its pick lands and names the default; reasoning has a way back to Default;
;; the chip reads again when a pick lands anywhere and when the chat returns.

(defun web-function-text (text name)
  "The source of TEXT's top-level function NAME, up to the next top-level form."
  (let* ((at (or (search (format nil "~%async function ~a(" name) text)
                 (search (format nil "~%function ~a(" name) text)))
         (end (and at (search (format nil "~%}~%") text :start2 at))))
    (and at end (subseq text at end))))

(deftest web-cell-a-drawer-pick-lands-on-its-session-and-the-default ()
  ;; The chat's pick goes through the session route, which pins the session
  ;; and moves the default new sessions start on (SELECT-SESSION-MODEL);
  ;; Control's Settings moves the default alone.
  (let* ((app (web-page-text "app.js"))
         (html (web-page-text "index.html"))
         (choose (web-function-text app "chooseModel"))
         (submit (web-function-text app "submit"))
         (models (web-function-text app "renderModels")))
    (is (search "api(\"POST\", \"/api/gateway/model\"" (web-page-text "control.js")) "Control moves the default")
    (is (not (search "\"/api/gateway/model\"" app)) "and the chat never does")
    (is (and choose (search "/model`" choose) (search "if (!session)" choose)
             (< (search "if (!session)" choose) (search "api(" choose))) "a pick on the opener waits for the session, one in a session pins it")
    (is (and submit (search "landAim(session)" submit)
             (< (search "landAim(session)" submit) (search "sendPrompt(" submit))) "the opener's pick lands on the new session before its first message")
    (is (and models (search "t(\"This session\")" models) (search "t(\"The new session\")" models)
             (search "becomes the default for new sessions" models)) "the drawer says where a pick lands")
    (is (and models (search "t(\"Default (the model's own)\")" models)
             (search "effort: \"\"" models)) "reasoning's first row is Default, and it clears the pick")
    (is (search "id=\"pickscope-go\"" html) "the drawer says where the default changes")
    (is (search "case \"model_changed\": if (!payload.session_id || payload.session_id === state.current) loadTarget();" app) "the chip reads again when a pick or the default moves anywhere")
    (is (search "if (pane === \"chat\") { schedule(); loadTarget(); }" app) "and when the chat comes back from a board")))

;; A Control form's save. A refused one keeps what was typed and says why under
;; the form, the hand put back on the field the gateway names: control.js's ACT
;; does that for a save that names its form (FORMKEY), and a toast is left for a
;; control that is not a form. This reads every submit handler of the pane's
;; tabs and holds each save it starts to naming its form; the MCP and Skills
;; tabs say their own refusals where the write was made, and are not read. A
;; Retry repeats the same request, so the pane offers one only where the gateway
;; never answered: never beside a refusal.

(defun web-js-literal-end (text start)
  "Where the string literal or template whose quote opens at START in TEXT
ends: past its closing quote."
  (let ((quote (char text start)) (at (1+ start)))
    (loop (let ((char (char text at)))
            (cond ((char= char #\\) (incf at 2))
                  ((char= char quote) (return (1+ at)))
                  (t (incf at)))))))

(defun web-js-close (text open)
  "Where the bracket that opens at OPEN in TEXT closes, literals stepped over."
  (let ((depth 0) (at open))
    (loop (let ((char (char text at)))
            (if (find char "\"'`")
                (setf at (web-js-literal-end text at))
                (progn (cond ((find char "({[") (incf depth))
                             ((and (find char ")}]") (zerop (decf depth))) (return at)))
                       (incf at)))))))

(defun web-js-calls (text name)
  "Each call of the function NAME in TEXT, whole, from its name to its parenthesis."
  (loop for at = (search name text) then (search name text :start2 (1+ at))
        while at
        unless (let ((before (char text (1- at)))) (or (alphanumericp before) (find before "_$.")))
          collect (subseq text at (1+ (web-js-close text (+ at (length name) -1))))))

(deftest web-cell-control-forms-keep-their-refusal-under-them ()
  (dolist (file '("control.js" "settings.js" "access.js" "profiles.js"))
    (let* ((text (web-page-text file))
           (open (position #\{ text :start (search "function submit(" text)))
           (body (subseq text open (1+ (web-js-close text open))))
           (saves (loop for name in '("act(" "jobOp(" "pair(") append (web-js-calls body name)))
           (bare (remove-if (lambda (call) (uiop:string-suffix-p call ", formKey(node))")) saves)))
      (is saves (format nil "~a's submit starts a save" file))
      (is (null bare) (format nil "every save ~a's forms start names its form: ~{~a~^; ~}" file bare))))
  (let ((retries (loop for file in '("control.js" "settings.js" "mcp.js" "index.js" "profiles.js" "access.js")
                       for text = (web-page-text file)
                       append (loop for at = (search "retry:" text) then (search "retry:" text :start2 (1+ at))
                                    while at collect (subseq text at (min (length text) (+ at 30)))))))
    (is retries "the pane offers a Retry somewhere")
    (is (every (lambda (retry) (search "unreached(" retry)) retries) "only where the gateway never answered")))

;; What rides a prompt. A picture sent with it is kept by the gateway with the
;; prompt (exec.lisp PROMPT-IMAGES), so the operator's own line draws it from
;; the row it is sent, live and after a reload alike, and a rewind or a Retry
;; fetches it back by the id that row names: the tab keeps no copy of its own.
;; Any other file waits in the box, its chip saying where it will land, and
;; goes up at Send; the chip's x stops one on its way, and one that did not go
;; up offers Retry and says, where Send is, why the box will not send.

(defun web-js-function (text name)
  "The source of the top-level function NAME in TEXT, from its head to the
brace that closes it at the start of a line, or NIL."
  (let ((at (search (format nil "function ~a(" name) text)))
    (when at
      (subseq text at (+ 2 (search (format nil "~%}~%") text :start2 at))))))

(defun web-js-listener (text element)
  "The source of the click listener TEXT adds to the element ELEMENT."
  (let ((at (search (format nil "$(\"~a\").addEventListener(\"click\"" element) text)))
    (when at
      (subseq text at (+ 3 (search (format nil "~%});~%") text :start2 at))))))

(deftest web-cell-a-picture-sent-stays-in-the-operator-s-line ()
  (let* ((js (web-page-text "app.js"))
         (ask (web-js-function js "drawAsk")))
    (is (search "turn.prompt.metadata?.images" ask) "the operator's line reads the pictures its row names")
    (is (search "figureOf(image, image.id)" ask) "each drawn as a figure fetched by its id, whole on a click")
    (is (search "promptPictures(" (web-js-function js "retry")) "a Retry sends the pictures back with the words")
    (is (search "putBackPictures(" (web-js-function js "navigated")) "a rewind puts them back in the box")
    (is (search "no longer on disk" (web-js-function js "putBackPictures")) "and says so of one it cannot")
    (is (null (search "state.prompted" js)) "the tab keeps no copy of a sent picture")))

(deftest web-cell-a-file-goes-up-at-send-where-its-chip-says ()
  (let* ((js (web-page-text "app.js"))
         (files (web-page-text "files.js"))
         (pick (web-js-function js "addFile"))
         (up (web-js-function js "putUp"))
         (send (web-js-function js "filesUp"))
         (chips (web-js-listener js "attachments"))
         (strays (loop for at = (search "putUp(" js) then (search "putUp(" js :start2 (1+ at))
                       while at
                       unless (loop for part in (list up send chips)
                                    for start = (search part js)
                                    thereis (<= start at (+ start (length part))))
                         collect at)))
    (is (and pick (null (search "putUp(" pick))) "picking a file writes nothing")
    (is (search "placeFiles(" pick) "it reads where the file will land")
    (is (search "placeFiles(files, session, true)" (web-js-function js "submit")) "and again at Send, which holds when the place moved")
    (is (null strays) "a file goes up from Send or its chip's Retry, nowhere else")
    (is (search "entry.stop()" chips) "the x stops a file on its way")
    (is (and (search "signal?.addEventListener(\"abort\"" files) (search "xhr.abort()" files)) "and putFile stops its bytes")
    (is (search "\"attach-retry\"" (web-js-function js "paintFile")) "a file that did not go up offers Retry on its chip")
    (is (search "aria-describedby" (web-js-function js "renderComposer")) "and Send is described by why it will not send")
    (is (search "Retry or remove {name} to send" (web-js-function js "attachWhy")) "in words")))

;; The link outside its tab. A browser asking is a notice that holds, with the
;; code and Allow and Deny, over the others until it is answered or out of
;; time, and the foot's mark shows it at once: every notice reads the link
;; again (the cell's own reading, never the notice's words). A link whose
;; dials keep failing says so with a Try now, and each state reads still, not
;; by motion alone.

(deftest web-cell-a-browser-asking-holds-a-notice-and-the-mark-shows-it ()
  (let* ((app (web-page-text "app.js"))
         (access (web-page-text "access.js"))
         (css (web-page-text "app.css"))
         (receive (web-js-function app "receive"))
         (notice (and receive (search "case \"organism.notice\":" receive))))
    ;; A notice reads the link again.
    (is (and notice (search "readLinkSoon();" receive :start2 notice)
             (< (search "readLinkSoon();" receive :start2 notice) (search "break;" receive :start2 notice))))
    (is (search "control.link.read().then(paintLink)" (web-js-function app "readLinkSoon")))
    (is (search "paintAsk();" (web-js-function app "paintLink")) "a reading paints the ask where the operator is")
    (is (search "control.link.askNotice()" (web-js-function app "paintAsk")))
    ;; A flood of news never takes the ask off the stack.
    (is (search "!each.matches(\".error, .link-ask\")" (web-js-function app "toast")))
    (is (search "function askNotice()" access))
    (is (search "(seen() || c.link?.ask)" access) "an ask is read again while it waits, so one answered elsewhere leaves")
    (is (search "failing: [t(\"Failing.\")" access) "a failing link has its own state")
    (is (search "act: \"link-retry\"" access) "and a Try now")
    ;; Failing is a hollow mark and asking a ringed one: each reads still,
    ;; with motion reduced too.
    (is (cl-ppcre:scan "(?s)\\.link-mark\\[data-state=\"failing\"\\]::after \\{[^}]*border: 1.5px solid var\\(--danger-lit\\)" css))
    (is (cl-ppcre:scan "(?s)\\.link-mark\\[data-asking\\]::after \\{[^}]*border: 1.5px solid var\\(--accent\\)" css))))

;; The slash reply. A command the catalog says acts on no session runs from the
;; opener as it is, and no untitled session is left behind; the panel it
;; answers with is drawn under its line, a row's command one click from the box.

(deftest web-cell-a-sessionless-command-runs-without-a-session-and-shows-its-panel ()
  (let* ((app (web-page-text "app.js"))
         (run (web-js-function app "runSlash"))
         (panel (web-js-function app "slashPanel")))
    (is (search "slashCommand(line)?.session !== false" run) "the catalog's word decides")
    (is (search "(needs ? await newSession(state.folder) : null)" run) "only a command that needs one opens a session")
    (is (search ", \"\", answer?.dialog);" run) "the panel rides the reply")
    (is (search "qrCode(dialog.picture.rows)" panel) "its picture")
    (is (search "pick.dataset.value = row.value" panel) "its rows")
    (is (search "id=\"slash-panel\"" (web-page-text "index.html")))))
