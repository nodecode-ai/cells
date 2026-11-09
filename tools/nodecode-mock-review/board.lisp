;;;; board.lisp --- the review board: one page, its mocks, and its Send.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The board is served from the gateway's own port under /mock-review/,
;;;; public because a browser page cannot carry the operator token. What
;;;; stands in for it is a per-review submit token: the page it was rendered
;;;; with carries it, and a POST without it is refused, so another page open
;;;; in the same browser cannot answer a review it was never given.
;;;;
;;;; A mock is served as its own document and framed with sandbox="allow-scripts
;;;; allow-forms", a unique origin: it can be a live prototype and still read
;;;; nothing of the board around it. Nothing of a mock is ever inlined into
;;;; the board's own document.

(in-package #:nodecode-mock-review)

(defparameter +page-headers+
  '(:cache-control "no-cache"
    :x-content-type-options "nosniff"
    :referrer-policy "no-referrer"
    :content-security-policy
    "default-src 'none'; style-src 'unsafe-inline'; script-src 'unsafe-inline'; img-src 'self' data:; frame-src 'self'; form-action 'none'; base-uri 'none'")
  "What the board itself is sent with: nothing loaded from anywhere but here.")

(defparameter +mock-headers+
  '(:cache-control "no-cache"
    :x-content-type-options "nosniff"
    :content-security-policy "default-src 'self' data: blob: 'unsafe-inline' 'unsafe-eval'")
  "What a mock is sent with: its own page's images and inline code, and no way out.")

(defun escape-html (text)
  "TEXT as HTML text: the five characters that leave the text are escaped."
  (with-output-to-string (out)
    (loop for char across (or text "")
          do (case char
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&#39;" out))
               (t (write-char char out))))))

(defun decisions-of (round)
  (coerce (nlk:json-array round "decisions") 'list))

(defun options-of (decision)
  (coerce (nlk:json-array decision "options") 'list))

(defun option-key (decision option)
  "The URL name of OPTION inside DECISION: letters and digits of the two ids, joined by --."
  (format nil "~a--~a" (nlk:json-value decision :string "id") (nlk:json-value option :string "id")))

(defun number-of-options (round)
  (reduce #'+ (decisions-of round) :key (lambda (decision) (length (options-of decision))) :initial-value 0))

;;; --- the page ---------------------------------------------------------------

(defparameter +page-style+
  "body{font:15px/1.5 system-ui,sans-serif;margin:0;background:#f6f6f7;color:#16181d}"
  "The board's own look, when no project design profile is in play.")

(defun decision-card (out data decision)
  "One decision: its question, its options' mocks, the three marks and a note."
  (declare (ignore data))
  (let ((id (nlk:json-value decision :string "id"))
        (recommended (nlk:json-value decision :string "recommended")))
    (format out "<section class=\"card\" data-decision=\"~a\">~%" (escape-html id))
    (format out "<h2>~a</h2>~%" (escape-html (nlk:json-value decision :string "question")))
    (format out "<div class=\"options\">~%")
    (dolist (option (options-of decision))
      (let ((key (option-key decision option))
            (label (nlk:json-value option :string "label"))
            (mock (nlk:json-value option :string "mock")))
        (format out "<label class=\"option\"><input type=\"radio\" name=\"choice-~a\" value=\"~a\">~%"
                (escape-html id) (escape-html key))
        (format out "<span class=\"label\">~a~@[ <em>(recommended)</em>~]</span></label>~%"
                (escape-html label)
                (and recommended (equal recommended (nlk:json-value option :string "id"))))
        (when (plusp (length (or mock "")))
          (format out "<iframe class=\"mock\" sandbox=\"allow-scripts allow-forms\" loading=\"lazy\" src=\"m/~a.html\"></iframe>~%"
                  (escape-html key)))))
    (format out "</div>~%")
    (format out "<div class=\"marks\">~%")
    (dolist (mark '(#("approve" "Approve") #("change" "Change") #("skip" "Skip")))
      (format out "<label><input type=\"radio\" name=\"mark-~a\" value=\"~a\">~a</label>~%"
              (escape-html id) (aref mark 0) (aref mark 1)))
    (format out "</div>~%")
    (format out "<textarea name=\"note-~a\" rows=\"2\" placeholder=\"why, or what to change\"></textarea>~%"
            (escape-html id))
    (format out "</section>~%")))

(defvar +page-script+ nil
  "The page's one script, defined below the page that writes it out.")

(defun board-page (data round)
  "The board's document for DATA's ROUND."
  (let ((id (nlk:json-value data :string "id")))
    (with-output-to-string (out)
      (format out "<!doctype html>~%<html lang=\"en\"><head><meta charset=\"utf-8\">")
      (format out "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">~%")
      (format out "<title>~a — review round ~a</title>~%"
              (escape-html (nlk:json-value data :string "title"))
              (nlk:json-value round :integer "n"))
      (format out "<style>~a~%" +page-style+)
      (format out "main{max-width:1100px;margin:0 auto;padding:24px}.card{background:#fff;border:1px solid #d9dbe0;border-radius:10px;padding:16px;margin:16px 0}.card h2{font-size:16px;margin:0 0 12px}.options{display:grid;gap:12px;grid-template-columns:repeat(auto-fit,minmax(260px,1fr))}.option{display:block;border:1px solid #e3e5e9;border-radius:8px;padding:8px}.option .label{font-weight:600}.mock{width:100%;height:320px;border:1px solid #e3e5e9;border-radius:6px;background:#fff}.marks{display:flex;gap:16px;margin:12px 0 8px}textarea{width:100%;font:inherit;border:1px solid #d9dbe0;border-radius:6px;padding:8px}footer{position:sticky;bottom:0;background:#fff;border-top:1px solid #d9dbe0;padding:12px 24px;display:flex;gap:16px;align-items:center}button{font:inherit;padding:8px 18px;border:0;border-radius:8px;background:#1d4ed8;color:#fff;cursor:pointer}#said{margin-left:auto;color:#166534}@media(prefers-color-scheme:dark){body{background:#101216;color:#e6e8ec}.card,footer{background:#181b21;border-color:#2a2f38}textarea,.mock{background:#0d0f13;color:inherit;border-color:#2a2f38}}")
      (format out "</style></head><body>~%<main>~%")
      (format out "<h1>~a</h1>~%" (escape-html (nlk:json-value data :string "title")))
      (let ((brief (nlk:json-value data :string "brief")))
        (when (and brief (plusp (length brief)))
          (format out "<p>~a</p>~%" (escape-html brief))))
      (format out "<p class=\"muted\">Round ~a · ~a~:[~; · answered~]</p>~%"
              (nlk:json-value round :integer "n")
              (nlk:json-value round :string "created_at")
              (and (hash-table-p (nlk:json-value round :any "feedback")) t))
      (format out "<form id=\"board\">~%")
      (dolist (decision (decisions-of round))
        (decision-card out data decision))
      (format out "<section class=\"card\"><h2>Anything else</h2>")
      (format out "<textarea id=\"general\" rows=\"3\" placeholder=\"a note on the round as a whole\"></textarea></section>~%")
      (format out "</form></main>~%")
      (format out "<footer><button id=\"send\" form=\"board\">Send feedback</button>")
      (format out "<span id=\"said\"></span></footer>~%")
      (format out "<script>const review=~s,token=~s,round=~a;~%" id (nlk:json-value data :string "token")
              (nlk:json-value round :integer "n"))
      (format out "~a~%" +page-script+)
      (format out "</script></body></html>~%"))))

(defparameter +page-script+
  "document.getElementById('send').addEventListener('click',async e=>{
  e.preventDefault();
  const fb={decisions:{},note:document.getElementById('general').value};
  document.querySelectorAll('[data-decision]').forEach(card=>{
    const id=card.dataset.decision;
    const mark=card.querySelector('input[name=\"mark-'+CSS.escape(id)+'\"]:checked');
    const choice=card.querySelector('input[name=\"choice-'+CSS.escape(id)+'\"]:checked');
    const note=card.querySelector('textarea[name=\"note-'+CSS.escape(id)+'\"]');
    if(mark||choice||(note&&note.value.trim()))
      fb.decisions[id]={mark:mark?mark.value:'',choice:choice?choice.value.split('--').pop():'',note:note?note.value:''};
  });
  const said=document.getElementById('said');
  const r=await fetch('submit',{method:'POST',headers:{'content-type':'application/json'},
    body:JSON.stringify({token:token,round:round,feedback:fb})});
  const answer=await r.json().catch(()=>({error:'the board gave no answer'}));
  said.textContent=answer.error?answer.error:'Sent. The agent has it.';
  said.style.color=answer.error?'#b91c1c':'#166534';
});"
  "The page's one script: collect the marks, send them, say what came back.")

;;; --- the route ---------------------------------------------------------------

(defun query-value (env name)
  "NAME's value in the request's query string, or NIL."
  (let ((query (getf env :query-string)))
    (when (and (stringp query) (plusp (length query)))
      (loop for pair in (uiop:split-string query :separator "&")
            for at = (position #\= pair)
            when (and at (string= name (subseq pair 0 at)))
              do (return (subseq pair (1+ at)))))))

(defun request-body (env)
  "The request's body as a string, at most a megabyte: a reviewer's marks, never a file."
  (let* ((length (or (getf env :content-length) 0))
         (stream (getf env :raw-body))
         (text (and stream (plusp length) (make-string (min length 1000000)))))
    (if text (progn (read-sequence text stream) text) "")))

(defun answered (status body &optional (type "application/json"))
  (list status (list :content-type type :cache-control "no-cache" :x-content-type-options "nosniff")
        (list body)))

(defun path-parts (env)
  "The parts after /mock-review/: the review id, then its sub-path."
  (uiop:split-string (string-trim "/" (or (getf env :path-info) "")) :separator "/"))

(defun serve-board (env)
  "GET /mock-review/ID/ answers the board; POST /mock-review/ID/submit takes the marks."
  (handler-case
      (let* ((parts (path-parts env))
             (id (second parts))
             (rest (cddr parts)))
        (cond
          ((null id) (answered 404 "{\"error\":\"no review named\"}"))
          (t
           (let ((data (load-review id)))
             (cond
               ((and (equal (first rest) "submit") (eq (getf env :request-method) :post))
                (submit-feedback data (request-body env)))
               ((and (equal (first rest) "m") (second rest))
                (serve-mock data (second rest)))
               ((or (null rest) (equal (first rest) ""))
                (let ((round (round-by-number data (query-value env "round"))))
                  (answered 200 (board-page data round) "text/html; charset=utf-8")))
               (t (answered 404 "{\"error\":\"no such place on the board\"}")))))))
    (mock-review-error (condition)
      (answered 404 (format nil "{\"error\":~s}"
                            (princ-to-string (mock-review-error-detail condition)))))))

(defun serve-mock (data key)
  "The mock KEY names, from the newest round that carries one."
  (loop for round in (rounds data)
        do (loop for decision in (decisions-of round)
                 do (loop for option in (options-of decision)
                          when (equal key (option-key decision option))
                            do (return-from serve-mock
                                 (answered 200 (nlk:json-value option :string "mock")
                                           "text/html; charset=utf-8")))))
  (answered 404 "{\"error\":\"no such mock\"}"))

(defun submit-feedback (data body &aux (raw (ignore-errors (nlk:decode-json body))))
  "Store the marks BODY carries against DATA's newest round, or answer why not."
  (handler-case
      (progn
        (unless (hash-table-p raw)
          (error 'mock-review-error :detail "a submission must be a JSON object"))
        (unless (equal (nlk:json-value raw :string "token") (nlk:json-value data :string "token"))
          (error 'mock-review-error
                 :detail "this page was not the one this board handed out: open the review link again"))
        (let ((round (round-by-number data (nlk:json-value raw :integer "round"))))
          (setf (gethash "feedback" round)
                (normalize-feedback data round (nlk:json-value raw :any "feedback")))
          (save-review data)
          (answered 200 (format nil "{\"ok\":true,\"round\":~a}"
                                (nlk:json-value round :integer "n")))))
    (mock-review-error (condition)
      (answered 400 (format nil "{\"error\":~s}"
                            (princ-to-string (mock-review-error-detail condition)))))))
