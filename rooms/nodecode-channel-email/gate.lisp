;;;; gate.lisp --- whether one message may open a turn, and where it belongs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure. Email's sender is a claim anyone can type, so a message reaches
;;;; the kit only when the receiving server vouched for its From domain
;;;; (Authentication-Results), it was written by a person and not a machine
;;;; (RFC 3834's auto-replies, lists, bounces), and its sender is on the list.
;;;; What passes is placed: one email thread is one room.

(in-package #:nodecode-channel-email)

;;; --- who sent it ----------------------------------------------------------------

(defun from-address (mail)
  "MAIL's sender as (values ADDRESS NAME), the address downcased."
  (parse-address (or (header mail "from") "")))

(defun domain-of (address)
  (let ((at (position #\@ address :from-end t)))
    (string-downcase (string-trim "<>. " (if at (subseq address (1+ at)) address)))))

(defun aligned-p (from-domain domain)
  "Whether DOMAIN speaks for FROM-DOMAIN: the same domain, or one the other's
parent (DMARC's relaxed alignment, without the public-suffix list)."
  (and (plusp (length domain)) (plusp (length from-domain))
       (or (string= from-domain domain)
           (uiop:string-suffix-p from-domain (concatenate 'string "." domain))
           (uiop:string-suffix-p domain (concatenate 'string "." from-domain)))))

(defun sender-allowed-p (address allowed)
  "Whether ADDRESS is on ALLOWED: an entry is an address, or @domain for
every address at exactly that domain."
  (and address
       (some (lambda (entry)
               (let ((entry (string-downcase (trim entry))))
                 (if (and (> (length entry) 1) (char= (char entry 0) #\@))
                     (uiop:string-suffix-p address entry)
                     (string= address entry))))
             allowed)
       t))

(defun addressed-to-p (mail address)
  "Whether MAIL names ADDRESS among its recipients."
  (let ((address (string-downcase address)))
    (and (some (lambda (name)
                 (some (lambda (value)
                         (find address (parse-address-list value) :key #'car :test #'string=))
                       (header-values mail name)))
               '("to" "cc" "delivered-to" "x-original-to" "envelope-to"))
         t)))

;;; --- written by a machine -------------------------------------------------------

(defun automated-p (mail &key self)
  "Why MAIL is a machine's and gets no answer -- an RFC 3834 auto-reply, a
list's, a bounce, a delivery report, the bot's own (SELF, its address) -- or
NIL for a person's."
  ;; An answer to any of these is how two robots mail each other forever.
  (let ((from (from-address mail))
        (submitted (header mail "auto-submitted")))
    (cond ((and submitted (not (string-equal "no" (trim (first (split-outside submitted #\;))))))
           "auto-submitted")
          ((ppcre:scan "(?i)^\\s*(bulk|list|junk|auto_reply)" (or (header mail "precedence") ""))
           "bulk precedence")
          ((or (header mail "list-id") (header mail "list-unsubscribe") (header mail "list-post"))
           "mailing list")
          ((or (header mail "x-autoreply") (header mail "x-autorespond"))
           "auto-reply")
          ((string= "<>" (trim (or (header mail "return-path") "")))
           "bounce")
          ((ppcre:scan "(?i)^multipart/report" (or (header mail "content-type") ""))
           "delivery report")
          ((null from) "no sender")
          ((ppcre:scan "(?i)^(mailer-daemon|postmaster|no-?reply|do-?not-?reply|bounces?)[+@]" from)
           "system sender")
          ((and self (string= from (string-downcase self))) "own message"))))

;;; --- vouched for ----------------------------------------------------------------

(defun strip-comments (text)
  (loop for next = (ppcre:regex-replace-all "\\([^()]*\\)" text "")
        until (string= next text)
        do (setf text next)
        finally (return text)))

(defun auth-results (value)
  "An Authentication-Results VALUE as (authserv-id . results), each result
(method verdict ((property . value) ...)), all downcased."
  (let ((parts (mapcar #'trim (split-outside (strip-comments value) #\;))))
    (cons (string-downcase (first (ppcre:split "\\s+" (first parts))))
          (loop for part in (rest parts)
                for tokens = (ppcre:split "\\s+" part)
                for head = (first tokens)
                for eq = (and head (position #\= head))
                when eq
                  collect (list (string-downcase (subseq head 0 eq))
                                (string-downcase (subseq head (1+ eq)))
                                (loop for token in (rest tokens)
                                      for e = (position #\= token)
                                      when e
                                        collect (cons (string-downcase (subseq token 0 e))
                                                      (string-downcase (unquote (subseq token (1+ e)))))))))))

(defun authenticated-p (mail &key authserv)
  "Whether the receiving server vouched for MAIL's From domain, as (values
OK-P HOW): DMARC passed, or DKIM or SPF passed for an aligned domain. Only
the topmost Authentication-Results counts -- the last hop's, stamped above
anything the sender wrote -- and, given AUTHSERV, only when that server
signed it."
  (let* ((from (from-address mail))
         (domain (and from (domain-of from)))
         (value (first (header-values mail "authentication-results")))
         (parsed (and value (auth-results value))))
    (flet ((aligned (method key)
             (find-if (lambda (result)
                        (and (string= method (first result))
                             (string= "pass" (second result))
                             (let ((property (cdr (assoc key (third result) :test #'string=))))
                               (and property (aligned-p domain (domain-of property))))))
                      (rest parsed))))
      (cond ((null domain) (values nil "no sender"))
            ((null parsed) (values nil "no Authentication-Results"))
            ((and authserv (not (string-equal authserv (car parsed))))
             (values nil (format nil "stamped by ~a, not ~a" (car parsed) authserv)))
            ((find-if (lambda (result)
                        (and (string= "dmarc" (first result))
                             (string= "pass" (second result))
                             (let ((from-domain (cdr (assoc "header.from" (third result) :test #'string=))))
                               (or (null from-domain) (aligned-p domain (domain-of from-domain))))))
                      (rest parsed))
             (values t "dmarc"))
            ((or (aligned "dkim" "header.d") (aligned "dkim" "header.i")) (values t "dkim"))
            ((aligned "spf" "smtp.mailfrom") (values t "spf"))
            (t (values nil (format nil "~:[nothing passed~;~:*~{~a~^, ~}~]"
                                   (mapcar (lambda (r) (format nil "~a=~a" (first r) (second r)))
                                           (rest parsed)))))))))

;;; --- where it belongs -----------------------------------------------------------

(defun hex-id (text)
  "16 hex digits naming TEXT: an id the kit's session names carry whole (no
hyphen in it)."
  (let ((digest (nlk:sha256-text text)))
    (subseq digest (1+ (or (position #\: digest) -1)) (+ 17 (or (position #\: digest) -1)))))

(defun own-message-id (mail)
  "MAIL's Message-ID, or one made from its headers when it carries none."
  (or (first (message-ids (header mail "message-id")))
      (format nil "<~a@nodecode.invalid>" (hex-id (format nil "~s" (mail-headers mail))))))

(defun mail-id (message-id)
  "The kit's message id for MESSAGE-ID, a <...> header value."
  (hex-id (string-downcase message-id)))

(defun thread-root (mail)
  "The Message-ID that opened MAIL's thread: the first References names it,
else what it replies to, else MAIL itself."
  (or (first (message-ids (header mail "references")))
      (first (message-ids (header mail "in-reply-to")))
      (own-message-id mail)))

(defun thread-channel (sender root)
  "The room id of SENDER's thread opened by ROOT: one per person and thread,
so a forged References never reaches someone else's conversation."
  (hex-id (format nil "~a|~a" (string-downcase sender) (string-downcase root))))
