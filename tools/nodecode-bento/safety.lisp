;;;; safety.lisp --- a preview first, a keyed token, an audit line, then the send.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's safety.py, cut to the two rules a cell can enforce by
;;;; itself: every Tier 2 and Tier 3 call is a dry run first, and the real
;;;; call must carry a confirm_token bound to the exact request that dry run
;;;; printed -- so a token cannot be spent on a different payload, and nothing
;;;; is sent by a call that did not preview it. The token is SHA-256 over the
;;;; tool, the encoded request and an expiry, keyed by the secret key, so it
;;;; needs no state to check and no run can forge one. Every write that
;;;; reaches Bento leaves one line in the cell's audit log.

(in-package #:nodecode-bento)

(defparameter +confirm-ttl+ 900
  "Seconds a confirm_token may be spent for, as the original's 15 minutes.")
(defparameter +audit-file+ "bento/audit.jsonl"
  "The audit log, under the home: one JSON line a write, no keys, no bodies.")

;;; --- the confirm token -------------------------------------------------------------

(defun token-key ()
  "The secret a token is keyed with: the secret key, bound to this site."
  (format nil "~a~c~a" (or (credential (setting :secret-key) "BENTO_SECRET_KEY") "")
          #\Nul (site-uuid)))

(defun token-digest (tool payload expiry)
  "The digest binding TOOL, PAYLOAD's exact bytes and EXPIRY to this site."
  (subseq (nlk:sha256-text
           (format nil "~a~c~a~c~d~c~a"
                   tool #\Nul (nlk:encode-json-object payload) #\Nul expiry #\Nul (token-key)))
          7))

(defun confirm-token (tool payload)
  "A token for the real call of TOOL with PAYLOAD, good for +CONFIRM-TTL+."
  (let ((expiry (+ (get-universal-time) +confirm-ttl+)))
    (format nil "~d.~a" expiry (token-digest tool payload expiry))))

(defun check-confirm (tool payload token)
  "Refuse unless TOKEN was issued for this TOOL and these exact PAYLOAD bytes
and has not expired."
  (let* ((token (nonblank token))
         (dot (and token (position #\. token)))
         (expiry (and dot (ignore-errors (parse-integer token :end dot))))
         (digest (and dot (subseq token (1+ dot)))))
    (unless (and expiry digest)
      (fail "this call needs a confirm_token: run the same call with dry_run=true first"))
    (when (< expiry (get-universal-time))
      (fail "the confirm_token expired (it lives ~d minutes): run the dry run again"
            (round +confirm-ttl+ 60)))
    (unless (string= digest (token-digest tool payload expiry))
      (fail "payload changed since preview: run the dry run again with these exact parameters"))))

;;; --- the audit log ----------------------------------------------------------------

(defun audit-write (tool tier path status)
  "Append one line about a write that reached Bento; a log that cannot be
written costs the line, never the call."
  (ignore-errors
    (let ((file (nlk:home +audit-file+)))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :if-exists :append
                                :if-does-not-exist :create)
        (format out "~a~%"
                (nlk:encode-json-object
                 (nlk:json-object "at" (iso-now)
                                  "tool" tool
                                  "tier" (string-downcase (symbol-name tier))
                                  "path" path
                                  "status" status)))))))

;;; --- one prepared call ------------------------------------------------------------

(defstruct (built (:constructor make-built))
  "One call a tool prepared: what it would send, its tier, and the line and
count its answer opens with."
  (method :post)
  (path "")
  (body nil)
  (tier :t1)
  (note nil)
  (count nil))

(defun tier-label (tier)
  "TIER in words."
  (ecase tier
    (:t0 "T0 read")
    (:t1 "T1 reversible write")
    (:t2 "T2 gated: a call that can fire Flows or change consent")
    (:t3 "T3 send: real email to real people")))

(defun gated-p (tier)
  "Whether TIER is a dry run first."
  (member tier '(:t2 :t3)))

(defun preview-text (built)
  "The dry run's answer: tier, note, the request and its exact body."
  (format nil "tier: ~a~@[~%note: ~a~]~%request: ~a ~a~%body: ~a"
          (tier-label (built-tier built)) (built-note built)
          (string-upcase (symbol-name (built-method built))) (built-path built)
          (nlk:encode-json-object (built-body built))))

(defun run-built (built tool &key dry-run confirm-token)
  "Run BUILT, named TOOL. A read or Tier 1 write goes now; a Tier 2 or 3 call
answers its preview and a confirm_token unless DRY-RUN is explicitly false,
and then needs that token for the same body."
  (let ((tier (built-tier built))
        (method (built-method built))
        (path (built-path built))
        (body (built-body built)))
    (when (and (gated-p tier)
               (string-equal (or (setting :environment) "production") "disabled"))
      (fail "environment is disabled in the section: every Tier 2 and Tier 3 call is refused until it says production"))
    (if (gated-p tier)
        (if (not (null dry-run))
            (format nil "~a dry run -- nothing was sent.~%confirm_token: ~a~%~
(it lives ~d minutes; the real call repeats these exact parameters with dry_run=false)~%~a"
                    tool (confirm-token tool body) (round +confirm-ttl+ 60) (preview-text built))
            (progn
              (check-confirm tool body confirm-token)
              (multiple-value-bind (data status) (request method path :body body)
                (audit-write tool tier path status)
                (answer-text method path status data
                             :note (built-note built) :count (built-count built)))))
        (multiple-value-bind (data status) (request method path :body body)
          (audit-write tool tier path status)
          (answer-text method path status data
                       :note (built-note built) :count (built-count built))))))

;;; --- the model-facing summary of a preview -----------------------------------------

(defun dry-run-p (value)
  "VALUE of a dry_run member: T unless it was explicitly false."
  (not (null value)))
