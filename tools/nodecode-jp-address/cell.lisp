;;;; cell.lisp --- the address_split tool, the /jp-address command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model splits an address with the `address_split' tool, the operator
;;;; with /jp-address; both answer from SPLIT.LISP. The section of the config
;;;; is `jp-address':
;;;;   "jp-address": {"enabled": true, "base_url": "", "timeout_seconds": 15}
;;;; No key: the address data is published openly.

(in-package #:nodecode-jp-address)

;;; --- the base the lookups read ------------------------------------------------

(defparameter +default-base+
  "https://japanese-addresses-v2.geoloniamaps.com/api"
  "The published address data: the national list is <base>.json and one
municipality's town file is <base>/<prefecture>/<municipality>.json.")

(defun usable-base (raw)
  "RAW as the base URL a lookup may read, or NIL when it is not one: a
scheme, a host, an optional port and a non-empty path, and nothing else.
Plain http only for the loopback host, because the address is the user's."
  (when (and (stringp raw)
             (let ((trimmed (nlk:trimmed raw)))
               (and (plusp (length trimmed)) (string= trimmed raw)))
             (not (find-if (lambda (ch)
                             (let ((code (char-code ch)))
                               (or (< code 32)
                                   (member ch '(#\Space #\? #\#) :test #'char=)
                                   (= 92 code))))
                           raw)))
    (let ((uri (ignore-errors (quri:uri raw))))
      (when uri
        (let ((scheme (quri:uri-scheme uri))
              (host (quri:uri-host uri))
              (path (quri:uri-path uri)))
          (when (and (member scheme '("https" "http") :test #'string=)
                     host (plusp (length host))
                     path (plusp (length path))
                     (not (char= #\/ (char path (1- (length path)))))
                     (null (quri:uri-query uri))
                     (null (quri:uri-fragment uri))
                     (null (quri:uri-userinfo uri))
                     (or (string= scheme "https")
                         (member (string-downcase host) '("127.0.0.1" "localhost")
                                 :test #'string=)))
            raw))))))

;;; --- the primer ----------------------------------------------------------------

(defparameter +primer+
  "Japanese addresses are split by the nodecode-jp-address cell: one tool,
`address_split', and one slash command, /jp-address.
  address_split(address)   One Japanese address, as written, split into prefecture, city and
      town: it answers level (0 no match, 1 prefecture only, 2 municipality, 3 one town), pref,
      city, town, other (the house number, the building and anything else left over), an optional
      point (lat and lng, a coarse town or municipality point, not a rooftop geocode), and
      attribution. Keep `other' in the reply, and copy the attribution into it: the data is
      Geolonia's, CC BY 4.0, built on Japan's Digital Agency address base registry. A result
      with no level carries status: ambiguous_town or ambiguous_city (with the towns or the
      municipalities to choose from), upstream, or bad_args, and the message says what to do.
  /jp-address 東京都千代田区霞が関2-1-2   the same split, in the conversation.
Doctrine: one call answers one address; write the address in Japanese, with the prefecture, and
let it be ambiguous rather than guessing. It reads nothing of yours: a call sends the matched
prefecture and municipality names to the address data host and nothing else, and no key is used."
  "What (help :jp-address) answers while the cell runs.")

;;; --- the settings --------------------------------------------------------------

(defun read-settings (values table)
  "The settings the cell runs on: the base the lookups read, checked here
because it becomes a URL, and the one timeout the declaration derived."
  (declare (ignore table))
  (let ((given (getf values :base-url)))
    (list :base (cond ((or (null given) (string= (nlk:trimmed given) "")) +default-base+)
                      ((usable-base given) given)
                      (t (nlk:config-error
                          "base_url is ~s; use https://host/path, ending before .json"
                          given)))
          :timeout (getf values :timeout-seconds))))

;;; --- the tool ------------------------------------------------------------------

(nlk:define-contract split-request
  (:doc "Split one Japanese address into prefecture, city and town, from Geolonia's public address data. Pass the address as it is written in Japanese, such as 東京都千代田区霞が関2-1-2. Answers level, pref, city, town, other (the house number, the building and anything else left over), an optional point, and attribution. Keep other in the reply and copy attribution into it. A result with no level has status ambiguous_town, ambiguous_city, upstream or bad_args; an ambiguous one lists the towns or municipalities to choose from.")
  (:unknown :refuse)
  ("address" :text :required t
             :doc "one Japanese address, 400 characters or fewer, as written: 東京都千代田区霞が関2-1-2"))

(define-verb address-split-json (text)
  "The tool's answer: TEXT split against the address files, as JSON text, the
shape the original tool had. An upstream failure is a fact in the result, not
a failed turn, so the model can say what happened."
  (nlk:encode-json-object
   (result-object
    (handler-case (split-address text (setting :base) (setting :timeout))
      (jp-address-error (condition)
        (list :status "upstream" :message (princ-to-string condition)))))))

(defun split-tool (request)
  "The `address_split' tool: the record the declaration materialized, answered
as JSON text."
  (address-split-json (split-request-address request)))

;;; --- the command ---------------------------------------------------------------

(defun describe-result (result)
  "RESULT as the lines an operator reads in the conversation."
  (if (getf result :status)
      (format nil "~a~@[ prefectures: ~{~a~^, ~}~]~@[ municipalities: ~{~a~^, ~}~]~@[ towns: ~{~a~^, ~}~]"
              (getf result :message)
              (getf result :prefectures)
              (getf result :municipalities)
              (getf result :towns))
      (with-output-to-string (out)
        (format out "level ~a~@[ · prefecture ~a~]~@[ · city ~a~]~@[ · town ~a~]~@[ · other ~a~]"
                (getf result :level)
                (getf result :pref)
                (getf result :city)
                (getf result :town)
                (getf result :other))
        (let ((point (getf result :point)))
          (when point
            (format out "~&point ~a, ~a (level ~a, not a rooftop geocode)"
                    (getf point :lat) (getf point :lng) (getf point :level))))
        (format out "~&~a" +attribution+))))

(defun run-jp-address (args session-id)
  "The /jp-address command: the same split, in the conversation. Bare, it
asks for the address instead of splitting nothing."
  (declare (ignore session-id))
  (let ((args (nlk:trimmed (or args ""))))
    (if (zerop (length args))
        (values "one Japanese address, split into prefecture, city and town"
                (nle:list-dialog
                 :title "JP address"
                 :summary "the prefecture, the municipality and the town, with the rest left over"
                 :rows (list (nle:ask-row "Split an address" "Address" "/jp-address"
                                           :placeholder "東京都千代田区霞が関2-1-2"
                                           :verb "split"))))
        (handler-case (describe-result (split-address args (setting :base) (setting :timeout)))
          (jp-address-error (condition) (princ-to-string condition))))))

;;; --- the declaration ------------------------------------------------------------

(nle:define-cell jp-address
  (:section ("jp-address")
    (:guide "nothing is required and no key is used; base_url points the lookups at another copy of the data, and timeout_seconds bounds one of them")
    ("base_url" :string :default ""
                :doc "the address data base, ending before .json; empty uses Geolonia's public host")
    ("timeout_seconds" :integer :default 15 :min 1
                       :doc "how long one address file may take to arrive"))
  (:settings 'read-settings)
  (:tool "address_split" (nlk:contract 'split-request) 'split-tool)
  (:command "jp-address" 'run-jp-address
            :description "Split a Japanese address into prefecture, city and town"
            :argument-hint "ADDRESS")
  (:help :jp-address
    "jp-address: a Japanese address, split into prefecture, city and town"
    +primer+))
