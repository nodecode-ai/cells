;;;; cell.lisp --- the b64_decode tool, /b64, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of olric3015/hermes-plugins' b64-ref, a Hermes Agent
;;;; plugin that taught a harness's composer a @b64: reference: write
;;;; @b64:SGVsbG8= in a message and what the value holds is put there instead
;;;; of the reference - text as text, JSON pretty-printed, gzip inflated, a
;;;; binary named by its format with a hex dump of its first bytes. Hermes
;;;; expands a reference as the message goes out; Nodecode has no such seam,
;;;; and needs none: the model calls b64_decode with the value, and the same
;;;; description comes back as the tool result, before it writes the answer the
;;;; user reads. The decoding itself is decode.lisp's, unchanged in what it
;;;; answers.

(in-package #:nodecode-b64)

;;; --- the tool --------------------------------------------------------------------

(defparameter +encodings+ '("auto" "base64" "base64url" "hex" "url")
  "The encodings the tool's member and a value's own prefix can name; auto
reads the encoding off the value's shape.")

(nlk:define-contract b64-arguments
  (:doc "Decode a value a user or a tool handed over in base64, base64url, hex or
percent-encoding, and see what it holds: text as text, JSON pretty-printed, gzip and
zlib inflated, anything else named by its format with a hex dump of its first bytes.
Call it whenever a value like SGVsbG8=, a data: URI, a Basic auth header's payload, a
Kubernetes secret, a hex dump or a %XX string turns up - a model decoding it in its
head gets bytes wrong often enough to matter, and cannot inflate gzip at all. It does
not run, open, save or fetch what it decodes, and does not decrypt anything.")
  ("value" :text :required t
           :doc "the value to decode, whole; its encoding prefix (base64:, base64url:, hex:, url:) is honoured when it has one")
  ("encoding" :string :default "auto" :options +encodings+
              :doc "auto reads the encoding off the value's own shape; a prefix inside the value wins over this member"))

(defun current-limits ()
  "The bounds the running cell's section sets, as the decoder reads them."
  (list :max-input (setting :max-input-chars)
        :max-inflated (setting :max-inflated-bytes)
        :text-chars (setting :text-chars)
        :text-lines (setting :text-lines)
        :dump-bytes (setting :dump-bytes)))

(defun describe-or-reason (value forced)
  "VALUE described, or the one line saying why it could not be read: what a
model can act on, rather than a failed call."
  (handler-case (expand-value value :forced forced :limits (current-limits))
    (b64-error (failure) (format nil "b64: ~a" (b64-error-detail failure)))))

(defun run-decode (arguments)
  "The b64_decode tool: ARGUMENTS' value described."
  (let ((encoding (b64-arguments-encoding arguments)))
    (describe-or-reason (b64-arguments-value arguments)
                        (and (not (string-equal encoding "auto")) encoding))))

;;; --- /b64 ------------------------------------------------------------------------

(defparameter +usage+
  "b64: /b64 <base64|base64url|hex|percent-encoded> - what the value holds: text as
 text, JSON pretty-printed, gzip and zlib inflated, any other format named with a hex
 dump. Prefix the value with base64:, base64url:, hex: or url: when its shape is
 ambiguous; quote a value that holds spaces."
  "What /b64 says when it is typed alone.")

(defun run-slash (args session-id)
  "The /b64 command: the value typed after it, described. => the answer's text."
  (declare (ignore session-id))
  (running-settings)
  (let ((value (nlk:trimmed (or args ""))))
    (if (zerop (length value))
        +usage+
        (describe-or-reason value nil))))

;;; --- the manual ------------------------------------------------------------------

(defparameter +manual+
  "b64: decodes base64 (standard or URL-safe, padded or not), hex (also 0x48656c6c6f,
48:65:6c:6c:6f, \\x48\\x65), percent-encoding and data: URIs, and says what the value
holds: text as text (control and invisible characters shown as \\x07), JSON
pretty-printed, gzip and zlib inflated in memory one layer deep, and anything else
named by its format (PNG and GIF with their size, JPEG, WebP, PDF, ZIP, bzip2, xz,
zstd, 7-Zip, ELF, Mach-O, Windows PE, Java class, WebAssembly, SQLite, DER, Ogg, MP3,
WAV, MP4) with a hex dump of its first 128 bytes. Notes call out a value cut short,
padding that is off, a second layer of encoding, and a JWT. The encoding is told by
the value's shape: a %XX makes it percent-encoding, hex digits in pairs (or a 0x
prefix) make it hex, the base64 alphabets cover the rest; base64:, base64url:, hex:
or url: in front of the value picks one. The tool b64_decode does the same for a
value the model is handed; /b64 does it for the operator. Nothing decoded is run,
opened, saved or fetched, and nothing is decrypted. The section b64 bounds it:
max_input_chars (a longer value is refused), max_inflated_bytes (inflation is cut
there), text_chars and text_lines (how much of a result is shown), dump_bytes (how
much of a binary is dumped)."
  "What (help :b64) answers while the cell runs.")

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell b64
  (:section ("b64")
    (:guide "nothing here is required: a value of up to 65,536 characters is decoded, gzip and zlib data is inflated at most 1 MiB, and the result is the same from the b64_decode tool and /b64")
    ("max_input_chars" :integer :default 65536 :min 16
                        :doc "the longest value decoded; a longer one is refused, not cut")
    ("max_inflated_bytes" :integer :default 1048576 :min 1024
                           :doc "gzip and zlib output is cut at this many bytes")
    ("text_chars" :integer :default 3000 :min 40
                   :doc "characters of decoded text shown; the rest is counted")
    ("text_lines" :integer :default 60 :min 1
                   :doc "lines of decoded text shown; the rest is counted")
    ("dump_bytes" :integer :default 128 :min 16
                   :doc "bytes of a binary result shown as a hex dump"))
  (:settings (lambda (values table)
               (declare (ignore table))
               values))
  (:tool "b64_decode" (nlk:contract 'b64-arguments) 'run-decode)
  (:command "b64" 'run-slash
            :description "Decode base64, base64url, hex or percent-encoded text"
            :argument-hint "VALUE")
  (:help :b64 "b64: decode base64, base64url, hex or percent-encoded text - /b64 <value>"
         +manual+))
