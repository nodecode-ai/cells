;;;; decode-test.lisp --- the decoder's half: what a value holds, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: text comes back as text, hex in the four ways hex is
;;;; written, percent-encoding and data: URIs, JSON pretty-printed with its keys
;;;; in order, gzip inflated and what it held described one level down, a binary
;;;; named by its bytes with a hex dump, a refusal that says what is wrong, the
;;;; encoding a value or a caller names, the ceiling inflation is cut at, and the
;;;; control characters a value uses to hide. Every test reads the pure half,
;;;; so none needs the cell started; the seam tests follow in cell-test.lisp.

(in-package #:nodecode.test)

(deftest b64-hello-world-is-text-as-text ()
  (let ((answer (b64-expand "SGVsbG8sIHdvcmxkIQ==")))
    (is (search "base64: 18 characters -> 13 bytes" answer) "how it was read, and what it cost")
    (is (search "Content: text, 13 characters, 1 line" answer) "text, with its size")
    (is (search "  Hello, world!" answer) "and the text itself, indented")))

(deftest b64-hex-in-every-way-hex-is-written ()
  (dolist (value '("48656c6c6f" "0x48656c6c6f" "48:65:6c:6c:6f" "48 65 6c 6c 6f"))
    (let ((answer (b64-expand value)))
      (is (search "hex: 10 characters -> 5 bytes" answer) value)
      (is (search "  Hello" answer) value)))
  (is (search "read as hex; it is also valid base64" (b64-expand "48656c6c6f")) "a value that fits both says how to read it the other way"))

(deftest b64-percent-encoding-and-a-data-uri ()
  (let ((answer (b64-expand "caf%C3%A9%20au%20lait")))
    (is (search "percent-encoding: 21 characters -> 13 bytes" answer) "a %XX makes it percent-encoding")
    (is (search "  café au lait" answer) "the escapes read as the UTF-8 bytes they carry"))
  (is (search "'+' is kept as '+'" (b64-expand "a+b%20c")) "a plus is not a space outside a form")
  (let ((answer (b64-expand "data:text/plain;base64,SGVsbG8=" )))
    (is (search "data: URI (text/plain, base64)" answer) "a data: URI names its type and encoding")
    (is (search "  Hello" answer) "and decodes to what it carries")))

(deftest b64-json-is-pretty-printed ()
  (let ((answer (b64-expand (b64-text "{\"ok\":false,\"event\":\"deploy\"}"))))
    (is (search "Content: JSON object, 2 keys" answer) "an object is named by its key count")
    (is (search "JSON" answer) "and shown as JSON, not as one line of text")
    (is (search "\"event\": \"deploy\"" answer) "its keys in order")
    (is (search "\"ok\": false" answer) "and its values as JSON wrote them")))

(deftest b64-gzip-inflates-and-what-it-held-is-described ()
  (let* ((gzipped (b64-gzip (nodecode-b64:utf-8-octets "{\"event\":\"deploy\",\"ok\":false}")))
         (answer (b64-expand (b64-base64 gzipped))))
    (is (search "Content: gzip data, inflates to" answer) "gzip is inflated in memory")
    (is (search "Inflated: JSON object, 2 keys" answer) "and what it held is described the same way")
    (is (search "\"ok\": false" answer) "down to the value")))

(deftest b64-a-binary-is-named-by-its-bytes ()
  (let* ((png (concatenate '(vector (unsigned-byte 8))
                            (nodecode-b64::octets #x89 80 78 71 13 10 26 10)
                            (nodecode-b64::octets 0 0 0 13)
                            (nodecode-b64::ascii "IHDR")
                            (nodecode-b64::octets 0 0 0 1 0 0 0 1)))
         (answer (b64-expand (b64-base64 png))))
    (is (search "Content: PNG image, 1 x 1 pixels" answer) "a PNG is named, with its size read from IHDR")
    (is (search "First bytes (hex)" answer) "and shown as bytes")
    (is (search "  00000000  89 50 4e 47" answer) "the dump starts at offset zero")))

(deftest b64-refusals-say-what-is-wrong ()
  (is (search "as padding" (b64-refusal "SG=VsbG8=")) "an '=' inside the value")
  (is (search "cut short" (b64-refusal "SGVsbG8sI")) "a value with one character left over")
  (is (search "mixes the standard" (b64-refusal "a+b_c")) "both alphabets in one value")
  (is (search "not base64, base64url, hex" (b64-refusal "hello, world!")) "text that is none of them")
  (is (search "nothing to decode" (b64-refusal "   ")) "an empty value")
  (is (search "is not a hex digit" (b64-refusal "0x48zz")) "a hex value with a bad digit"))

(deftest b64-the-encoding-can-be-named ()
  (is (search "  Hello" (b64-expand "hex:48656c6c6f")) "a prefix in the value picks the encoding")
  (let ((answer (b64-expand "48656c6c6f" :forced "base64")))
    (is (search "base64, unpadded: 10 characters -> 7 bytes" answer) "and so does the caller's member")
    (is (null (search "hex:" answer)) "which overrides the shape"))
  (is (search "  Hello" (b64-expand "SGVsbG8" :forced "base64url")) "base64url reads the URL alphabet"))

(deftest b64-inflation-is-cut-at-the-ceiling ()
  (let* ((zeros (make-array 4000 :element-type '(unsigned-byte 8) :initial-element 0))
         (limits (list :max-input 65536 :max-inflated 1024 :text-chars 40 :text-lines 5 :dump-bytes 16))
         (answer (nodecode-b64:expand-value (b64-base64 (b64-gzip zeros)) :limits limits)))
    (is (search "inflates to 1,024 bytes" answer) "the output stops at the ceiling")
    (is (search "inflated output is cut at 1,024 bytes" answer) "and says that it was cut")))

(deftest b64-invisible-characters-cannot-hide ()
  (let ((answer (b64-expand (b64-text (format nil "a~cb" (code-char 7))))))
    (is (search "  a\\x07b" answer) "a bell is written out")
    (is (search "control or invisible character" answer) "and called out")))

(deftest b64-a-jwt-and-a-second-layer-are-called-out ()
  (let ((token "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxIn0.c2ln"))
    (is (search "the text is a JWT" (b64-expand (b64-text token))) "a JWT is named as one"))
  (is (search "the text is base64 again" (b64-expand (b64-text (b64-text "hello there")))) "text that is base64 again says so"))
