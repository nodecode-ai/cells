;;;; proto.lisp --- protobuf by hand, the Connect envelope, and gzip.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): the wire codec of catalog/src/
;;;; discovery/protobuf.ts, as far as the Cascade messages devin.ts and
;;;; discovery/devin.ts use reach, the field numbers of catalog/src/discovery/
;;;; devin-proto.ts (generated from providers/devin/proto/exa/*.proto), and
;;;; the Connect envelope of ai/src/providers/connect-frame.ts. Pure
;;;; transforms, no I/O but the frame reader's stream.
;;;;
;;;; Encoding follows omp's codec byte for byte: fields go out in the order
;;;; the descriptor lists them (not by number), a proto3 scalar at its
;;;; default (empty string, zero, false) is left out, a sub-message is written
;;;; whenever it is present, and a repeated enum is packed. Decoding keeps
;;;; every field as it arrived; the readers below take the last occurrence of
;;;; a singular field, as omp's decoder does, and refuse a wire type the
;;;; descriptor does not name.
;;;;
;;;; The Connect envelope is one flag byte (1 = compressed, 2 = end of
;;;; stream), a 4-byte big-endian length and the payload. A request frame is
;;;; gzipped as omp's is; the image links no compressor (chipz only
;;;; inflates), so the gzip member written here carries stored deflate
;;;; blocks: a valid gzip stream any inflater reads, as large as its input.

(in-package #:nodecode-devin)

(define-condition proto-error (error)
  ((text :initarg :text :reader proto-error-text))
  (:report (lambda (condition stream) (write-string (proto-error-text condition) stream)))
  (:documentation "Bytes that are not the protobuf, envelope or gzip member they should be."))

(defun proto-fail (control &rest arguments)
  "Signal PROTO-ERROR with CONTROL formatted over ARGUMENTS."
  (error 'proto-error :text (apply #'format nil control arguments)))

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun octets (sequence)
  "SEQUENCE as a simple octet vector."
  (coerce sequence 'octets))

(defun utf8 (text)
  "TEXT's UTF-8 bytes."
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun utf8-text (octets)
  "OCTETS read as UTF-8; malformed bytes refuse, as omp's fatal decoder does."
  (handler-case (sb-ext:octets-to-string (octets octets) :external-format :utf-8)
    (error () (proto-fail "a string field is not UTF-8"))))

;;; --- the writer -------------------------------------------------------------------

(defun make-writer ()
  "An empty growing octet buffer."
  (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun put-byte (out byte)
  (vector-push-extend byte out))

(defun put-varint (out value)
  "VALUE as a base-128 varint; a negative int32 or int64 goes out as its
64-bit two's complement, ten bytes, as protobuf writes it."
  (let ((value (if (minusp value) (ldb (byte 64 0) value) value)))
    (loop
      (let ((low (logand value #x7f)))
        (setf value (ash value -7))
        (if (zerop value)
            (return (put-byte out low))
            (put-byte out (logior low #x80)))))))

(defun put-tag (out field wire-type)
  (put-varint out (logior (ash field 3) wire-type)))

(defun put-octets (out octets)
  (loop for byte across octets do (put-byte out byte)))

(defun put-fixed (out value count)
  "VALUE's low COUNT bytes, little-endian."
  (dotimes (index count)
    (put-byte out (ldb (byte 8 (* 8 index)) value))))

(defun put-length-delimited (out field octets)
  (put-tag out field 2)
  (put-varint out (length octets))
  (put-octets out octets))

(defun double-bits (value)
  "The IEEE 754 binary64 bits of VALUE."
  (let ((value (coerce value 'double-float)))
    (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits value)) 32)
            (sb-kernel:double-float-low-bits value))))

;;; One writer per field kind, each leaving out the proto3 default.

(defun pb-string (out field value)
  "A string field; empty or NIL is the default and is left out."
  (when (and value (plusp (length value)))
    (put-length-delimited out field (utf8 value))))

(defun pb-present-string (out field value)
  "An `optional' string field: written whenever VALUE is a string."
  (when (stringp value)
    (put-length-delimited out field (utf8 value))))

(defun pb-varint (out field value)
  "A uint32, uint64, int32, int64 or enum field; zero is left out."
  (when (and value (/= value 0))
    (put-tag out field 0)
    (put-varint out value)))

(defun pb-bool (out field value)
  "A bool field; false is left out."
  (when value
    (put-tag out field 0)
    (put-varint out 1)))

(defun pb-double (out field value)
  "A double field; zero is left out."
  (when (and value (/= value 0))
    (put-tag out field 1)
    (put-fixed out (double-bits value) 8)))

(defun pb-message (out field octets)
  "A sub-message field, written whenever it is present (OCTETS, NIL when absent)."
  (when octets
    (put-length-delimited out field octets)))

(defun pb-strings (out field values)
  "A repeated string field: one tagged entry per value."
  (dolist (value values)
    (put-length-delimited out field (utf8 value))))

(defun pb-messages (out field encoded)
  "A repeated message field: one tagged entry per encoded message."
  (dolist (octets encoded)
    (put-length-delimited out field octets)))

(defun pb-packed (out field values)
  "A repeated varint field (enum, int), packed into one entry."
  (when values
    (let ((inner (make-writer)))
      (dolist (value values) (put-varint inner value))
      (put-length-delimited out field inner))))

(defmacro encoding ((out) &body body)
  "Run BODY writing into the buffer OUT; answer what it wrote as octets."
  `(let ((,out (make-writer)))
     ,@body
     (octets ,out)))

;;; --- the reader --------------------------------------------------------------------

(defun read-varint (octets position)
  "(values VALUE NEXT) of the varint at POSITION in OCTETS."
  (let ((value 0) (shift 0))
    (loop
      (when (>= position (length octets))
        (proto-fail "a varint runs past the end of the message"))
      (when (>= shift 70)
        (proto-fail "a varint is longer than ten bytes"))
      (let ((byte (aref octets position)))
        (incf position)
        (setf value (logior value (ash (logand byte #x7f) shift)))
        (incf shift 7)
        (unless (logtest byte #x80)
          (return (values (ldb (byte 64 0) value) position)))))))

(defmacro ecase-wire (wire-type &body clauses)
  "CASE over a wire type, refusing the group types and anything unnamed."
  `(case ,wire-type
     ,@clauses
     (t (proto-fail "unsupported protobuf wire type ~d" ,wire-type))))

(defun decode-fields (octets)
  "Every field OCTETS carries, in order, as (FIELD WIRE-TYPE VALUE): VALUE an
integer for a varint or fixed field, octets for a length-delimited one."
  (let ((octets (octets octets)) (position 0) (fields '()))
    (flet ((take (field count)
             (when (> (+ position count) (length octets))
               (proto-fail "field ~d runs past the end of the message" field))
             (prog1 (subseq octets position (+ position count))
               (incf position count)))
           (little-endian (bytes)
             (loop for index below (length bytes) sum (ash (aref bytes index) (* 8 index))))
           (varint ()
             (multiple-value-bind (value next) (read-varint octets position)
               (setf position next)
               value)))
      (loop while (< position (length octets))
            do (let* ((tag (varint))
                      (field (ash tag -3))
                      (wire-type (logand tag 7)))
                 (when (zerop field)
                   (proto-fail "field number 0"))
                 (push (list field wire-type
                             (ecase-wire wire-type
                               (0 (varint))
                               (1 (little-endian (take field 8)))
                               (2 (take field (varint)))
                               (5 (little-endian (take field 4)))))
                       fields))))
    (nreverse fields)))

(defun field-entries (message field wire-type)
  "The values of FIELD in the decoded MESSAGE, in order, each checked to ride WIRE-TYPE."
  (loop for (number type value) in message
        when (= number field)
          collect (if (= type wire-type)
                      value
                      (proto-fail "field ~d arrived as wire type ~d, not ~d" field type wire-type))))

(defun field-last (message field wire-type)
  "The last value of the singular FIELD, or NIL when it is absent."
  (car (last (field-entries message field wire-type))))

(defun pb-text (message field)
  "The string FIELD, \"\" when absent."
  (let ((octets (field-last message field 2)))
    (if octets (utf8-text octets) "")))

(defun pb-present-text (message field)
  "The `optional' string FIELD, or NIL when absent."
  (let ((octets (field-last message field 2)))
    (and octets (utf8-text octets))))

(defun pb-texts (message field)
  "The repeated string FIELD, a list."
  (mapcar #'utf8-text (field-entries message field 2)))

(defun pb-uint (message field)
  "The unsigned varint FIELD (uint32, uint64, enum, bool), 0 when absent."
  (or (field-last message field 0) 0))

(defun pb-int32 (message field)
  "The int32 (or enum) FIELD, signed, 0 when absent."
  (let ((value (ldb (byte 32 0) (pb-uint message field))))
    (if (logbitp 31 value) (- value (ash 1 32)) value)))

(defun pb-flag (message field)
  "The bool FIELD."
  (/= 0 (pb-uint message field)))

(defun pb-double-value (message field)
  "The double FIELD, 0d0 when absent."
  (let ((bits (field-last message field 1)))
    (if bits
        (sb-kernel:make-double-float (let ((high (ldb (byte 32 32) bits)))
                                       (if (logbitp 31 high) (- high (ash 1 32)) high))
                                     (ldb (byte 32 0) bits))
        0d0)))

(defun pb-float-value (message field)
  "The float FIELD as a double, 0d0 when absent."
  (let ((bits (field-last message field 5)))
    (if bits
        (coerce (sb-kernel:make-single-float (if (logbitp 31 bits) (- bits (ash 1 32)) bits))
                'double-float)
        0d0)))

(defun pb-sub (message field)
  "The sub-message FIELD decoded, or NIL when absent."
  (let ((octets (field-last message field 2)))
    (and octets (decode-fields octets))))

(defun pb-subs (message field)
  "The repeated message FIELD, each decoded."
  (mapcar #'decode-fields (field-entries message field 2)))

;;; --- the Connect envelope ---------------------------------------------------------

(defconstant +compressed-flag+ #x01
  "Flag bit: the payload is gzipped.")

(defconstant +end-stream-flag+ #x02
  "Flag bit: the payload is the end-of-stream JSON trailer.")

(defparameter +max-frame-payload+ (* 16 1024 1024)
  "The largest payload a frame may announce (omp's MAX_CONNECT_FRAME_PAYLOAD):
the length prefix is the peer's to choose, so a corrupt one fails here
before anything is buffered.")

(defun connect-frame (payload &optional (flags 0))
  "PAYLOAD in one Connect envelope."
  (encoding (out)
    (put-byte out flags)
    (loop for shift from 24 downto 0 by 8 do (put-byte out (ldb (byte 8 shift) (length payload))))
    (put-octets out payload)))

(defun read-exactly (stream count)
  "COUNT bytes from STREAM, or (values PARTIAL :eof) when it ends first."
  (let* ((buffer (make-array count :element-type '(unsigned-byte 8)))
         (end (read-sequence buffer stream)))
    (if (< end count)
        (values (subseq buffer 0 end) :eof)
        buffer)))

(defun read-connect-frame (stream)
  "(values FLAGS PAYLOAD) of the next envelope on the octet STREAM, or NIL at
a clean end between frames; PROTO-ERROR for a stream that ends inside one or
a length past +MAX-FRAME-PAYLOAD+."
  (multiple-value-bind (header eof) (read-exactly stream 5)
    (cond ((and eof (zerop (length header))) nil)
          (eof (proto-fail "the stream ended inside a frame header"))
          (t (let ((length (loop for index from 1 to 4
                                 for value = (aref header index)
                                   then (logior (ash value 8) (aref header index))
                                 finally (return value))))
               (when (> length +max-frame-payload+)
                 (proto-fail "Devin Connect frame length ~d exceeds ~d-byte cap" length +max-frame-payload+))
               (multiple-value-bind (payload eof) (read-exactly stream length)
                 (when eof
                   (proto-fail "the stream ended inside a ~d-byte frame" length))
                 (values (aref header 0) payload)))))))

(defun decode-connect-frames (octets)
  "Every envelope in OCTETS, as (FLAGS . PAYLOAD) in order."
  (let ((stream (flexi-streams:make-in-memory-input-stream octets)))
    (loop for (flags payload) = (multiple-value-list (read-connect-frame stream))
          while flags
          collect (cons flags payload))))

;;; --- gzip ------------------------------------------------------------------------------

(defparameter +crc-table+
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (index 256 table)
      (let ((value index))
        (dotimes (bit 8)
          (setf value (if (logbitp 0 value)
                          (logxor #xEDB88320 (ash value -1))
                          (ash value -1))))
        (setf (aref table index) value))))
  "The CRC-32 (IEEE 802.3, reflected) table a gzip trailer is computed with.")

(defun crc32 (octets)
  "The CRC-32 of OCTETS, as gzip's trailer carries it."
  (let ((crc #xFFFFFFFF))
    (loop for byte across octets
          do (setf crc (logxor (aref +crc-table+ (logand #xFF (logxor crc byte))) (ash crc -8))))
    (logxor crc #xFFFFFFFF)))

(defun gzip (octets)
  "OCTETS as one gzip member of stored deflate blocks (RFC 1951 BTYPE 00)."
  (let ((octets (octets octets)))
    (encoding (out)
      ;; ID1 ID2, CM deflate, no flags, no mtime, no extra flags, OS unknown
      (put-octets out #(#x1f #x8b 8 0 0 0 0 0 0 #xff))
      (let ((total (length octets)))
        (if (zerop total)
            (put-octets out #(1 0 0 #xff #xff))
            (loop for start from 0 below total by #xffff
                  for end = (min total (+ start #xffff))
                  for size = (- end start)
                  do (put-byte out (if (= end total) 1 0))
                     (put-fixed out size 2)
                     (put-fixed out (logxor size #xffff) 2)
                     (put-octets out (subseq octets start end)))))
      (put-fixed out (crc32 octets) 4)
      (put-fixed out (ldb (byte 32 0) (length octets)) 4))))

(defun gunzip (octets)
  "The bytes the gzip member OCTETS holds."
  (handler-case (octets (chipz:decompress nil 'chipz:gzip (octets octets)))
    (error (e) (proto-fail "not a gzip member: ~a" e))))

;;; --- the Cascade messages ------------------------------------------------------------
;;; Field numbers and descriptor order from devin-proto.ts; a comment names
;;; each message's .proto package. Only the fields omp sets or reads are
;;; carried.

(defparameter +chat-source-user+ 1 "ChatMessageSource.USER")
(defparameter +chat-source-system+ 2 "ChatMessageSource.SYSTEM: the assistant's turn")
(defparameter +chat-source-tool+ 4 "ChatMessageSource.TOOL")
(defparameter +request-type-cascade+ 5 "ChatMessageRequestType.CASCADE")
(defparameter +planner-mode-default+ 1 "ConversationalPlannerMode.DEFAULT")
(defparameter +cache-control-ephemeral+ 1 "CacheControlType.EPHEMERAL")
(defparameter +stop-reason-max-tokens+ 3 "StopReason.MAX_TOKENS")

(defun encode-metadata (&key ide-name ide-version ide-type extension-name extension-version
                             api-key locale os user-jwt supported-model-displays)
  "exa.codeium_common_pb.Metadata."
  (encoding (out)
    (pb-string out 1 ide-name)
    (pb-string out 7 ide-version)
    (pb-string out 28 ide-type)
    (pb-string out 12 extension-name)
    (pb-string out 2 extension-version)
    (pb-string out 3 api-key)
    (pb-string out 4 locale)
    (pb-string out 5 os)
    (pb-string out 21 user-jwt)
    (pb-packed out 30 supported-model-displays)))

(defun encode-image (base64 mime-type)
  "exa.codeium_common_pb.ImageData."
  (encoding (out)
    (pb-string out 1 base64)
    (pb-string out 2 mime-type)))

(defun encode-tool-call (id name arguments)
  "exa.codeium_common_pb.ChatToolCall."
  (encoding (out)
    (pb-string out 1 id)
    (pb-string out 2 name)
    (pb-string out 3 arguments)))

(defun encode-chat-prompt (&key message-id source prompt tool-calls tool-call-id
                                tool-result-is-error images thinking signature)
  "exa.chat_pb.ChatMessagePrompt: TOOL-CALLS and IMAGES are encoded messages."
  (encoding (out)
    (pb-string out 1 message-id)
    (pb-varint out 2 source)
    (pb-string out 3 prompt)
    (pb-messages out 6 tool-calls)
    (pb-string out 7 tool-call-id)
    (pb-bool out 9 tool-result-is-error)
    (pb-messages out 10 images)
    (pb-string out 11 thinking)
    (pb-string out 12 signature)))

(defun encode-tool-definition (name description schema-json &optional strict)
  "exa.chat_pb.ChatToolDefinition."
  (encoding (out)
    (pb-string out 1 name)
    (pb-string out 2 description)
    (pb-string out 3 schema-json)
    (pb-bool out 12 strict)))

(defun encode-tool-choice (option-name)
  "exa.chat_pb.ChatToolChoice, its oneof on option_name."
  (encoding (out)
    (pb-present-string out 1 option-name)))

(defun encode-cache-options (type)
  "exa.chat_pb.PromptCacheOptions."
  (encoding (out)
    (pb-varint out 1 type)))

(defun encode-completion-configuration (&key (completions 1) max-tokens max-newlines temperature
                                             first-temperature top-k top-p stop-patterns
                                             fim-eot-threshold)
  "exa.codeium_common_pb.CompletionConfiguration."
  (encoding (out)
    (pb-varint out 1 completions)
    (pb-varint out 2 max-tokens)
    (pb-varint out 3 max-newlines)
    (pb-double out 5 temperature)
    (pb-double out 6 first-temperature)
    (pb-varint out 7 top-k)
    (pb-double out 8 top-p)
    (pb-strings out 9 stop-patterns)
    (pb-double out 11 fim-eot-threshold)))

(defun encode-chat-request (&key metadata prompt prompts chat-model-uid request-type configuration
                                 tools disable-parallel-tool-calls tool-choice system-cache-options
                                 cascade-id planner-mode execution-id model-assignment-jwt)
  "exa.api_server_pb.GetChatMessageRequest: METADATA, CONFIGURATION,
TOOL-CHOICE and SYSTEM-CACHE-OPTIONS are encoded messages, PROMPTS and
TOOLS lists of them."
  (encoding (out)
    (pb-message out 1 metadata)
    (pb-string out 2 prompt)
    (pb-messages out 3 prompts)
    (pb-string out 21 chat-model-uid)
    (pb-varint out 7 request-type)
    (pb-message out 8 configuration)
    (pb-messages out 10 tools)
    (pb-bool out 11 disable-parallel-tool-calls)
    (pb-message out 12 tool-choice)
    (pb-message out 13 system-cache-options)
    (pb-string out 16 cascade-id)
    (pb-varint out 20 planner-mode)
    (pb-string out 22 execution-id)
    (pb-present-string out 26 model-assignment-jwt)))

(defun encode-metadata-request (metadata)
  "exa.auth_pb.GetUserJwtRequest and exa.api_server_pb.GetCliModelConfigsRequest:
both carry the metadata alone, as field 1."
  (encoding (out)
    (pb-message out 1 metadata)))

(defun encode-assign-model-request (&key metadata router-uid cascade-id prompt)
  "exa.api_server_pb.AssignModelRequest: PROMPT an encoded ChatMessagePrompt or NIL."
  (encoding (out)
    (pb-message out 1 metadata)
    (pb-string out 2 router-uid)
    (pb-string out 3 cascade-id)
    (pb-message out 5 prompt)))

;;; Decoders answer plists.

(defun decode-tool-call (message)
  "exa.codeium_common_pb.ChatToolCall: (:id :name :arguments)."
  (list :id (pb-text message 1) :name (pb-text message 2) :arguments (pb-text message 3)))

(defun decode-chat-response (octets)
  "exa.api_server_pb.GetChatMessageResponse, the fields omp folds."
  (let* ((message (decode-fields octets))
         (usage (pb-sub message 7)))
    (list :message-id (pb-text message 1)
          :delta-text (pb-text message 3)
          :stop-reason (pb-int32 message 5)
          :tool-calls (mapcar #'decode-tool-call (pb-subs message 6))
          ;; exa.codeium_common_pb.ModelUsageStats
          :usage (and usage (list :input (pb-uint usage 2) :output (pb-uint usage 3)
                                  :cache-write (pb-uint usage 4) :cache-read (pb-uint usage 5)))
          :delta-thinking (pb-text message 9)
          :delta-signature (pb-text message 10)
          :credit-cost (pb-int32 message 14)
          :committed-credit-cost (pb-int32 message 18)
          :committed-acu-cost (pb-double-value message 22)
          :actual-model-uid (pb-present-text message 23))))

(defun decode-user-jwt-response (octets)
  "exa.auth_pb.GetUserJwtResponse: (values USER-JWT CUSTOM-API-SERVER-URL)."
  (let ((message (decode-fields octets)))
    (values (pb-text message 1) (pb-text message 2))))

(defun decode-assign-model-response (octets)
  "exa.api_server_pb.AssignModelResponse: (values ASSIGNMENT-JWT MODEL-UID),
NIL when it carries no assignment."
  (let ((assignment (pb-sub (decode-fields octets) 1)))
    (when assignment
      (values (pb-text assignment 1) (pb-text assignment 2)))))

(defun decode-model-config (message)
  "exa.codeium_common_pb.ClientModelConfig, the fields omp's discovery reads."
  (let ((info (pb-sub message 23))
        (family (pb-sub message 30)))
    (list :label (pb-text message 1)
          :model-uid (pb-text message 22)
          :disabled (pb-flag message 4)
          :supports-images (pb-flag message 5)
          :max-tokens (pb-int32 message 18)
          :description (pb-present-text message 27)
          :is-default-in-family (pb-flag message 31)
          ;; exa.codeium_common_pb.ModelInfo
          :info (and info
                     (let ((features (pb-sub info 6)))
                       (list :max-output-tokens (pb-int32 info 13)
                             :harness-uids (pb-texts info 20)
                             :display-option (pb-int32 info 22)
                             :model-router (pb-flag info 25)
                             ;; exa.codeium_common_pb.ModelFeatures, or NIL when absent
                             :features (and features
                                            (list :images (pb-flag features 11)
                                                  :tool-calls (pb-flag features 12)
                                                  :parallel-tool-calls (pb-flag features 21)
                                                  :thinking (pb-flag features 15))))))
          ;; exa.codeium_common_pb.ModelFamilyMetadata
          :family (and family
                       (list :label (pb-text family 1)
                             :entries (loop for entry in (pb-subs family 2)
                                            for value = (pb-sub entry 2)
                                            collect (list :key (pb-text entry 1)
                                                          :value (and value
                                                                      (list :order (pb-int32 value 1)
                                                                            :name (pb-text value 2)))))
                             :is-default (pb-flag family 3)))
          ;; exa.codeium_common_pb.ModelDimension
          :dimensions (loop for dimension in (pb-subs message 32)
                            collect (list :label (pb-text dimension 1)
                                          :value (pb-float-value dimension 2)
                                          :denominator (pb-text dimension 3)
                                          :kind (pb-int32 dimension 6))))))

(defun decode-model-configs-response (octets)
  "exa.api_server_pb.GetCliModelConfigsResponse: its client model configs."
  (mapcar #'decode-model-config (pb-subs (decode-fields octets) 1)))
