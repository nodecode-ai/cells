;;;; cell.lisp --- the tool, the command, the manual, and the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One tool and one command. The tool answers in the shape the original
;;;; plugin answered in: a verdict, one row per detected citation, the ones it
;;;; could not verify, and the pack's identity. Nothing here signs, fetches or
;;;; installs anything: the corpus ships with the cell.

(in-package #:nodecode-dike)

(defparameter +primer+
  "Dike checks the article citations of a Chilean legal brief against a local,
versioned statute corpus (25 statutes, 8,544 articles), and answers citation by
citation. Call the tool dike_verificar_citas before saying that a citation is
correct, exists or is in force: it answers VERIFICADA only when the pack holds
that article of that statute and its window covers the date asked. A citation
it could not verify is not one to trust -- tell the person what the answer
says. What it proves is identity, text integrity and the in-force window the
pack records, not that the statute says what the brief claims, and not today's
law: the pack is a dated copy. A statute named with no article (ley 19.880,
codigo civil) is not verified at all, and an article absent from the pack is
not thereby absent from the law."
  "What (help :dike) answers while the cell runs.")

(defparameter +scope+
  "Solo identidad, integridad y estado temporal registrado de citas con artículo detectadas. No se certifican interpretación, menciones no verificadas ni cambios posteriores a la captura de fuentes.")

(defparameter +messages+
  '(("TODAS_VERIFICADAS"
     . "Las citas con artículo detectadas fueron verificadas en el corpus local. No se certifican todas las referencias ni la vigencia actual.")
    ("CON_PROBLEMAS"
     . "Hay citas detectadas que no pudieron verificarse completamente en el corpus local.")
    ("SIN_CITAS"
     . "No se detectaron citas con artículo verificables; esto no acredita las menciones normativas del escrito."))
  "The sentence each verdict carries.")

;;; --- JSON -----------------------------------------------------------------------------

(defun json (&rest pairs)
  "One JSON object from alternating key/value PAIRS; a key whose value is NIL is
written as null."
  (apply #'nlk:make-json-object pairs))

(defun json-list (list)
  "LIST as a JSON array."
  (coerce list 'vector))

;;; --- the date vigencia is judged at ----------------------------------------------------

(defun today ()
  "Today, as YYYY-MM-DD."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun now-iso ()
  "Now, as an ISO timestamp with the zone's offset."
  (multiple-value-bind (second minute hour day month year day-of-week daylight zone)
      (decode-universal-time (get-universal-time))
    (declare (ignore day-of-week daylight))
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~:[+~;-~]~2,'0d:~2,'0d"
            year month day hour minute second (minusp zone) (abs zone) 0)))

(defun iso-date-p (text)
  "Whether TEXT is a YYYY-MM-DD date."
  (and (= (length text) 10)
       (loop for i below 10
             always (if (member i '(4 7))
                        (char= (char text i) #\-)
                        (and (digit-char-p (char text i)) t)))))

;;; --- one citation ---------------------------------------------------------------------

(defun gid (slug token)
  "The identifier an answer carries for one article: its statute, its token."
  (format nil "cl.norma.~a:art:~a" (substitute #\_ #\- slug) token))

(defun failed-row (raw state reason &key gid url)
  "The row of a citation the pack does not prove."
  (json "cita_raw" raw "estado" "FALLO" "resolucion" state "vigencia" :null
        "motivo" reason "gid" gid "fuente_url" url))

(defun citation-row (citation resolution as-of)
  "The row for CITATION: what the pack proves for it, or why it proves nothing."
  (let ((raw (subseq (citation-raw citation) 0 (length (citation-raw citation)))))
    (if (not (string= (getf resolution :state) "RESOLVED"))
        (failed-row raw (getf resolution :state) (getf resolution :reason))
        (let* ((norm (norm-by-slug (getf resolution :slug)))
               (token (citation-token citation))
               (article (gethash token (norm-articles norm))))
          (if (null article)
              (failed-row raw "RESOLVED"
                          (format nil "el artículo ~a no está en el corpus de ~a"
                                  token (norm-slug norm))
                          :gid (gid (norm-slug norm) token) :url (norm-url norm))
              (let ((state (in-force-p article as-of)))
                (if (string= state "IN_FORCE")
                    (json "cita_raw" raw "estado" "VERIFICADA" "resolucion" "RESOLVED"
                          "vigencia" "IN_FORCE" "estado_texto_historico" "CURRENT_VERSION_APPLIES"
                          "gid" (gid (norm-slug norm) token) "fuente_url" (norm-url norm)
                          "fecha_recuperacion" (format nil "~aT00:00:00Z" (article-retrieved article))
                          "texto_hash" (article-sha article) "motivo" :null)
                    (failed-row raw "RESOLVED"
                                (format nil "el artículo ~a de ~a está ~a al ~a"
                                        token (norm-slug norm)
                                        (if (string= state "NOT_YET_IN_FORCE")
                                            "aún no vigente" "fuera de vigencia")
                                        as-of)
                                :gid (gid (norm-slug norm) token) :url (norm-url norm)))))))))

;;; --- the answer --------------------------------------------------------------------

(defun verify (text &key as-of)
  "Verify every citation in TEXT against the pack, at AS-OF (default today).
=> a JSON object: the verdict, a row per citation, what was not verified, and
the pack's identity."
  (load-corpus)
  (let* ((as-of (if (and as-of (iso-date-p as-of)) as-of (today)))
         (folded (fold text))
         (names (names-in folded))
         (citations (citations-in folded text))
         (rows (loop for citation in citations
                      collect (let ((resolution (resolve-citation citation names folded)))
                                (setf (citation-norm citation) (getf resolution :slug))
                                (cons citation (citation-row citation resolution as-of)))))
         (verified (count-if (lambda (pair) (string= "VERIFICADA" (gethash "estado" (cdr pair))))
                             rows))
         (failed (- (length rows) verified))
         (used (remove-duplicates (remove nil (mapcar (lambda (pair) (citation-norm (car pair))) rows))
                                  :test #'equal))
         (verdict (cond ((null rows) "SIN_CITAS")
                        ((zerop failed) "TODAS_VERIFICADAS")
                        (t "CON_PROBLEMAS"))))
    (json "veredicto" verdict
          "detalles" (json-list (mapcar #'cdr rows))
          "problemas" (json-list (loop for (citation . row) in rows
                                        unless (string= "VERIFICADA" (gethash "estado" row))
                                          collect (format nil "~a: ~a" (citation-raw citation)
                                                          (gethash "motivo" row))))
          "no_verificadas" (json-list (loop for (citation . row) in rows
                                             unless (string= "VERIFICADA" (gethash "estado" row))
                                               collect (citation-raw citation)))
          "menciones_sin_articulo"
          (json-list (loop for named in names
                           unless (member (first (named-slugs named)) used :test #'equal)
                             collect (named-designation named)))
          "mensaje" (cdr (assoc verdict +messages+ :test #'equal))
          "alcance" +scope+
          "todas_citas_detectadas_verificadas" (string= verdict "TODAS_VERIFICADAS")
          "todo_verificado" (and (string= verdict "TODAS_VERIFICADAS")
                                  (null (loop for named in names
                                               unless (member (first (named-slugs named)) used
                                                               :test #'equal)
                                                 collect named)))
          "certificacion_sustantiva" nil
          "certificacion_vigencia_actual" nil
          "meta" (json "origen" "escrito" "documento" "escrito"
                        "as_of" as-of "fecha_constancia" (now-iso)
                        "pack_hash" +pack-hash+ "corpus" "chile-base"
                        "total_citas" (length rows) "citas_verificadas" verified
                        "citas_fallidas" failed
                        "certificacion_vigencia_actual" nil))))

;;; --- what the caller passes -------------------------------------------------------------

(defun brief-text (texto ruta max-bytes)
  "The brief: TEXTO, or the .md/.txt file RUTA, capped at MAX-BYTES of UTF-8."
  (let ((text (cond ((and texto ruta) (fail "da texto o ruta, no ambos"))
                    (texto texto)
                    (ruta (if (and (probe-file ruta)
                                    (member (pathname-type (pathname ruta))
                                            '("md" "txt") :test #'equalp))
                              (handler-case
                                  (with-open-file (in ruta :external-format :utf-8)
                                    (let ((s (make-string (file-length in))))
                                      (read-sequence s in) s))
                                (error () (fail "no se pudo leer el archivo")))
                              (fail "ruta debe ser un archivo .md o .txt")))
                    (t (fail "hace falta texto o ruta")))))
    (when (zerop (length (nlk:trimmed text)))
      (fail "el escrito está vacío"))
    (let ((bytes (length (sb-ext:string-to-octets text :external-format :utf-8))))
      (when (> bytes max-bytes)
        (fail "el escrito ocupa ~d bytes y el máximo es ~d" bytes max-bytes)))
    text))

(defun check-citations (&key texto ruta)
  "Verify the Chilean-law article citations of a brief: TEXTO as text, or RUTA a
.md or .txt file. => a JSON object (a hash table): veredicto, detalles, one
row per citation with estado VERIFICADA or FALLO, no_verificadas, and meta
with the pack's hash and the counts. Nothing is signed and nothing is fetched."
  (let ((max-bytes (setting :max-bytes))
        (as-of (setting :as-of)))
    (verify (brief-text texto ruta max-bytes) :as-of as-of)))

(defun check-json (&key texto ruta)
  "CHECK-CITATIONS as the JSON text the tool answers with."
  (handler-case (nlk:encode-json-object (check-citations :texto texto :ruta ruta))
    (dike-error (condition) (nlk:encode-json-object
                              (json "error" (format nil "~a" condition)
                                    "error_code" "DIKE_ERROR"
                                    "todo_verificado" nil)))))

(defun argument (arguments key)
  "The member KEY of a tool's ARGUMENTS, which arrive as a hash table or a plist."
  (cond ((hash-table-p arguments) (gethash key arguments))
        ((listp arguments)
         (or (getf arguments (intern key :keyword)) (cdr (assoc key arguments :test #'equal))))
        (t nil)))

(defun check-tool (arguments)
  "The dike_verificar_citas tool: {texto} or {ruta} => the JSON answer."
  (check-json :texto (argument arguments "texto") :ruta (argument arguments "ruta")))

(defparameter +schema+
  (list :description
        (concatenate 'string
                     "Verifica las citas de normas chilenas de un escrito jurídico contra un corpus local "
                     "identificado por hash. Úsala SIEMPRE antes de afirmar que una cita legal es "
                     "correcta, existe o está vigente. Devuelve, cita por cita, si se pudo verificar; "
                     "las que no, vienen con el motivo. Una cita no verificada NO se puede dar por "
                     "buena: hay que decírselo al usuario tal cual. Comprueba identidad y estado de "
                     "vigencia registrado en el corpus; no consulta cambios posteriores en vivo ni "
                     "verifica que la norma diga lo que el escrito le atribuye.")
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "texto" (nlk:make-json-object
                    "type" "string"
                    "description" "El texto del escrito, o el pasaje con las citas.")
          "ruta" (nlk:make-json-object
                   "type" "string"
                   "description" "Ruta de un archivo .md o .txt con el escrito (alternativa a texto)."))))
  "The tool's schema: one of texto or ruta.")

;;; --- what the person reads -------------------------------------------------------------

(defun report (answer)
  "ANSWER, the JSON object, as the lines a person reads."
  (let ((rows (coerce (gethash "detalles" answer) 'list))
        (meta (gethash "meta" answer)))
    (with-output-to-string (out)
      (format out "dike: ~a (~d citas, ~d verificadas, ~d fallidas) corpus ~a al ~a"
              (gethash "veredicto" answer) (gethash "total_citas" meta)
              (gethash "citas_verificadas" meta) (gethash "citas_fallidas" meta)
              (gethash "pack_hash" meta) (gethash "as_of" meta))
      (dolist (row rows)
        (format out "~%~a  ~a~@[ - ~a~]"
                (gethash "cita_raw" row) (gethash "estado" row) (gethash "motivo" row)))
      (dolist (mention (coerce (gethash "menciones_sin_articulo" answer) 'list))
        (format out "~%sin artículo (no verificada): ~a" mention))
      (format out "~%~a" (gethash "mensaje" answer)))))

(defun run-slash (args session-id)
  "/dike with the brief's text after it: the citations, one to a line."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (zerop (length text))
        "dike: /dike seguido del escrito, o pide al modelo que use dike_verificar_citas con una ruta"
        (handler-case (report (check-citations :texto text))
          (dike-error (condition) (format nil "dike: ~a" condition))))))

(nle:define-cell dike
  (:section ("dike")
    (:guide "nothing is required: the statute corpus rides with the cell; as_of fixes the date vigencia is judged at (empty is today); max_bytes caps the brief")
    ("as_of" :string :default ""
     :doc "the date vigencia is judged at, YYYY-MM-DD; empty is today")
    ("max_bytes" :integer :default 400000 :min 1
     :doc "the longest brief, in UTF-8 bytes"))
  (:help :dike "dike_verificar_citas: every Chilean-law article citation of a brief, checked against a local corpus" +primer+)
  (:tool "dike_verificar_citas" +schema+ 'check-tool)
  (:command "dike" 'run-slash
   :catches 'dike-error
   :description "Check a brief's Chilean-law citations against the local corpus"
   :session nil))
