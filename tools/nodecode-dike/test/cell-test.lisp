;;;; cell-test.lisp --- the cell against the pack it ships and the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test reads the bundled tables and nothing else: the pack is the whole
;;;; corpus, so no exchange is stubbed and none happens.

(in-package #:nodecode.test)

(define-test-slice "dike" "DIKE-CELL-" :start nodecode-dike:start-cell)

(define-cell-lifecycle-tests "dike"
  (:help :dike)
  (:command "dike")
  (:running (is (stringp (dike:corpus))) (is (search "statutes" (dike:corpus))))
  (:refused ("max_bytes" 0) ("max_bytes" "many"))
  (:idle dike:dike-error (dike:check-citations :texto "art. 1 del Código Civil")))

;;; --- fixtures -----------------------------------------------------------------

(defun dike-answer (text &key (as-of "2026-11-09"))
  "The JSON answer for TEXT at AS-OF, decoded."
  (nlk:decode-json (nlk:encode-json-object (nodecode-dike::verify text :as-of as-of))))

(defun dike-rows (answer)
  "The citation rows of ANSWER, as a list."
  (coerce (nlk:json-value answer :array "detalles") 'list))

;;; --- a citation the pack proves ------------------------------------------------

(deftest dike-cell-verifies-an-article-of-a-code ()
  (with-cell-stop ((dike-start))
    (let* ((answer (dike-answer "El artículo 1545 del Código Civil dispone que el deudor responde."))
           (rows (dike-rows answer)))
      (is (equal "TODAS_VERIFICADAS" (nlk:json-value answer :string "veredicto")))
      (is (= 1 (length rows)))
      (let ((row (first rows)))
        (is (equal "VERIFICADA" (nlk:json-value row :string "estado")))
        (is (equal "cl.norma.codigo_civil:art:1545" (nlk:json-value row :string "gid")))
        (is (equal "IN_FORCE" (nlk:json-value row :string "vigencia")))
        (is (= 64 (length (nlk:json-value row :string "texto_hash"))))
        (is (search "leychile.cl" (nlk:json-value row :string "fuente_url"))))
      (is (equal "af1422d66d1e6eee" (nlk:json-value answer :string "meta" "pack_hash")))
      (is (= 1 (nlk:json-value answer :integer "meta" "citas_verificadas"))))))

(deftest dike-cell-reads-a-whole-enumeration ()
  (with-cell-stop ((dike-start))
    (let* ((answer (dike-answer "Los artículos 1545, 1546 y 1547 del Código Civil se aplican."))
           (rows (dike-rows answer)))
      (is (= 3 (length rows)) "three citations, not one")
      (is (equal '("cl.norma.codigo_civil:art:1545" "cl.norma.codigo_civil:art:1546"
                   "cl.norma.codigo_civil:art:1547")
                 (mapcar (lambda (row) (nlk:json-value row :string "gid")) rows)))
      (is (equal "TODAS_VERIFICADAS" (nlk:json-value answer :string "veredicto"))))))

(deftest dike-cell-says-what-it-could-not-verify ()
  (with-cell-stop ((dike-start))
    (let* ((answer (dike-answer "El artículo 9999 del Código Civil y el artículo 4 de la Ley 19.880."))
           (rows (dike-rows answer)))
      (is (equal "CON_PROBLEMAS" (nlk:json-value answer :string "veredicto")))
      (is (= 2 (length (nlk:json-value answer :array "no_verificadas"))))
      (is (search "no está en el corpus" (nlk:json-value (first rows) :string "motivo")))
      (is (equal "UNRESOLVABLE" (nlk:json-value (second rows) :string "resolucion")) "ley 19.880 is outside the pack")
      (let ((mentions (coerce (nlk:json-value answer :array "menciones_sin_articulo") 'list)))
        (is (find "ley 19.880" mentions :test #'equal) "the mention is disclosed")))))

(deftest dike-cell-refuses-a-citation-with-no-statute ()
  (with-cell-stop ((dike-start))
    (let* ((answer (dike-answer "El artículo 5 dispone lo contrario."))
           (row (first (dike-rows answer))))
      (is (equal "CON_PROBLEMAS" (nlk:json-value answer :string "veredicto")))
      (is (equal "UNRESOLVABLE" (nlk:json-value row :string "resolucion")))
      (is (search "ninguna norma" (nlk:json-value row :string "motivo"))))
    (is (equal "SIN_CITAS" (nlk:json-value (dike-answer "Una cláusula sin números.") :string "veredicto")))))

(deftest dike-cell-judges-vigencia-at-the-date-asked ()
  (with-cell-stop ((dike-start))
    (let ((row (first (dike-rows (dike-answer "El artículo 73 de la Ley 20.720." :as-of "2026-11-09")))))
      (is (equal "FALLO" (nlk:json-value row :string "estado")))
      (is (search "fuera de vigencia" (nlk:json-value row :string "motivo"))))
    (let ((row (first (dike-rows (dike-answer "El artículo 14 quater de la Ley 21.719." :as-of "2026-11-09")))))
      (is (equal "FALLO" (nlk:json-value row :string "estado")))
      (is (search "aún no vigente" (nlk:json-value row :string "motivo"))))))

;;; --- the tool and the command ----------------------------------------------------

(deftest dike-cell-the-tool-answers-json ()
  (with-cell-stop ((dike-start))
    (let ((arguments (make-hash-table :test #'equal)))
      (setf (gethash "texto" arguments) "El artículo 1545 del Código Civil.")
      (let ((answer (nlk:decode-json (nodecode-dike::check-tool arguments))))
        (is (equal "TODAS_VERIFICADAS" (nlk:json-value answer :string "veredicto")))
        (is (equal "escrito" (nlk:json-value answer :string "meta" "documento"))))
      (setf (gethash "texto" arguments) "")
      (let ((answer (nlk:decode-json (nodecode-dike::check-tool arguments))))
        (is (equal "DIKE_ERROR" (nlk:json-value answer :string "error_code")))))))

(deftest dike-cell-reads-a-brief-from-a-file ()
  (with-cell-stop ((dike-start))
    (let ((path (merge-pathnames "dike-cell-test-brief.md" (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
               (write-string "El artículo 1546 del Código Civil." out))
             (is (equal "TODAS_VERIFICADAS"
                        (nlk:json-value (nlk:decode-json
                                          (nlk:encode-json-object
                                           (dike:check-citations :ruta (namestring path))))
                                        :string "veredicto")))
             (is (search "dike: CON_PROBLEMAS"
                         (dike::run-slash "El artículo 5 dispone." nil))))
        (ignore-errors (delete-file path))))))
