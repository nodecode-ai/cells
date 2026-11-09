;;;; census.lisp --- the bundled census snapshot, the index over it, the lookup.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; data/censo.json is the design system's own components.json, snapshotted:
;;;; every family (a component and its modifiers), every class it declares and
;;;; the pack each one lives in, the title and the words its author filed it
;;;; under, and the canonical markup lifted from the catalogue demo. Reading it
;;;; needs nothing but the image: no network, no subprocess, no write.
;;;;
;;;; The index is built once, on the first start or the first lookup after it:
;;;; one record a family, with the text a query is matched against folded into
;;;; it, so a query costs one pass over 400-odd records and no parsing.

(in-package #:nodecode-aurora)

(defparameter +censo-relative-path+ "data/censo.json"
  "Where the snapshot sits inside this folder.")

(defparameter +max-limit+ 20
  "How many families one lookup answers with, at most -- the original's cap.")

(defparameter +max-classes+ 25
  "How many class names a lookup lists for one family -- the original's cap.")

(defparameter *families* nil
  "The index: one FAMILY record a family, or NIL before the first read.")

(defparameter *known* nil
  "CLASS name => (PACK FAMILY), the census's class table, or NIL.")

(defparameter *packs* nil
  "PACK path => (NAME DESCRIPTION), or NIL.")

(defparameter *meta* nil
  "(:SYSTEM :VERSION :CHANNEL :CDN :FAMILIES :CLASSES) off the snapshot.")

;;; --- where the snapshot is --------------------------------------------------------

(defun cell-directory ()
  "The folder this cell's source sits in, which holds data/."
  ;; The .asd's own directory, which ASDF keeps even after a peripheral is
  ;; compiled into the cache; the loaded file's directory is the fallback for
  ;; a folder loaded by hand.
  (or (ignore-errors (asdf:system-source-directory "nodecode-aurora"))
      (ignore-errors (uiop:pathname-directory-pathname
                      (or *load-truename* *compile-file-truename*)))))

(defun censo-path ()
  "The snapshot's path in this folder."
  (merge-pathnames +censo-relative-path+ (cell-directory)))

;;; --- reading it -------------------------------------------------------------------

(defun load-censo (&optional (path (censo-path)))
  "PATH decoded into objects, or a config refusal naming what is missing."
  (let ((text (and path (nlk:read-text (namestring path)))))
    (unless text
      (nlk:config-error
       "the census snapshot (~a) is missing or unreadable: reinstall the folder, ~
data/censo.json ships with it"
       (or path "data/censo.json")))
    (let ((object (ignore-errors (nlk:decode-json text))))
      (unless (hash-table-p object)
        (nlk:config-error "the census snapshot (~a) is not a JSON object" path))
      object)))

(defun jfield (object key)
  "KEY's string member of OBJECT, whatever the decode made of its keys."
  (and (hash-table-p object) (nlk:json-value object :string key)))

(defun jarray (object key)
  "KEY's member of OBJECT as a list when it is an array, else an empty list."
  (let ((value (and (hash-table-p object) (nlk:json-array object key))))
    (if value (coerce value 'list) '())))

(defun jtable (object key)
  "KEY's member of OBJECT when it is an object, else an empty hash table."
  (or (and (hash-table-p object) (nlk:json-value object :object key))
      (make-hash-table :test #'equal)))

(defun entry-name (key)
  "A decoded object's key as a lowercase string: what a class or family is."
  (string-downcase (if (stringp key) key (string key))))

(defun text-list (object key)
  "KEY's members of OBJECT that are strings, each as a string."
  (loop for value in (jarray object key) when (stringp value) collect value))

;;; --- folding a query, and the text it is matched against ----------------------------

(defparameter +folds+
  (loop for (code . letter)
        in '((#x00e1 . #\a) (#x00e0 . #\a) (#x00e4 . #\a) (#x00e2 . #\a) (#x00e3 . #\a)
             (#x00e9 . #\e) (#x00e8 . #\e) (#x00eb . #\e) (#x00ea . #\e)
             (#x00ed . #\i) (#x00ec . #\i) (#x00ef . #\i) (#x00ee . #\i)
             (#x00f3 . #\o) (#x00f2 . #\o) (#x00f6 . #\o) (#x00f4 . #\o) (#x00f5 . #\o)
             (#x00fa . #\u) (#x00f9 . #\u) (#x00fc . #\u) (#x00fb . #\u)
             (#x00f1 . #\n) (#x00e7 . #\c))
        collect (cons (code-char code) letter))
  "The accents Spanish writes, folded to the letter underneath.")

(defun fold-char (character)
  "CHARACTER, downcased and unaccented."
  (let ((plain (char-downcase character)))
    (or (cdr (assoc plain +folds+ :test #'char=)) plain)))

(defun normalize (text)
  "TEXT lowercased and unaccented, which is how a query and the census meet."
  (with-output-to-string (out)
    (loop for character across (if (stringp text) text (princ-to-string (or text "")))
          do (write-char (fold-char character) out))))

(defparameter +stopwords+
  '("con" "sin" "que" "los" "las" "del" "para" "una" "uno" "por" "como" "mas" "muy"
    "the" "and" "for" "with" "sus" "este" "esta" "dos")
  "Words a query carries that name nothing in the census.")

(defun tokens (text)
  "TEXT's words, normalized, three letters or more, stopwords dropped."
  (let ((plain (normalize text)) (words '()) (start nil))
    (labels ((emit (end)
               (when start
                 (let ((word (subseq plain start end)))
                   (unless (or (< (length word) 3) (member word +stopwords+ :test #'string=))
                     (push word words)))
                 (setf start nil))))
      (loop for index from 0 below (length plain)
            for character = (char plain index)
            do (if (or (alphanumericp character)
                       (member character '(#\- #\_) :test #'char=))
                   (unless start (setf start index))
                   (emit index)))
      (emit (length plain)))
    (nreverse words)))

;;; Spanish is how people ask ("tabla que se apila en movil", "aviso de error") and
;;; the census is filed in a mix of both: expanding the query beats asking the model
;;; to translate first. The table is the original's, entry for entry.

(defparameter +synonyms+
  '(("aviso" "alert" "callout" "alerta")
    ("alerta" "alert" "aviso")
    ("error" "danger" "error" "fallo")
    ("acceso" "login" "acceso" "sesion" "auth")
    ("entrar" "login" "acceso")
    ("login" "login" "acceso")
    ("formulario" "form" "formgrid" "field" "input" "campo")
    ("campo" "field" "input" "campo")
    ("tabla" "table" "tabla")
    ("boton" "btn" "boton")
    ("grafico" "chart" "grafico" "barras" "linea")
    ("grafica" "chart" "grafico")
    ("tarjeta" "article" "card" "bento" "kpi" "tarjeta")
    ("chat" "chat" "msg" "prompt" "mensaje")
    ("conversacion" "chat" "msg")
    ("menu" "nav" "navbar" "menu" "tabs")
    ("navegacion" "nav" "navbar" "breadcrumb" "tabs")
    ("cargando" "carga" "skeleton" "spinner" "loading")
    ("modal" "modal" "drawer" "sheet" "dialogo")
    ("dialogo" "modal" "drawer" "sheet")
    ("fichero" "file" "zona" "upload" "fichero")
    ("subir" "upload" "file" "zona")
    ("precio" "plan" "precio" "price" "pricing")
    ("icono" "icon" "icono")
    ("buscador" "search" "buscador" "buscar")
    ("pasos" "wizard" "stepper" "pasos" "formgrid")
    ("metrica" "kpi" "metrica" "stat" "tendencia")
    ("calendario" "calendar" "calendario")
    ("etiqueta" "badge" "tag" "etiqueta")
    ("desplegable" "dropdown" "select" "desplegable")
    ("acordeon" "acordeon" "preguntas" "faq" "details")
    ("informe" "informe" "report" "print")
    ("panel" "dash" "panel" "appshell" "kanban")
    ("esqueleto" "skeleton" "carga")
    ("apila" "apilable" "responsive")
    ("apilable" "apilable")
    ("movil" "apilable" "responsive")
    ("responsive" "responsive" "apilable")
    ("peligroso" "danger"))
  "A query word, then the census words it stands for: the original's table.")

(defun terms (word)
  "WORD and what it stands for, in the order they are tried."
  (let ((row (assoc word +synonyms+ :test #'string=)))
    (if row (cdr row) (list word))))

;;; --- the index ---------------------------------------------------------------------

(defstruct (family (:conc-name fam-))
  "One census family: what it is called, where it lives, and the text a query meets."
  name pack title demo markup classes class-norms avoid norm words haystack)

(defun build-index (census)
  "Fill the three tables off CENSUS and answer its metadata."
  (let ((known (make-hash-table :test #'equal))
        (packs (make-hash-table :test #'equal))
        (families '())
        (family-table (jtable census "familias"))
        (class-table (jtable census "clases"))
        (pack-table (jtable census "packs")))
    (maphash (lambda (path pack)
               (setf (gethash (entry-name path) packs)
                     (list (or (jfield pack "nombre") "") (or (jfield pack "descripcion") ""))))
             pack-table)
    (maphash (lambda (class entry)
               (setf (gethash (entry-name class) known)
                     (list (jfield entry "pack") (jfield entry "familia"))))
             class-table)
    (maphash
     (lambda (name entry)
       (let* ((pack (jfield entry "pack"))
              (meta (gethash (normalize (or pack "")) packs))
              (avoid (subseq (text-list entry "noUsar") 0
                             (min 3 (length (text-list entry "noUsar")))))
              (palabras (text-list entry "palabras"))
              (classes (text-list entry "clases")))
         (push (make-family
                :name (entry-name name)
                :pack pack
                :title (or (jfield entry "titulo") "")
                :demo (jfield entry "demo")
                :markup (or (jfield entry "markup") "")
                :classes classes
                :class-norms (mapcar #'normalize classes)
                :avoid avoid
                :words (mapcar #'normalize palabras)
                :norm (normalize (entry-name name))
                :haystack (normalize (format nil "~a ~{~a~^ ~} ~{~a~^ ~} ~a ~a"
                                             (or (jfield entry "titulo") "")
                                             palabras avoid
                                             (first meta) (second meta))))
                families)))
     family-table)
    (setf *families* (nreverse families) *known* known *packs* packs
          *meta* (list :system (or (jfield census "sistema") "Aurora")
                        :version (or (jfield census "version") "")
                        :channel (or (jfield census "canal") "")
                        :cdn (or (jfield census "cdn") "")
                        :families (hash-table-count family-table)
                        :classes (hash-table-count class-table)))
    *meta*))

(defun ensure-census (&optional (path (censo-path)))
  "Read PATH and index it when nothing is indexed yet; answer the metadata."
  (or *meta* (build-index (load-censo path))))

(defun census-info ()
  "(:SYSTEM :VERSION :CHANNEL :CDN :FAMILIES :CLASSES) of the indexed snapshot."
  (ensure-census))

;;; --- scoring -----------------------------------------------------------------------

(defun family-base (name)
  "A name without the namespace: article for nz-article."
  (let ((prefix "nz-"))
    (if (and (> (length name) (length prefix))
             (string= prefix name :end2 (length prefix)))
        (subseq name (length prefix))
        name)))

(defun score-family (words family)
  "The score WORDS earn on FAMILY and the terms that matched, on the original's
scale: a family name 10, a class 6, the filing text 2, and two more for a
family whose own base class exists."
  (let ((score 0) (matched '()))
    (flet ((hit (term points) (incf score points) (pushnew term matched :test #'string=)))
      (dolist (word words)
        (dolist (term (terms word))
          (cond ((cl:search term (fam-norm family)) (hit term 10))
                ((some (lambda (class) (cl:search term class)) (fam-class-norms family))
                 (hit term 6))
                ((and (> (length term) 3) (cl:search term (fam-haystack family)))
                 (hit term 2)))))
      (let ((base (family-base (fam-name family))))
        (when (some (lambda (class) (string= base (family-base class))) (fam-class-norms family))
          (incf score 2))))
    (values score (sort matched #'string<))))

(defun ranked (words limit)
  "The LIMIT best families for WORDS: highest score, then the shorter name."
  (let ((hits '()))
    (dolist (family *families*)
      (multiple-value-bind (score matched) (score-family words family)
        (when (plusp score) (push (list score family matched) hits))))
    (subseq (sort hits (lambda (a b)
                         (let ((left (first a)) (right (first b)))
                           (or (> left right)
                               (and (= left right)
                                    (or (< (length (fam-name (second a)))
                                           (length (fam-name (second b))))
                                        (and (= (length (fam-name (second a)))
                                                (length (fam-name (second b))))
                                             (string< (fam-name (second a))
                                                      (fam-name (second b))))))))))
            0 (min limit (length hits)))))

;;; --- the lookup --------------------------------------------------------------------

(defun section-limit ()
  "The section's own limit, or the original's default outside a running cell."
  (let ((value (ignore-errors (setting :limit))))
    (if (integerp value) value 5)))

(defun clamp-limit (limit fallback)
  "LIMIT within 1 and +MAX-LIMIT+, or FALLBACK when it names none."
  (let ((value (or limit fallback 5)))
    (max 1 (min +max-limit+ (if (integerp value) value (or fallback 5))))))

(defun census-cdn ()
  "The CDN base a page links, with the channel the section pins when it pins one."
  (let ((channel (ignore-errors (setting :channel))))
    (if (and (stringp channel) (plusp (length channel)))
        (format nil "https://cdn.jsdelivr.net/gh/Ntizar/Aurora@~a/" channel)
        (or (and *meta* (getf *meta* :cdn))
            "https://cdn.jsdelivr.net/gh/Ntizar/Aurora@v10/"))))

(defun render-family (result position with-markup)
  "One family of a lookup, as the lines a model reads."
  (destructuring-bind (score family matched) result
    (with-output-to-string (out)
      (format out "[~d] ~a -- score ~d -- ~a~%" position (fam-name family) score
              (fam-title family))
      (format out "    pack: ~a~@[ -- demo: ~a~]~%" (or (fam-pack family) "none")
              (fam-demo family))
      (when (fam-classes family)
        (format out "    classes: ~{~a~^ ~}~%"
                (subseq (fam-classes family) 0
                        (min +max-classes+ (length (fam-classes family))))))
      (when (fam-avoid family)
        (format out "    do not use: ~{~a~^, ~}~%" (fam-avoid family)))
      (when matched
        (format out "    matched: ~{~a~^, ~}~%" matched))
      (when (and with-markup (fam-markup family) (plusp (length (fam-markup family))))
        (format out "    markup: ~a~%" (fam-markup family))))))

(defun lookup (need &key limit (with-markup t))
  "NEED in plain words (Spanish or English) => what the census holds for it: one
block a family, with the pack to link, every class, the demo and the real
markup. LIMIT caps the families (1 to 20; the section's value when NIL)."
  (let ((text (and (stringp need)
                   (string-trim '(#\Space #\Tab #\Newline #\Return) need))))
    (if (or (null text) (zerop (length text)))
        "ERROR: name what you need to build, in plain words: aurora_search \"table that stacks on mobile\"."
        (let* ((meta (ensure-census))
               (limit (clamp-limit limit (section-limit)))
               (words (tokens text))
               (hits (and words (ranked words limit))))
          (if (null hits)
              (format nil "Nothing in the Aurora census matched \"~a\": try the words a class ~
or a title uses (table, alert, kpi, chat, chart), in Spanish or English.~%Link only ~
the packs you use: ~a<pack>"
                      text (census-cdn))
              (with-output-to-string (out)
                (format out "~a ~a (~d families, channel ~a). need: \"~a\"~%"
                        (getf meta :system) (getf meta :version)
                        (getf meta :families) (getf meta :channel) text)
                (loop for result in hits
                      for position from 1
                      do (write-string (render-family result position with-markup) out))
                (format out "Link only the packs you use, one link a pack: ~a<pack>. ~
tokens and the packs come before any marcas/*.css.~%"
                        (census-cdn))))))))
