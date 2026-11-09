;;;; cell.lisp --- the doctrine, the section, the two tools, /aurora.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin gave its agent four skills and two tools. The tools are here as
;;;; tools of the same names, and the doctrine the skills carried is what
;;;; (help :aurora) answers and what the second paragraph of each tool's
;;;; description points at: the rules a page has to keep, the way a version is
;;;; resolved and linked, and the order -- look the object up, write it, check
;;;; it before delivering.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "aurora": {"limit": 5, "channel": "v10",
;;;;              "footer": "Hecho con <3 por ...", "require_footer": true}
;;;; Nothing here needs a key: the census ships in data/censo.json, so a
;;;; section that is absent runs on the defaults below and one that is
;;;; malformed refuses the start and names the member.

(in-package #:nodecode-aurora)

;;; --- the attribution the system asks for ------------------------------------------

(defun default-footer ()
  "The footer the design system requires verbatim: its author's words, with the
heart it writes in them."
  (format nil "Hecho con ~c~c por David Antizar"
          (code-char #x2764) (code-char #xfe0f)))

(defparameter +footer+ (default-footer)
  "The attribution a page must carry, as the section defaults it.")

(defparameter +channel+ "v10"
  "The living channel the snapshot names, which a page links.")

;;; --- the manual ---------------------------------------------------------------------

(defparameter +primer+
  "Aurora is a pure CSS design system: no build step, no JavaScript, every public
class namespaced .nz-. The nodecode-aurora cell carries its catalogue offline, so a page
is built from objects that exist instead of class names that might.
  aurora_search(need)      what the catalogue holds for a need, in Spanish or English:
                            the family, the pack to link, every class, the demo and the
                            real markup. Use it instead of guessing a class name.
  aurora_validate(path|html)  a page measured: classes the census does not know, packs
                            used but not linked, a gradient, a backdrop-filter, an
                            !important, a hand-written colour, an emoji in the UI, two
                            h1, the missing attribution footer -- and the warnings a
                            person reads.
  /aurora WHAT YOU NEED    the same lookup from the command line.
Rules the system's own CI enforces, and the check fails a page for: only .nz-* classes,
values as var(--nz-*) tokens, no gradient and no backdrop-filter and no !important (the
crystal layer and the hiding helpers excepted), BEM with .nz-thing__part/--modifier and
state via .is-x or [aria-*], mobile-first with min-width breaks at 640/960/1200/1440, a
44px touch target, one h1, and the footer verbatim: Hecho con <3 por David Antizar.
Typography: 13px is the interface minimum, 12px is data only, weight is a token, never
load Google Fonts. Copy in Spanish.
Linking: tokens.css first, then only the packs the page uses, then any marcas/*.css --
tokens and marcas share a specificity, so the later file wins. Pin a tag for delivery,
and never @master in a <link>. `<body class=\"nz\">`, `<html lang=\"es\"
data-nz-theme=\"light\">`; dark is the theme attribute, light is the one to design first.
Order of work: search the object, write the page from the markup the search returned, then
validate before delivering; warnings are read, failures block."
  "What (help :aurora) answers while the cell runs.")

;;; --- the boundaries -----------------------------------------------------------------

(nlk:define-contract aurora-search
  (:doc "Look up Aurora design-system objects by what you need to build, in Spanish or
English ('tabla que se apila en movil', 'kpi', 'chat', 'alert'). Answers the family, the
CSS pack to link, every class and the real markup from the catalogue. Offline: the census
ships with the cell. Use it instead of guessing class names.")
  ("need" :text :required t :doc "what you need to build, in plain words")
  ("limit" :integer :doc "how many families to answer with (default from the section, at most 20)")
  ("with_markup" :boolean :default t :doc "include each family's canonical markup"))

(nlk:define-contract aurora-page
  (:doc "Validate an Aurora page offline: failures (must fix) and warnings (read them),
with the classes, families and packs the page measured. Pass a file path, or the HTML
itself when there is no file on disk.")
  ("path" :string :doc "the path of the HTML file to check")
  ("html" :text :doc "the page's HTML, when it is not on disk"))

(defun expand-path (path)
  "PATH with a leading ~ read the way a shell reads it."
  (if (and (stringp path) (plusp (length path)) (char= #\~ (char path 0)))
      (namestring (merge-pathnames (subseq path (min 2 (length path))) (user-homedir-pathname)))
      path))

(defun read-page (path)
  "PATH's HTML as a string; the two refusals the original had, as text a model reads."
  (let* ((name (expand-path path))
         (text (and (stringp name) (plusp (length name)) (nlk:read-text name))))
    (cond ((or (null name) (zerop (length (princ-to-string name))))
           "ERROR: aurora_validate wants a page: pass \"path\" or \"html\".")
          ((null text) (format nil "ERROR: no such file: ~a" name))
          ((> (length text) +max-input+)
           (format nil "ERROR: file too large (~d bytes; ~d at most)" (length text) +max-input+))
          (t text))))

(defun search-tool (request)
  "aurora_search -- the lookup, as the lines a model reads."
  (lookup (aurora-search-need request)
          :limit (aurora-search-limit request)
          :with-markup (and (aurora-search-with-markup request) t)))

(defun validate-tool (page)
  "aurora_validate -- a page read off disk or out of the call, then measured."
  (let ((source (or (aurora-page-html page)
                    (let ((path (aurora-page-path page)))
                      (if path (read-page path)
                          "ERROR: aurora_validate wants a page: pass \"path\" or \"html\".")))))
    (if (and (stringp source) (>= (length source) 6) (string= "ERROR:" source :end2 6))
        source
        (let* ((footer (setting :footer))
               (require-footer (setting :require-footer)))
          (when (> (length source) +max-input+)
            (return-from validate-tool (format nil "ERROR: html too large (~d bytes; ~d at most)"
                                               (length source) +max-input+)))
          (multiple-value-bind (ok failures warnings measured)
              (check source :footer footer :require-footer require-footer)
            (report-text ok failures warnings measured))))))

;;; --- the command --------------------------------------------------------------------

(defun run-slash (args session-id)
  "The /aurora command: ARGS is what the operator needs to build."
  (declare (ignore session-id))
  (let ((need (and (stringp args) (string-trim '(#\Space #\Tab) args))))
    (if (or (null need) (zerop (length need)))
        (format nil "aurora: type /aurora and what you need to build, e.g. /aurora table ~
that stacks on mobile. The census holds ~d families and ~d classes; the tools are ~
aurora_search and aurora_validate."
                (getf (ensure-census) :families) (getf (ensure-census) :classes))
        (lookup need :limit (setting :limit)))))

;;; --- the declaration ----------------------------------------------------------------

(nle:define-cell aurora
  (:section ("aurora")
    (:guide "nothing here is required: the census ships with the folder. limit is how many families a lookup that names none answers with; channel is the Aurora channel (or pinned tag) a page links; footer and require_footer say whether a page must carry the system's attribution")
    ("limit" :integer :default 5 :min 1
             :doc "how many families a lookup answers with when the call names none (1 to 20)")
    ("channel" :string :default "v10"
               :doc "the Aurora channel or pinned tag the pages a lookup builds should link (v10, or a tag such as v10.10.0)")
    ("footer" :string :default +footer+
              :doc "the attribution a page must carry verbatim, as the system requires it")
    ("require_footer" :boolean :default t
                      :doc "whether a page without the attribution fails the check"))
  (:tool "aurora_search" (nlk:contract 'aurora-search) #'search-tool)
  (:tool "aurora_validate" (nlk:contract 'aurora-page) #'validate-tool)
  (:command "aurora" 'run-slash
            :description "Look up Aurora design-system objects for what you need to build"
            :argument-hint "WHAT YOU NEED TO BUILD")
  (:help :aurora
   "aurora: build web pages with the Aurora CSS system - aurora_search, aurora_validate, /aurora"
   +primer+)
  (:start (lambda () (ensure-census))))
