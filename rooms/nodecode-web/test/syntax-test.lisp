;;;; syntax-test.lisp --- a file's text in colour (page/syntax.js).
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(deftest web-cell-syntax-paints-each-language ()
  ;; syntax-test.mjs runs the page's own module in node: each language's
  ;; marks, the state a line hands down, the text kept, and a painter fed
  ;; edits drawing what a fresh one draws.
  (let* ((runner (asdf:system-relative-pathname "nodecode-web" "test/syntax-test.mjs"))
         (module (merge-pathnames "syntax.js" nodecode-web::*page-directory*))
         (output (uiop:run-program (list "node" (namestring runner) (namestring module))
                                   :output :string :error-output :output :ignore-error-status t))
         (last (car (last (uiop:split-string (string-trim '(#\Newline) output) :separator '(#\Newline))))))
    (is (and (search "SYNTAX " last) (search " 0 failed" last)) (format nil "syntax.js: ~a" output))))

(defun web-resolved-color (declarations light name &optional (depth 0))
  "The color the theme gives NAME, a token whose value may itself be var(--other)."
  (let ((value (loop for selector in (if light '(":root[data-theme=\"light\"]" ":root") '(":root"))
                     thereis (loop for (rule . declaration) in declarations
                                   when (and (string= rule selector) (eql 0 (search (format nil "~a:" name) declaration)))
                                     return (string-trim " " (subseq declaration (1+ (length name))))))))
    (cond ((null value) nil)
          ((and (eql 0 (search "var(" value)) (< depth 4))
           (web-resolved-color declarations light (subseq value 4 (position #\) value)) (1+ depth)))
          (t (web-css-color value)))))

(deftest web-cell-code-colours-hold-aa-on-the-files-ground ()
  ;; Every colour a file's text takes is read on the Files view's ground
  ;; (--inset), in both themes, and paints through a token.
  (let* ((css (web-page-text "app.css"))
         (declarations (web-css-declarations css))
         (tokens (loop for (rule . declaration) in declarations
                       when (and (string= rule ":root") (eql 0 (search "--sx-" declaration))
                                 (not (eql 0 (search "--sx-selection" declaration))))
                         collect (subseq declaration 0 (position #\: declaration))))
         (short '()))
    (is (<= 11 (length tokens)) "the code colours are root tokens")
    (dolist (light '(nil t))
      (let ((ground (web-resolved-color declarations light "--inset")))
        (dolist (token tokens)
          (let ((color (web-resolved-color declarations light token)))
            (unless (and color (>= (web-contrast color ground) 4.5d0))
              (push (format nil "~a~:[~; (light)~]~@[ ~,2f~]" token light (and color (web-contrast color ground))) short))))))
    (is (null short) (format nil "code colours under 4.5:1 on --inset: ~{~a~^, ~}" short))
    (let ((widening (loop for (rule . declaration) in declarations
                          when (and (eql 0 (search ".sx-" rule)) (or (search "font" declaration) (search "weight" declaration)))
                            collect rule)))
      (is (null widening) "colour only: no code mark changes a letter's width"))
    (is (search "/21px" css) "the code views' lines stand whole pixels apart")))
