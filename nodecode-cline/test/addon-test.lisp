;;;; addon-test.lisp --- the cline add-on against a scripted store and feed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store here is a temp auth.json named through :auth-path, and every
;;;; wire is a stubbed nle:http-fetch — the core suite's own seams (WITH-TEMP-AUTH,
;;;; WITH-STUBBED-FDEFINITION). Nothing touches the network and nothing reads
;;;; the operator's own auth.json. The served lane is openai-completions, a
;;;; lane every image knows without a catalog; which lane is served is the
;;;; section's `provider', and cline-pass is only its default.

(in-package #:nodecode.test)

(define-test-slice "cline" "CLINE-ADDON-")

(define-addon-config "cline" nodecode-cline:start-addon)

(define-addon-lifecycle-tests "cline"
  (:hooks 'nle::resolve-provider-credential 'nle::list-provider-models)
  (:refused ("client_type" 5)))

;;; --- the fixtures -----------------------------------------------------------

(defparameter +cline-feed+
  (concatenate 'string
               "{\"recommended\":[{\"id\":\"moonshotai/kimi-k3\",\"name\":\"kimi-k3\"}],"
               "\"free\":[{\"id\":\"cline-free/deepseek-v4.1-flash\",\"name\":\"Deepseek-v4.1-Flash\"},"
               "{\"id\":\"poolside/laguna-s-2.1:free\",\"name\":\"laguna-s-2.1:free\"}],"
               "\"clinePass\":[{\"id\":\"cline-pass/kimi-k3\",\"name\":\"cline-pass/kimi-k3\"},"
               "{\"id\":\"cline-free/deepseek-v4.1-flash\",\"name\":\"twice\"}]}")
  "A feed in api.cline.bot's shape: four buckets of {id, name}, one id in two.")

(defun cline-served-start ()
  "The add-on started on the openai-completions lane."
  (cline-start "provider" "openai-completions"))

(defmacro with-cline-feed ((&optional (body '+cline-feed+)) &body forms)
  "Run FORMS with every NLE:HTTP-FETCH answering BODY, or failing as a
refused dial when BODY is :unreachable."
  `(with-stubbed-fdefinition
       (nle:http-fetch (url &rest args)
        (if (eq ,body :unreachable)
            (values nil "connection refused" t)
            ,body))
     ,@forms))

(defun cline-ids (rows)
  "The ids of listing ROWS, in order."
  (mapcar (lambda (row) (getf row :id)) rows))

(defun cline-listing (rows &optional error)
  "LIST-WITH-FEED on the served lane over a listing that answers ROWS and ERROR."
  (flet ((listing (provider) (declare (ignore provider)) (values rows error)))
    (nodecode-cline::list-with-feed #'listing "openai-completions")))

;;; --- the client header ------------------------------------------------------

(deftest cline-addon-names-the-client-on-the-served-lane (with-addon-stop ((cline-served-start)))
  ;; The key is auth.json's — the tier the :CREDENTIAL point never sees,
  ;; which is why this is advice on the resolver — and the header rides it
  ;; into the frozen config the request path reads.
  (with-temp-auth (auth "{\"api_keys\":{\"openai-completions\":{\"provider\":\"openai-completions\",\"key\":\"sk-cline\"},\"anthropic\":{\"provider\":\"anthropic\",\"key\":\"sk-ant\"}}}")
    (let* ((credential (nle::resolve-provider-credential "openai-completions" :auth-path auth))
           (headers (getf (nle:credential-attributes credential) :headers)))
      (is (equal "sk-cline" (nle:credential-key credential)) "the key is untouched")
      (is (eq :api-key (nle:credential-source credential)) "and so is where it came from")
      (is (equal "nodecode" (nlk:header-value headers "x-client-type")) "named nodecode")
      (is (equal nle::*user-agent-version* (nlk:header-value headers "x-client-version"))))
    ;; another lane's credential is untouched
    (is (null (nle:credential-attributes
               (nle::resolve-provider-credential "anthropic" :auth-path auth))))
    (nlk:bind ((nle::*provider* "openai-completions") (nle::*api-key* nil)
               (nle::*auth-file-path* auth)
               (config (nle::compiled-turn-context-provider-config (user-context))))
      ;; the frozen config carries the header to the request
      (is (equal "nodecode" (nlk:header-value (nle::credential-attribute config :headers)
                                              "x-client-type"))))))

(deftest cline-addon-client-type-is-the-section-s ()
  (with-addon-stop ((cline-start "provider" "openai-completions" "client_type" "my-shell"))
    (with-temp-auth (auth "{\"api_keys\":{\"openai-completions\":{\"provider\":\"openai-completions\",\"key\":\"k\"}}}")
      (is (equal "my-shell"
                 (nlk:header-value (getf (nle:credential-attributes
                                          (nle::resolve-provider-credential "openai-completions"
                                                                            :auth-path auth))
                                         :headers)
                                   "x-client-type"))))))

;;; --- the feed ---------------------------------------------------------------

(deftest cline-addon-reads-the-feed-s-buckets-in-order ()
  ;; clinePass then free by default; an id in two buckets is listed once,
  ;; under the first; the free bucket's rows say they are free, once; a name
  ;; that is only the id again is no display.
  (let ((rows (nodecode-cline::feed-rows-from +cline-feed+ '("clinePass" "free"))))
    (is (equal '("cline-pass/kimi-k3" "cline-free/deepseek-v4.1-flash"
                 "poolside/laguna-s-2.1:free")
               (cline-ids rows)))
    (is (equal '(nil "twice" "laguna-s-2.1 (free)")
               (mapcar (lambda (row) (getf row :display)) rows))))
  (is (equal '("Deepseek-v4.1-Flash (free)" "laguna-s-2.1 (free)")
             (mapcar (lambda (row) (getf row :display))
                     (nodecode-cline::feed-rows-from +cline-feed+ '("free")))))
  ;; a body that is not the feed lists nothing
  (is (null (nodecode-cline::feed-rows-from "<html>captive portal</html>" '("free")))))

(deftest cline-addon-adds-the-feed-to-the-served-listing (with-addon-stop ((cline-served-start)))
  (with-cline-feed ()
    ;; The advice over a listing that answered: the feed's rows lead, and
    ;; an id both carry is the feed's row.
    (multiple-value-bind (rows error)
        (cline-listing '((:id "moonshotai/kimi-k3") (:id "cline-pass/kimi-k3" :display "listed")))
      (is (null error))
      (is (equal '("cline-pass/kimi-k3" "cline-free/deepseek-v4.1-flash"
                   "poolside/laguna-s-2.1:free" "moonshotai/kimi-k3")
                 (cline-ids rows))))
    ;; Through the kernel's own function: a lane with no listing of its own
    ;; still lists the feed, and its failure is no longer the answer.
    (multiple-value-bind (rows error) (nle::list-provider-models "openai-completions")
      (is (null error) "the feed answered, so there is something to pick")
      (is (member "cline-free/deepseek-v4.1-flash" (cline-ids rows) :test #'equal)))
    (let ((rows (nle::list-provider-models "anthropic")))
      ;; another lane's listing is its own
      (is (null (member "cline-free/deepseek-v4.1-flash" (cline-ids rows) :test #'equal))))))

(deftest cline-addon-unreachable-feed-leaves-the-listing (with-addon-stop ((cline-served-start)))
  (with-cline-feed (:unreachable)
    (multiple-value-bind (rows error) (cline-listing '((:id "a/b")))
      (is (equal '("a/b") (cline-ids rows)) "the listing stands")
      (is (null error)))
    (multiple-value-bind (rows error) (cline-listing nil "HTTP 401")
      (is (null rows))
      (is (equal "HTTP 401" error) "nothing listed, so the listing's reason stands"))))
