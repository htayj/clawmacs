(in-package :clawmacs/tests)

(in-suite netcons-suite)

(defparameter *netcons-search-fixture*
  "<html><body>
<div class=\"result\">
<a rel=\"nofollow\" class=\"result__a\" href=\"/l/?kh=-1&amp;uddg=https%3A%2F%2Fexample.com%2Fpage\">Example Page</a>
<a class=\"result__snippet\">A snippet about Alpha and Beta.</a>
</div>
</body></html>")

(defparameter *netcons-page-fixture*
  "<html><head><title>Fetched Example</title></head>
<body>
<main>
<h1>Fetched Example</h1>
<p>Alpha appears here.</p>
<p>Needle appears on the next line.</p>
</main>
</body></html>")

(defmacro with-netcons-function-override ((name lambda-list &body implementation)
                                          &body body)
  `(let ((original-function (symbol-function ',name)))
     (unwind-protect
          (progn
            (setf (symbol-function ',name)
                  (lambda ,lambda-list
                    ,@implementation))
            ,@body)
       (setf (symbol-function ',name) original-function))))

(defmacro with-netcons-package-state (&body body)
  "Run BODY with isolated package, project, and tool registries."
  `(let* ((root (temp-package-test-directory "netcons-config"))
          (clawmacs::*agent-tool-metadata-table*
           (make-hash-table :test #'eq))
          (clawmacs::*agent-tool-name-table*
           (make-hash-table :test #'equal))
          (clawmacs::*tool-table* (make-hash-table :test #'equal))
          (clawmacs::*project-registry* (make-hash-table :test #'equal))
          (clawmacs::*project-definitions-loaded-p* nil)
          (clawmacs::*package-configuration-path*
           (merge-pathnames "packages.json" root))
          (clawmacs::*package-configuration* nil)
          (clawmacs::*package-channels* (default-package-test-channels))
          (clawmacs::*available-packages* nil)
          (clawmacs::*package-registry-loaded-p* nil)
          (clawmacs::*loaded-packages* (make-hash-table :test #'equal))
          (clawmacs::*package-prompt-sections* nil)
          (clawmacs::*enabled-builtin-packages* nil))
     ,@body))

(defun reset-test-netcons-state ()
  "Reset netcons process-local ref cache when the package has been loaded."
  (let ((counter (find-symbol "*NETCONS-REF-COUNTER*" :clawmacs))
        (table (find-symbol "*NETCONS-REF-TABLE*" :clawmacs)))
    (when (and counter (boundp counter))
      (set counter 0))
    (when (and table (boundp table))
      (set table (make-hash-table :test #'equal)))))

(defun load-test-netcons-package ()
  "Enable and load the bundled netcons package."
  (set-package-enablement-scope "netcons" :global)
  (load-active-packages)
  (reset-test-netcons-state))

(defun netcons-package-tool-result (tool-name args)
  "Execute TOOL-NAME with ARGS and read its Lisp data result."
  (nth-value 0
    (clawmacs::lisp-data-read
     (clawmacs:execute-tool tool-name args))))

(defmacro with-netcons-http-fixtures (&body body)
  `(with-netcons-function-override (drakma:http-request (url &rest args)
     (declare (ignore args))
     (cond
       ((search "duckduckgo.com/html/" url :test #'char-equal)
        (values *netcons-search-fixture*
                200
                '(("content-type" . "text/html; charset=utf-8"))))
       ((string= url "https://example.com/page")
        (values *netcons-page-fixture*
                200
                '(("content-type" . "text/html; charset=utf-8"))))
       (t
        (error "Unexpected netcons test URL: ~A" url))))
     ,@body))

(test netcons-package-registers-agent-tools-and-prompt
  "Enabling netcons exposes package-scoped provider tools and prompt guidance."
  (with-netcons-package-state
    (load-test-netcons-package)
    (let* ((*current-caller* :user)
           (tools (coerce (tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools)
                             #'string<))
           (prompt (render-package-prompt-sections)))
      (dolist (name '("netcons_find" "netcons_open"
                      "netcons_run" "netcons_search"))
        (is (member name tool-names :test #'string=)))
      (is (search "Web lookup with netcons" prompt))
      (is (search "netcons_run" prompt))
      (is (search "search_query" prompt))
      (is (search "Prefer these tools over `lisp_eval`" prompt)))))

(test netcons-run-search-open-find-contract
  "Grouped netcons_run supports Codex-style search, open, and find payloads."
  (with-netcons-package-state
    (load-test-netcons-package)
    (with-netcons-http-fixtures
      (let* ((search-result
               (netcons-package-tool-result
                "netcons_run"
                '(:response_length "short"
                  :search_query #(((:q . "clawmacs netcons")
                                   (:domains . #("example.com"))
                                   (:recency . 7)
                                   (:limit . 1))))))
             (searches (getf search-result :search-query))
             (search (aref searches 0))
             (results (getf search :results))
             (first-result (aref results 0))
             (ref-id (getf first-result :ref-id)))
        (is (= 1 (length searches)))
        (is (string= "clawmacs netcons" (getf search :query)))
        (is (search "site%3Aexample.com" (getf search :search-url)))
        (is (string= "Example Page" (getf first-result :title)))
        (is (string= "https://example.com/page" (getf first-result :url)))
        (is (search "Alpha" (getf first-result :snippet)))
        (is (string= "netcons1" ref-id))
        (let* ((open-result
                 (netcons-package-tool-result
                  "netcons_run"
                  `(:open #(((:ref_id . ,ref-id)
                             (:lineno . 1)
                             (:max_chars . 120))))))
               (opened (aref (getf open-result :open) 0))
               (lines (getf opened :lines)))
          (is (string= ref-id (getf opened :ref-id)))
          (is (string= "Fetched Example" (getf opened :title)))
          (is (plusp (length lines)))
          (is (some (lambda (line)
                      (search "Alpha appears" (getf line :text)))
                    (coerce lines 'list))))
        (let* ((find-result
                 (netcons-package-tool-result
                  "netcons_run"
                  `(:find #(((:ref_id . ,ref-id)
                             (:pattern . "needle"))))))
               (found (aref (getf find-result :find) 0))
               (matches (getf found :matches)))
          (is (= 1 (getf found :count)))
          (is (search "Needle appears" (getf (aref matches 0) :text))))))))

(test netcons-focused-tools-open-direct-url-and-find
  "Focused tools support direct URL fetching and text lookup."
  (with-netcons-package-state
    (load-test-netcons-package)
    (with-netcons-http-fixtures
      (let* ((open-result
               (netcons-package-tool-result
                "netcons_open"
                '(:url "https://example.com/page"
                  :lineno 2
                  :max_chars 160)))
             (opened (aref (getf open-result :open) 0))
             (lines (coerce (getf opened :lines) 'list)))
        (is (string= "https://example.com/page" (getf opened :url)))
        (is (= 2 (getf opened :lineno)))
        (is (every (lambda (line)
                     (>= (getf line :line) 2))
                   lines)))
      (let* ((find-result
               (netcons-package-tool-result
                "netcons_find"
                '(:url "https://example.com/page"
                  :ref_id ""
                  :pattern "alpha"
                  :limit 3)))
             (found (aref (getf find-result :find) 0)))
        (is (= 1 (getf found :count)))
        (is (search "Alpha appears" (getf (aref (getf found :matches) 0)
                                          :text)))))))
