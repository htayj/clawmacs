(in-package :clawmacs)

(defvar *netcons-search-url* "https://duckduckgo.com/html/"
  "Search endpoint used by netcons_search and netcons_run.")

(defvar *netcons-user-agent*
  "Clawmacs Netcons/0.1 (+https://github.com/htayj/clawmacs)"
  "User-Agent sent by netcons HTTP requests.")

(defvar *netcons-connection-timeout* 15
  "Connection timeout in seconds for netcons HTTP requests.")

(defvar *netcons-default-max-chars* 30000
  "Default maximum characters returned by a fetched page.")

(defvar *netcons-find-default-limit* 20
  "Default maximum line matches returned by netcons_find.")

(defvar *netcons-ref-counter* 0
  "Monotonic counter used to generate netcons ref ids.")

(defvar *netcons-ref-table* (make-hash-table :test #'equal)
  "Process-local map from netcons ref ids to URL/page metadata.")

(defun netcons-blank-string-p (value)
  "Return true when VALUE is NIL or only whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  value)))))

(defun netcons-string (value field-name &key allow-nil)
  "Normalize VALUE as a string argument named FIELD-NAME."
  (cond
    ((null value)
     (if allow-nil
         nil
         (error "~A is required." field-name)))
    ((stringp value)
     value)
    (t
     (error "~A must be a string, got ~S." field-name value))))

(defun netcons-positive-integer (value field-name default)
  "Return VALUE as a positive integer, or DEFAULT when VALUE is NIL."
  (cond
    ((null value) default)
    ((and (integerp value) (plusp value)) value)
    (t
     (error "~A must be a positive integer, got ~S." field-name value))))

(defun netcons-sequence-list (value field-name)
  "Return VALUE as a list, accepting JSON arrays decoded as vectors."
  (cond
    ((null value) nil)
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t
     (error "~A must be an array/list, got ~S." field-name value))))

(defun netcons-response-length (value)
  "Normalize a Codex-style RESPONSE_LENGTH value."
  (let ((text (string-downcase (netcons-string (or value "medium")
                                               "response-length"))))
    (unless (member text '("short" "medium" "long") :test #'string=)
      (error "response_length must be short, medium, or long, got ~S." value))
    text))

(defun netcons-result-limit (response-length)
  "Return default search result count for RESPONSE-LENGTH."
  (cond
    ((string= response-length "short") 5)
    ((string= response-length "long") 20)
    (t 10)))

(defun netcons-content-limit (response-length &optional max-chars)
  "Return default fetched content size for RESPONSE-LENGTH."
  (or max-chars
      (cond
        ((string= response-length "short") 12000)
        ((string= response-length "long") 60000)
        (t *netcons-default-max-chars*))))

(defun netcons-http-url-p (url)
  "Return non-nil when URL has an HTTP or HTTPS scheme."
  (and (stringp url)
       (or (alexandria:starts-with-subseq "http://" url)
           (alexandria:starts-with-subseq "https://" url))))

(defun netcons-ensure-url (url)
  "Validate URL as an HTTP(S) URL."
  (let ((value (netcons-string url "url")))
    (unless (netcons-http-url-p value)
      (error "Only http:// and https:// URLs are supported, got: ~A" value))
    value))

(defun netcons-http-get (url)
  "Fetch URL and return values BODY-STRING, STATUS-CODE, HEADERS."
  (multiple-value-bind (body status-code headers)
      (provider-http-request-with-retries
       "netcons"
       (lambda ()
         (drakma:http-request
          url
          :method :get
          :want-stream nil
          :force-binary nil
          :connection-timeout *netcons-connection-timeout*
          :user-agent *netcons-user-agent*)))
    (values (http-body-string body) status-code headers)))

(defun netcons-header-value (headers name)
  "Return HTTP header NAME from HEADERS."
  (provider-http-header-value headers name))

(defun netcons-strip-tags (text)
  "Return TEXT with simple HTML tags removed."
  (with-output-to-string (out)
    (loop :with in-tag-p := nil
          :for char :across (or text "")
          :do (cond
                ((char= char #\<) (setf in-tag-p t))
                ((char= char #\>) (setf in-tag-p nil))
                ((not in-tag-p) (write-char char out))))))

(defun netcons-clean-text (text)
  "Normalize HTML-ish TEXT for display."
  (let* ((without-tags (netcons-strip-tags text))
         (decoded (decode-html-entities without-tags)))
    (string-trim '(#\Space #\Tab #\Newline #\Return) decoded)))

(defun netcons-html-document-p (body headers)
  "Return non-nil when BODY/HEADERS look like HTML."
  (let ((content-type (netcons-header-value headers "content-type")))
    (or (and content-type
             (search "html" content-type :test #'char-equal))
        (search "<html" body :test #'char-equal)
        (search "<!doctype html" body :test #'char-equal))))

(defun netcons-body-title (body headers)
  "Return a title for BODY, if one can be found."
  (or (and (netcons-html-document-p body headers)
           (html-title body))
      ""))

(defun netcons-body-text (body headers)
  "Return agent-readable text from BODY."
  (if (netcons-html-document-p body headers)
      (html-to-text body)
      body))

(defun netcons-truncate (text max-chars)
  "Return TEXT truncated to MAX-CHARS plus truncation metadata."
  (let* ((value (or text ""))
         (length (length value)))
    (if (> length max-chars)
        (values (subseq value 0 max-chars) t length)
        (values value nil length))))

(defun netcons-split-lines (text)
  "Return TEXT split into lines."
  (split-string-by-char (or text "") #\Newline))

(defun netcons-line-window (lines lineno max-chars)
  "Return values selected line plists, truncation flag, and source line count."
  (let* ((start-line (max 1 (or lineno 1)))
         (line-count (length lines))
         (rest (nthcdr (1- start-line) lines))
         (remaining max-chars)
         (selected nil)
         (truncated-p nil))
    (loop :for line := (pop rest)
          :for number :from start-line
          :while (and line (plusp remaining))
          :do (let* ((take (min (length line) remaining))
                     (line-text (subseq line 0 take))
                     (used (+ take 1)))
                (push (list :line number :text line-text) selected)
                (decf remaining used)
                (when (< take (length line))
                  (setf truncated-p t
                        rest nil
                        remaining 0))))
    (when rest
      (setf truncated-p t))
    (values (coerce (nreverse selected) 'vector)
            truncated-p
            line-count)))

(defun netcons-next-ref-id (&optional (prefix "netcons"))
  "Return a fresh ref id."
  (format nil "~A~D" prefix (incf *netcons-ref-counter*)))

(defun netcons-store-ref (url &key title snippet status content headers)
  "Store URL metadata and return a fresh ref id."
  (let ((ref-id (netcons-next-ref-id)))
    (setf (gethash ref-id *netcons-ref-table*)
          (list :ref-id ref-id
                :url url
                :title (or title "")
                :snippet (or snippet "")
                :status status
                :content content
                :headers headers))
    ref-id))

(defun netcons-ref-entry (ref-id-or-url)
  "Return cached entry for REF-ID-OR-URL, or a transient direct-URL entry."
  (let ((value (netcons-string ref-id-or-url "ref_id")))
    (cond
      ((gethash value *netcons-ref-table*))
      ((netcons-http-url-p value)
       (let ((ref-id (netcons-store-ref value :title value)))
         (gethash ref-id *netcons-ref-table*)))
      (t
       (error "Unknown netcons ref_id ~S. Use a returned ref_id or direct URL."
              value)))))

(defun netcons-ref-id-or-url (spec)
  "Return SPEC's nonblank ref_id, or its direct URL when ref_id is blank."
  (let ((ref-id (netcons-string (tool-arg spec :ref-id "ref_id")
                                "ref_id"
                                :allow-nil t))
        (url (netcons-string (tool-arg spec :url "url")
                             "url"
                             :allow-nil t)))
    (cond
      ((not (netcons-blank-string-p ref-id)) ref-id)
      ((not (netcons-blank-string-p url)) url)
      (t
       (error "ref_id or url is required.")))))

(defun netcons-entry-ref-id (entry)
  "Return ENTRY's ref id."
  (getf entry :ref-id))

(defun netcons-extract-attribute (tag attribute)
  "Extract ATTRIBUTE value from one HTML TAG string."
  (let* ((needle (format nil "~A=" attribute))
         (start (search needle tag :test #'char-equal)))
    (when start
      (let* ((value-start (+ start (length needle)))
             (quote (and (< value-start (length tag))
                         (char tag value-start))))
        (cond
          ((member quote '(#\" #\') :test #'char=)
           (let ((end (position quote tag :start (1+ value-start))))
             (and end (subseq tag (1+ value-start) end))))
          (t
           (let ((end (or (position #\Space tag :start value-start)
                          (length tag))))
             (subseq tag value-start end))))))))

(defun netcons-tag-start (html position)
  "Return the '<' position for the tag containing POSITION."
  (loop :for index :downfrom position :to 0
        :when (char= (char html index) #\<)
          :return index))

(defun netcons-tag-end (html position)
  "Return the '>' position for the tag containing POSITION."
  (position #\> html :start position))

(defun netcons-next-class-position (html class-name start)
  "Return position of CLASS-NAME in HTML at or after START."
  (search class-name html :start2 start :test #'char-equal))

(defun netcons-extract-element-text-after-class (html class-name start end-limit)
  "Extract text from first element whose class contains CLASS-NAME."
  (let ((class-pos (netcons-next-class-position html class-name start)))
    (when (and class-pos (< class-pos end-limit))
      (let* ((tag-start (netcons-tag-start html class-pos))
             (tag-end (and tag-start (netcons-tag-end html tag-start)))
             (close (and tag-end (search "</" html :start2 tag-end
                                         :end2 end-limit))))
        (and tag-end close
             (netcons-clean-text (subseq html (1+ tag-end) close)))))))

(defun netcons-search-result-url (href)
  "Normalize a search result HREF."
  (let* ((decoded (decode-html-entities (or href "")))
         (uddg-pos (search "uddg=" decoded :test #'char-equal)))
    (cond
      (uddg-pos
       (let* ((value-start (+ uddg-pos (length "uddg=")))
              (value-end (or (position #\& decoded :start value-start)
                             (length decoded))))
         (url-decode-param (subseq decoded value-start value-end))))
      ((alexandria:starts-with-subseq "//" decoded)
       (concatenate 'string "https:" decoded))
      (t decoded))))

(defun netcons-extract-search-results (html limit)
  "Extract DuckDuckGo HTML search results from HTML."
  (let ((results nil)
        (start 0))
    (loop :while (< (length results) limit)
          :for class-pos := (netcons-next-class-position html "result__a" start)
          :while class-pos
          :for tag-start := (netcons-tag-start html class-pos)
          :for tag-end := (and tag-start (netcons-tag-end html tag-start))
          :while tag-end
          :for next-class := (or (netcons-next-class-position
                                  html "result__a" (1+ tag-end))
                                 (length html))
          :for tag := (subseq html tag-start (1+ tag-end))
          :for href := (netcons-extract-attribute tag "href")
          :for anchor-close := (search "</a>" html :start2 (1+ tag-end)
                                       :end2 next-class
                                       :test #'char-equal)
          :for title := (and anchor-close
                             (netcons-clean-text
                              (subseq html (1+ tag-end) anchor-close)))
          :for snippet := (netcons-extract-element-text-after-class
                           html "result__snippet" (1+ tag-end) next-class)
          :do (setf start next-class)
              (let ((url (and href (netcons-search-result-url href))))
                (when (and url
                           (netcons-http-url-p url)
                           (not (netcons-blank-string-p title)))
                  (let ((ref-id (netcons-store-ref url
                                                   :title title
                                                   :snippet snippet)))
                    (push (list :ref-id ref-id
                                :title title
                                :url url
                                :snippet (or snippet ""))
                          results)))))
    (coerce (nreverse results) 'vector)))

(defun netcons-domains-query-suffix (domains)
  "Return a search-engine query suffix for domain filters."
  (let ((values
          (remove-if #'netcons-blank-string-p
                     (mapcar (lambda (domain)
                               (netcons-string domain "domain"))
                             (netcons-sequence-list domains "domains")))))
    (cond
      ((null values) "")
      ((null (rest values))
       (format nil " site:~A" (first values)))
      (t
       (format nil " (~{site:~A~^ OR ~})" values)))))

(defun netcons-recency-param (recency)
  "Return a DuckDuckGo df parameter approximating RECENCY days."
  (cond
    ((null recency) nil)
    ((not (and (integerp recency) (plusp recency)))
     (error "recency must be a positive integer number of days, got ~S."
            recency))
    ((<= recency 1) "d")
    ((<= recency 7) "w")
    ((<= recency 31) "m")
    (t "y")))

(defun netcons-search-url (query recency domains)
  "Return the DuckDuckGo HTML URL for QUERY."
  (let* ((effective-query
           (concatenate 'string
                        query
                        (netcons-domains-query-suffix domains)))
         (df (netcons-recency-param recency)))
    (format nil "~A?q=~A~@[&df=~A~]"
            *netcons-search-url*
            (url-encode-param effective-query)
            df)))

(defun netcons-search-one (spec response-length)
  "Run one Codex-style search_query SPEC."
  (let* ((query (netcons-string (tool-arg spec :q "q") "q"))
         (recency (tool-arg spec :recency "recency"))
         (domains (tool-arg spec :domains "domains"))
         (limit (netcons-positive-integer
                 (tool-arg spec :limit "limit")
                 "limit"
                 (netcons-result-limit response-length)))
         (url (netcons-search-url query recency domains)))
    (when (netcons-blank-string-p query)
      (error "q must not be blank."))
    (multiple-value-bind (body status-code headers)
        (netcons-http-get url)
      (declare (ignore headers))
      (list :query query
            :status status-code
            :search-url url
            :results (netcons-extract-search-results body limit)))))

(defun netcons-open-entry (entry lineno response-length &key max-chars)
  "Fetch ENTRY and return a Codex-style open result."
  (let ((url (getf entry :url)))
    (multiple-value-bind (body status-code headers)
        (netcons-http-get url)
      (let* ((text (netcons-body-text body headers))
             (page-title (netcons-body-title body headers))
             (cached-title (getf entry :title))
             (title (or (and (not (netcons-blank-string-p page-title))
                             page-title)
                        (and (not (netcons-blank-string-p cached-title))
                             cached-title)
                        ""))
             (limit (netcons-content-limit response-length max-chars))
             (lines (netcons-split-lines text)))
        (setf (getf entry :status) status-code
              (getf entry :headers) headers
              (getf entry :content) text
              (getf entry :title) title)
        (multiple-value-bind (window truncated-p line-count)
            (netcons-line-window lines lineno limit)
          (list :ref-id (netcons-entry-ref-id entry)
                :url url
                :status status-code
                :title title
                :lineno (max 1 (or lineno 1))
                :line-count line-count
                :truncated truncated-p
                :lines window))))))

(defun netcons-open-one (spec response-length)
  "Run one Codex-style open SPEC."
  (let* ((ref-id (netcons-ref-id-or-url spec))
         (lineno (netcons-positive-integer
                  (tool-arg spec :lineno "lineno")
                  "lineno"
                  1))
         (max-chars (tool-arg spec :max-chars "max_chars"))
         (entry (netcons-ref-entry ref-id)))
    (netcons-open-entry entry lineno response-length
                        :max-chars (and max-chars
                                        (netcons-positive-integer
                                         max-chars
                                         "max_chars"
                                         nil)))))

(defun netcons-ensure-entry-content (entry response-length)
  "Ensure ENTRY has fetched content."
  (unless (getf entry :content)
    (netcons-open-entry entry 1 response-length))
  entry)

(defun netcons-find-one (spec response-length)
  "Run one Codex-style find SPEC."
  (let* ((ref-id (netcons-ref-id-or-url spec))
         (pattern (netcons-string (tool-arg spec :pattern "pattern")
                                  "pattern"))
         (limit (netcons-positive-integer
                 (tool-arg spec :limit "limit")
                 "limit"
                 *netcons-find-default-limit*))
         (entry (netcons-ensure-entry-content
                 (netcons-ref-entry ref-id)
                 response-length))
         (needle (string-downcase pattern))
         (matches nil))
    (when (netcons-blank-string-p pattern)
      (error "pattern must not be blank."))
    (loop :for line :in (netcons-split-lines (getf entry :content))
          :for number :from 1
          :while (< (length matches) limit)
          :when (search needle (string-downcase line))
            :do (push (list :line number :text line) matches))
    (list :ref-id (netcons-entry-ref-id entry)
          :url (getf entry :url)
          :pattern pattern
          :count (length matches)
          :matches (coerce (nreverse matches) 'vector))))

(defun netcons-run-data (args)
  "Execute Codex-like web.run ARGS and return Lisp data."
  (let* ((response-length
           (netcons-response-length (tool-arg args :response-length
                                              "response_length")))
         (search-specs (netcons-sequence-list
                        (tool-arg args :search-query "search_query")
                        "search_query"))
         (open-specs (netcons-sequence-list (tool-arg args :open "open")
                                            "open"))
         (find-specs (netcons-sequence-list (tool-arg args :find "find")
                                            "find"))
         (result nil))
    (when search-specs
      (push :search-query result)
      (push (coerce (mapcar (lambda (spec)
                              (netcons-search-one spec response-length))
                            search-specs)
                    'vector)
            result))
    (when open-specs
      (push :open result)
      (push (coerce (mapcar (lambda (spec)
                              (netcons-open-one spec response-length))
                            open-specs)
                    'vector)
            result))
    (when find-specs
      (push :find result)
      (push (coerce (mapcar (lambda (spec)
                              (netcons-find-one spec response-length))
                            find-specs)
                    'vector)
            result))
    (unless result
      (error "Provide at least one of search_query, open, or find."))
    (nreverse result)))

(defun netcons-tool-run (args)
  "Run Codex-like web search/open/find operations."
  (netcons-run-data args))

(defun netcons-tool-search (args)
  "Search the web with a single query."
  (let ((query (netcons-string (tool-arg args :q "q") "q")))
    (netcons-run-data
     `(:response-length ,(or (tool-arg args :response-length
                                      "response_length")
                             "medium")
       :search-query (((:q . ,query)
                       (:recency . ,(tool-arg args :recency "recency"))
                       (:domains . ,(tool-arg args :domains "domains"))
                       (:limit . ,(tool-arg args :limit "limit"))))))))

(defun netcons-tool-open (args)
  "Open one URL or netcons ref id."
  (netcons-run-data
     `(:response-length ,(or (tool-arg args :response-length
                                    "response_length")
                           "medium")
     :open (((:ref-id . ,(tool-arg args :ref-id "ref_id"))
             (:url . ,(tool-arg args :url "url"))
             (:lineno . ,(tool-arg args :lineno "lineno"))
             (:max-chars . ,(tool-arg args :max-chars "max_chars")))))))

(defun netcons-tool-find (args)
  "Find text in an opened URL or netcons ref id."
  (netcons-run-data
     `(:response-length ,(or (tool-arg args :response-length
                                    "response_length")
                           "medium")
     :find (((:ref-id . ,(tool-arg args :ref-id "ref_id"))
             (:url . ,(tool-arg args :url "url"))
             (:pattern . ,(tool-arg args :pattern "pattern"))
             (:limit . ,(tool-arg args :limit "limit")))))))

(register-package-prompt-section
 "netcons"
 "## Web lookup with netcons

- Use `netcons_run` for Codex-style web lookup. It accepts the familiar
  `search_query`, `open`, `find`, and `response_length` fields. Use returned
  `ref-id` values with later `open` and `find` calls.
- Use `netcons_search`, `netcons_open`, and `netcons_find` when a single
  focused operation is clearer than the grouped `netcons_run` contract.
- Search results are live HTTP results from DuckDuckGo HTML search. Domain
  filters are translated into `site:` query terms; recency is approximated with
  the search engine's day/week/month/year filter.
- Opened pages return line-numbered text windows. Use `lineno` to continue
  reading large pages and `netcons_find` to locate terms inside fetched page
  text.
- Prefer these tools over `lisp_eval` for web research and cite the returned
  URLs in final answers."
 :title "Web lookup with netcons"
 :package "netcons")

(deftool netcons-tool-run
  :name "netcons_run"
  :description "Run Codex-style web search/open/find operations. Input mirrors web.run: search_query, open, find, and response_length."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((search-query :type "array" :required nil
                       :items ((:type . "object"))
                       :description "Codex-style search_query array. Each item has q and optional recency, domains, and limit.")
         (open :type "array" :required nil
               :items ((:type . "object"))
               :description "Codex-style open array. Each item has ref_id, or direct URL as ref_id/url, and optional lineno/max_chars.")
         (find :type "array" :required nil
               :items ((:type . "object"))
               :description "Codex-style find array. Each item has ref_id, or direct URL as ref_id/url, pattern, and optional limit.")
         (response-length :type "string" :required nil
                          :description "One of short, medium, or long.")))

(deftool netcons-tool-search
  :name "netcons_search"
  :description "Search the web and return URL/title/snippet results with netcons ref ids."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((q :type "string"
            :description "Search query.")
         (recency :type "integer" :required nil
                  :description "Optional recency filter in days.")
         (domains :type "array" :required nil
                  :items ((:type . "string"))
                  :description "Optional domain filters, equivalent to Codex web domains.")
         (limit :type "integer" :required nil
                :description "Maximum result count.")
         (response-length :type "string" :required nil
                          :description "One of short, medium, or long.")))

(deftool netcons-tool-open
  :name "netcons_open"
  :description "Fetch a URL or netcons ref id and return line-numbered page text."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((ref-id :type "string" :required nil
                 :description "Ref id returned by netcons_search/netcons_run.")
         (url :type "string" :required nil
              :description "Direct http:// or https:// URL to open when ref-id is omitted.")
         (lineno :type "integer" :required nil
                 :description "1-indexed line number to start reading from.")
         (max-chars :type "integer" :required nil
                    :description "Maximum characters of page text to return.")
         (response-length :type "string" :required nil
                          :description "One of short, medium, or long.")))

(deftool netcons-tool-find
  :name "netcons_find"
  :description "Find text in a fetched page by netcons ref id or direct URL."
  :permission :agent-allowed
  :call-style :raw-args
  :args ((ref-id :type "string" :required nil
                 :description "Ref id returned by netcons_search/netcons_run.")
         (url :type "string" :required nil
              :description "Direct http:// or https:// URL to fetch and search when ref-id is omitted.")
         (pattern :type "string"
                  :description "Case-insensitive text to find.")
         (limit :type "integer" :required nil
                :description "Maximum match count.")
         (response-length :type "string" :required nil
                          :description "One of short, medium, or long.")))
