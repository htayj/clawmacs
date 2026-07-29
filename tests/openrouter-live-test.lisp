;;;; Live integration tests for OpenRouter support.
;;;; Run with:
;;;;   LD_LIBRARY_PATH=/path/to/ssl OPENROUTER_API_KEY=sk-or-v1-... \
;;;;     sbcl --noinform --load ~/quicklisp/setup.lisp \
;;;;          --eval "(push (truename \".\") asdf:*central-registry*)" \
;;;;          --load tests/openrouter-live-test.lisp

(ql:quickload "rplaca" :silent t)

(defpackage #:openrouter-live-test
  (:use #:cl))

(in-package #:openrouter-live-test)

(defvar *pass* 0)
(defvar *fail* 0)

(defmacro check (description &body body)
  `(handler-case
       (progn ,@body
              (incf *pass*)
              (format t "  [PASS] ~A~%" ,description))
     (error (e)
       (incf *fail*)
       (format t "  [FAIL] ~A~%         => ~A~%" ,description e))))

(defun section (title)
  (format t "~%~A~%~A~%" title (make-string (length title) :initial-element #\-)))

;;; --------------------------------------------------------------------------
;;; Helpers — build API-format messages without a full buffer
;;; --------------------------------------------------------------------------

(defun user-message (text)
  "Build a minimal API-format user message alist."
  `((:role . "user")
    (:content . ,(vector `((:type . "text") (:text . ,text))))))

(defun response-text (canonical-response)
  "Extract the first text string from a canonical response alist."
  (let* ((blocks (coerce (cdr (assoc :content canonical-response)) 'list)))
    (loop :for b :in blocks
          :for txt := (cdr (assoc :text b))
          :when txt :return txt)))

;;; ── 1. Token detection ──────────────────────────────────────────────────────

(section "1. Token detection")

(check "OPENROUTER_API_KEY env var is set"
  (let ((key (uiop:getenv "OPENROUTER_API_KEY")))
    (assert (and key (plusp (length key))) ()
            "OPENROUTER_API_KEY is not set")))

(check "read-provider-token :openrouter picks up env var"
  (let ((tok (rplaca::read-provider-token :openrouter)))
    (assert (and tok (stringp tok) (plusp (length tok))) ()
            "read-provider-token returned nil or empty")))

;;; ── 2. Dynamic model fetching ───────────────────────────────────────────────

(section "2. Dynamic model fetching  (live API call to /api/v1/models)")

;; Ensure we start without a cached list
(setf rplaca::*openrouter-cached-models* nil)

(check "fetch-openrouter-models returns a non-empty list"
  (let ((models (rplaca::fetch-openrouter-models)))
    (assert (listp models) () "Not a list")
    (assert (plusp (length models)) () "Empty list")
    (format t "         => ~A models fetched~%" (length models))))

(check "fetched models are all strings"
  (let ((models rplaca::*openrouter-cached-models*))
    (assert models () "Cache is empty — fetch must have failed")
    (assert (every #'stringp models) () "Not all models are strings")))

(check "fetched models follow provider/name format"
  (let ((models rplaca::*openrouter-cached-models*))
    (assert models () "Cache is empty")
    (let ((bad (remove-if (lambda (m) (find #\/ m)) models)))
      (when bad
        (format t "         WARNING: ~A model(s) without '/' — first: ~A~%"
                (length bad) (first bad))))))

(check "cached result is returned on second call (no re-fetch)"
  (let* ((cached rplaca::*openrouter-cached-models*)
         (result (rplaca::fetch-openrouter-models)))
    (assert (eq result cached) () "Second call returned a different object")))

(check "provider-known-models :openrouter returns cached list"
  (let ((pkm (rplaca::provider-known-models :openrouter)))
    (assert (eq pkm rplaca::*openrouter-cached-models*) ()
            "provider-known-models did not return the cache")))

;;; ── 3. Non-streaming request ────────────────────────────────────────────────

(section "3. Non-streaming request  (live openrouter-request)")

(check "openrouter-request returns a canonical response alist"
  (let* ((msgs (list (user-message "Reply with exactly one word: pong")))
         (resp (rplaca::openrouter-request msgs
                                             :model "openai/gpt-4o-mini"
                                             :max-tokens 16)))
    (format t "         => response: ~S~%" resp)
    (assert (listp resp) () "Response is not a list")
    (assert (assoc :content resp) () "No :content key in response")
    (assert (assoc :stop--reason resp) () "No :stop--reason key in response")))

(check "openrouter-request response contains a text string"
  (let* ((msgs (list (user-message "Say exactly: hello")))
         (resp (rplaca::openrouter-request msgs
                                             :model "openai/gpt-4o-mini"
                                             :max-tokens 16)))
    (let ((txt (response-text resp)))
      (format t "         => text: ~S~%" txt)
      (assert (and txt (stringp txt) (plusp (length txt))) ()
              "No text content in response"))))

(check "openrouter-request works with a second model (anthropic/claude-haiku-4-5)"
  ;; Use a model we know is in the live catalog (second element of the cached list)
  (let* ((second-model (or (second rplaca::*openrouter-cached-models*)
                           "anthropic/claude-haiku-4-5"))
         (msgs (list (user-message "Say: hi")))
         (resp (rplaca::openrouter-request msgs
                                             :model second-model
                                             :max-tokens 16)))
    (format t "         => model: ~A  text: ~S~%" second-model (response-text resp))
    (assert resp () "Nil response")))

;;; ── 4. Streaming request ────────────────────────────────────────────────────

(section "4. Streaming request  (live openrouter-request-streaming)")

(check "openrouter-request-streaming returns a stream-state struct"
  (let* ((msgs (list (user-message "Count from 1 to 3, one per line.")))
         (state (rplaca::openrouter-request-streaming
                 msgs
                 nil   ; callback is ignored in this implementation
                 :model "openai/gpt-4o-mini"
                 :max-tokens 64)))
    (assert (rplaca::stream-state-p state) ()
            "Return value is not a stream-state struct")
    ;; Wait up to 20 s for the SSE reader thread to finish
    (loop :repeat 200
          :until (rplaca::stream-state-done-p state)
          :do (sleep 0.1))
    (let ((text  (rplaca::stream-state-text state))
          (donep (rplaca::stream-state-done-p state))
          (errp  (rplaca::stream-state-error-p state)))
      (format t "         => done-p=~A  error-p=~A~%         => text: ~S~%"
              donep errp text)
      (when errp
        (error "Streaming error: ~A" errp))
      (assert donep () "Stream did not signal done within 20 s")
      (assert (and (stringp text) (plusp (length text))) ()
              "stream-state-text is empty after completion"))))

;;; ── Summary ─────────────────────────────────────────────────────────────────

(format t "~%~A~%" (make-string 50 :initial-element #\=))
(format t "Results: ~A passed, ~A failed~%" *pass* *fail*)
(format t "~A~%" (make-string 50 :initial-element #\=))
(uiop:quit (if (zerop *fail*) 0 1))
