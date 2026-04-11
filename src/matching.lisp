(in-package :clawmacs)

;;; --------------------------------------------------------------------------
;;; Minibuffer Matching
;;; --------------------------------------------------------------------------

(defun matching-core-available-p ()
  "Return T when the Coalton minibuffer matching core is loaded."
  (and (find-package '#:clawmacs/matching-core)
       (fboundp 'clawmacs/matching-core:fuzzy-match-p)))

(declaim (ftype (function (string) list) split-query-tokens))
(defun split-query-tokens (query)
  "Split QUERY by spaces and return a list of non-empty token strings.
Used for orderless-style matching where each space-separated word must
independently match the candidate via subsequence search."
  (clawmacs/matching-core:split-query-tokens query))

(declaim (ftype (function (string string) boolean) fuzzy-token-match-p))
(defun fuzzy-token-match-p (token candidate)
  "Return T if all characters in TOKEN appear in CANDIDATE in order (case-insensitive).
Both TOKEN and CANDIDATE should already be downcased."
  (clawmacs/matching-core:fuzzy-token-match-p token candidate))

(declaim (ftype (function (string string) boolean) fuzzy-match-p))
(defun fuzzy-match-p (query candidate)
  "Return T if QUERY matches CANDIDATE (case-insensitive).
Supports space-separated tokens (orderless-style): the query is split on
spaces and every token must independently match CANDIDATE via subsequence
search.  An empty query (or all-spaces query) matches everything."
  (clawmacs/matching-core:fuzzy-match-p query candidate))

(declaim (ftype (function (string string) list) fuzzy-token-positions))
(defun fuzzy-token-positions (token candidate)
  "Return a list of character positions in CANDIDATE matched by TOKEN using
greedy left-to-right subsequence matching.  TOKEN and CANDIDATE must already
be downcased.  Returns NIL if TOKEN does not match CANDIDATE."
  (clawmacs/matching-core:fuzzy-token-positions token candidate))

(declaim (ftype (function (string string) list) fuzzy-match-positions))
(defun fuzzy-match-positions (query candidate)
  "Return a sorted list of character positions in CANDIDATE matched by QUERY.
Handles space-separated tokens (orderless-style): combines matched positions
from every token.  Returns NIL if any token fails to match or query is empty."
  (clawmacs/matching-core:fuzzy-match-positions query candidate))

(declaim (ftype (function (string string) (or null integer)) fuzzy-token-score))
(defun fuzzy-token-score (token candidate)
  "Return a relevance score for TOKEN matching CANDIDATE (both already downcased).
Returns NIL when TOKEN does not match.  Higher scores mean better matches."
  (let ((score (clawmacs/matching-core:fuzzy-token-score-or-negative-one
                token candidate)))
    (unless (minusp score)
      score)))

(declaim (ftype (function (string string) (or null integer)) fuzzy-score))
(defun fuzzy-score (query candidate)
  "Return a relevance score for QUERY matching CANDIDATE (case-insensitive).
Handles space-separated tokens (orderless-style): scores each token
independently and sums them.  Returns 0 for an empty/blank query, or NIL
if any token fails to match."
  (let ((score (clawmacs/matching-core:fuzzy-score-or-negative-one
                query candidate)))
    (unless (minusp score)
      score)))
