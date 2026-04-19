(in-package #:clawmacs/matching-core)

;;; --------------------------------------------------------------------------
;;; Fuzzy Matching Core
;;; --------------------------------------------------------------------------

(defun split-query-tokens (query)
  "Split QUERY into non-empty space-separated matching tokens."
  (remove "" (uiop:split-string query :separator '(#\Space))
          :test #'string=))

(defun blank-query-p (query)
  "Return true when QUERY has no non-empty matching tokens."
  (null (split-query-tokens query)))

(defun position-from (character string start)
  "Return CHARACTER's position in STRING at or after START, or string length."
  (or (position character string :start start :test #'char=)
      (length string)))

(defun fuzzy-token-positions (token candidate)
  "Return greedy subsequence match positions for TOKEN in CANDIDATE."
  (let ((positions nil)
        (candidate-start 0)
        (candidate-length (length candidate)))
    (loop :for character :across token
          :for match-position := (position-from character candidate candidate-start)
          :do (when (= match-position candidate-length)
                (return-from fuzzy-token-positions nil))
              (push match-position positions)
              (setf candidate-start (1+ match-position)))
    (nreverse positions)))

(defun fuzzy-token-match-p (token candidate)
  "Return true when TOKEN is a subsequence of CANDIDATE."
  (or (zerop (length token))
      (not (null (fuzzy-token-positions token candidate)))))

(defun fuzzy-match-p (query candidate)
  "Return true when every token in QUERY fuzzy-matches CANDIDATE."
  (let ((tokens (split-query-tokens (string-downcase query)))
        (normalized-candidate (string-downcase candidate)))
    (every (lambda (token)
             (fuzzy-token-match-p token normalized-candidate))
           tokens)))

(defun fuzzy-match-positions (query candidate)
  "Return sorted unique CANDIDATE positions matched by QUERY."
  (if (blank-query-p query)
      nil
      (let ((normalized-candidate (string-downcase candidate))
            (positions nil))
        (dolist (token (split-query-tokens (string-downcase query)))
          (let ((token-positions
                  (fuzzy-token-positions token normalized-candidate)))
            (when (null token-positions)
              (return-from fuzzy-match-positions nil))
            (setf positions (append token-positions positions))))
        (sort (remove-duplicates positions) #'<))))

(defun string-prefix-p (prefix string)
  "Return true when PREFIX is a prefix of STRING."
  (let ((prefix-length (length prefix)))
    (and (<= prefix-length (length string))
         (string= prefix string :end2 prefix-length))))

(defun word-boundary-character-p (character)
  "Return true when CHARACTER separates words for matching score purposes."
  (member character '(#\- #\_ #\/ #\. #\Space) :test #'char=))

(defun word-boundary-position-p (candidate position)
  "Return true when POSITION is at the start of a word in CANDIDATE."
  (or (zerop position)
      (word-boundary-character-p (char candidate (1- position)))))

(defun consecutive-position-bonus (positions)
  "Return score bonus for consecutive matched positions."
  (loop :for previous :in positions
        :for position :in (rest positions)
        :sum (if (= position (1+ previous)) 5 0)))

(defun word-boundary-bonus (candidate positions)
  "Return score bonus for positions that start words in CANDIDATE."
  (loop :for position :in positions
        :sum (if (word-boundary-position-p candidate position) 8 0)))

(defun fuzzy-token-score-or-negative-one (token candidate)
  "Score TOKEN against CANDIDATE, or return -1 when it does not match."
  (let ((positions (fuzzy-token-positions token candidate)))
    (if (null positions)
        -1
        (let* ((token-length (length token))
               (candidate-length (length candidate))
               (first-position (first positions))
               (score (+ 1
                         (if (string= token candidate) 100 0)
                         (if (and (<= token-length candidate-length)
                                  (string-prefix-p token candidate))
                             50
                             0)
                         (if (search token candidate :test #'char=) 30 0)
                         (max 0 (- 20 first-position))
                         (consecutive-position-bonus positions)
                         (word-boundary-bonus candidate positions))))
          score))))

(defun fuzzy-score-or-negative-one (query candidate)
  "Score QUERY against CANDIDATE, or return -1 when any token fails."
  (let ((tokens (split-query-tokens (string-downcase query)))
        (normalized-candidate (string-downcase candidate))
        (total 0))
    (dolist (token tokens total)
      (let ((token-score
              (fuzzy-token-score-or-negative-one token normalized-candidate)))
        (when (minusp token-score)
          (return-from fuzzy-score-or-negative-one -1))
        (incf total token-score)))))
