(in-package #:clawmacs/matching-core)

(named-readtables:in-readtable coalton:coalton)

;;; --------------------------------------------------------------------------
;;; Typed Fuzzy Matching Core
;;; --------------------------------------------------------------------------

(coalton-toplevel
  (declare split-query-tokens (String -> (List String)))
  (define (split-query-tokens query)
    "Split QUERY into non-empty space-separated matching tokens."
    (lisp (List String) (query)
      (cl:remove "" (uiop:split-string query :separator '(#\Space))
                 :test #'cl:string=)))

  (declare position-from (Char -> String -> UFix -> UFix))
  (define (position-from character string start)
    "Return CHARACTER's position in STRING at or after START, or string length."
    (lisp UFix (character string start)
      (cl:or (cl:position character string :start start :test #'cl:char=)
             (cl:length string))))

  (declare fuzzy-token-positions (String -> String -> (List UFix)))
  (define (fuzzy-token-positions token candidate)
    "Return greedy subsequence match positions for TOKEN in CANDIDATE."
    (lisp (List UFix) (token candidate)
      (cl:let ((positions cl:nil)
               (candidate-start 0)
               (candidate-length (cl:length candidate)))
        (cl:loop :for character :across token
                 :for match-position := (cl:or (cl:position character candidate
                                                            :start candidate-start
                                                            :test #'cl:char=)
                                               candidate-length)
                 :do (cl:when (cl:= match-position candidate-length)
                       (cl:return-from fuzzy-token-positions cl:nil))
                     (cl:push match-position positions)
                     (cl:setf candidate-start (cl:1+ match-position)))
        (cl:nreverse positions))))

  (declare fuzzy-token-match-p (String -> String -> Boolean))
  (define (fuzzy-token-match-p token candidate)
    "Return true when TOKEN is a subsequence of CANDIDATE."
    (lisp Boolean (token candidate)
      (to-boolean
       (cl:or (cl:zerop (cl:length token))
              (cl:not (cl:null (fuzzy-token-positions token candidate)))))))

  (declare fuzzy-match-p (String -> String -> Boolean))
  (define (fuzzy-match-p query candidate)
    "Return true when every token in QUERY fuzzy-matches CANDIDATE."
    (lisp Boolean (query candidate)
      (cl:let ((tokens (split-query-tokens (cl:string-downcase query)))
               (normalized-candidate (cl:string-downcase candidate)))
        (to-boolean
         (cl:every (cl:lambda (token)
                     (fuzzy-token-match-p token normalized-candidate))
                   tokens)))))

  (declare fuzzy-match-positions (String -> String -> (List UFix)))
  (define (fuzzy-match-positions query candidate)
    "Return sorted unique CANDIDATE positions matched by QUERY."
    (lisp (List UFix) (query candidate)
      (cl:let ((tokens (split-query-tokens (cl:string-downcase query))))
        (cl:if (cl:null tokens)
               cl:nil
               (cl:let ((normalized-candidate (cl:string-downcase candidate))
                        (positions cl:nil))
                 (cl:dolist (token tokens)
                   (cl:let ((token-positions
                              (fuzzy-token-positions token normalized-candidate)))
                     (cl:when (cl:null token-positions)
                       (cl:return-from fuzzy-match-positions cl:nil))
                     (cl:setf positions (cl:append token-positions positions))))
                 (cl:sort (cl:remove-duplicates positions) #'cl:<))))))

  (declare string-prefix-p (String -> String -> Boolean))
  (define (string-prefix-p prefix string)
    "Return true when PREFIX is a prefix of STRING."
    (lisp Boolean (prefix string)
      (cl:let ((prefix-length (cl:length prefix)))
        (to-boolean
         (cl:and (cl:<= prefix-length (cl:length string))
                 (cl:string= prefix string :end2 prefix-length))))))

  (declare word-boundary-character-p (Char -> Boolean))
  (define (word-boundary-character-p character)
    "Return true when CHARACTER separates words for matching score purposes."
    (lisp Boolean (character)
      (to-boolean
       (cl:member character '(#\- #\_ #\/ #\. #\Space) :test #'cl:char=))))

  (declare word-boundary-position-p (String -> UFix -> Boolean))
  (define (word-boundary-position-p candidate position)
    "Return true when POSITION is at the start of a word in CANDIDATE."
    (lisp Boolean (candidate position)
      (to-boolean
       (cl:or (cl:zerop position)
              (word-boundary-character-p (cl:char candidate (cl:1- position)))))))

  (declare consecutive-position-bonus ((List UFix) -> Integer))
  (define (consecutive-position-bonus positions)
    "Return score bonus for consecutive matched positions."
    (lisp Integer (positions)
      (cl:loop :for previous :in positions
               :for position :in (cl:rest positions)
               :sum (cl:if (cl:= position (cl:1+ previous)) 5 0))))

  (declare word-boundary-bonus (String -> (List UFix) -> Integer))
  (define (word-boundary-bonus candidate positions)
    "Return score bonus for positions that start words in CANDIDATE."
    (lisp Integer (candidate positions)
      (cl:loop :for position :in positions
               :sum (cl:if (word-boundary-position-p candidate position) 8 0))))

  (declare fuzzy-token-score-or-negative-one (String -> String -> Integer))
  (define (fuzzy-token-score-or-negative-one token candidate)
    "Score TOKEN against CANDIDATE, or return -1 when it does not match."
    (lisp Integer (token candidate)
      (cl:let ((positions (fuzzy-token-positions token candidate)))
        (cl:if (cl:null positions)
               -1
               (cl:let* ((token-length (cl:length token))
                         (candidate-length (cl:length candidate))
                         (first-position (cl:first positions)))
                 (cl:+ 1
                       (cl:if (cl:string= token candidate) 100 0)
                       (cl:if (cl:and (cl:<= token-length candidate-length)
                                      (string-prefix-p token candidate))
                              50
                              0)
                       (cl:if (cl:search token candidate :test #'cl:char=) 30 0)
                       (cl:max 0 (cl:- 20 first-position))
                       (consecutive-position-bonus positions)
                       (word-boundary-bonus candidate positions)))))))

  (declare fuzzy-score-or-negative-one (String -> String -> Integer))
  (define (fuzzy-score-or-negative-one query candidate)
    "Score QUERY against CANDIDATE, or return -1 when any token fails."
    (lisp Integer (query candidate)
      (cl:let ((tokens (split-query-tokens (cl:string-downcase query)))
               (normalized-candidate (cl:string-downcase candidate))
               (total 0))
        (cl:dolist (token tokens total)
          (cl:let ((token-score
                     (fuzzy-token-score-or-negative-one token normalized-candidate)))
            (cl:when (cl:minusp token-score)
              (cl:return-from fuzzy-score-or-negative-one -1))
            (cl:incf total token-score)))))))
