(cl:in-package #:clawmacs/matching-core)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare split-query-tokens (String -> (List String)))
  (define (split-query-tokens query)
    "Split QUERY into non-empty space-separated matching tokens."
    (list:filter (fn (token) (/= 0 (str:length token)))
                 (str:split #\Space query)))

  (declare blank-query? (String -> Boolean))
  (define (blank-query? query)
    (list:null? (split-query-tokens query)))

  (declare position-from (Char -> String -> UFix -> UFix))
  (define (position-from character string start)
    "Return CHARACTER's position in STRING at or after START, or string length."
    (lisp UFix (character string start)
      (cl:let ((position (cl:position character string :start start)))
        (cl:if position position (cl:length string)))))

  (declare fuzzy-token-positions (String -> String -> (List UFix)))
  (define (fuzzy-token-positions token candidate)
    "Return greedy subsequence match positions for TOKEN in CANDIDATE."
    (let ((token-length (str:length token)))
      (rec scan ((token-index (the UFix 0))
                 (candidate-start (the UFix 0))
                 (positions Nil))
        (if (== token-index token-length)
            (list:reverse positions)
            (let ((candidate-length (str:length candidate))
                  (match-position
                    (position-from (str:ref-unchecked token token-index)
                                   candidate
                                   candidate-start)))
              (if (== match-position candidate-length)
                  Nil
                  (scan (+ token-index 1)
                        (+ match-position 1)
                        (Cons match-position positions))))))))

  (declare fuzzy-token-match-p (String -> String -> Boolean))
  (define (fuzzy-token-match-p token candidate)
    "Return TRUE when TOKEN is a subsequence of CANDIDATE."
    (if (== 0 (str:length token))
        True
        (not (list:null? (fuzzy-token-positions token candidate)))))

  (declare fuzzy-match-p (String -> String -> Boolean))
  (define (fuzzy-match-p query candidate)
    "Return TRUE when every token in QUERY fuzzy-matches CANDIDATE."
    (let ((tokens (split-query-tokens (str:downcase query)))
          (normalized-candidate (str:downcase candidate)))
      (list:all (fn (token)
                  (fuzzy-token-match-p token normalized-candidate))
                tokens)))

  (declare fuzzy-match-positions (String -> String -> (List UFix)))
  (define (fuzzy-match-positions query candidate)
    "Return sorted unique CANDIDATE positions matched by QUERY."
    (if (blank-query? query)
        Nil
        (let ((tokens (split-query-tokens (str:downcase query)))
              (normalized-candidate (str:downcase candidate)))
          (rec collect ((remaining tokens)
                        (positions Nil))
            (match remaining
              ((Nil)
               (list:sort (list:remove-duplicates positions)))
              ((Cons token rest)
               (let ((token-positions
                       (fuzzy-token-positions token normalized-candidate)))
                 (if (list:null? token-positions)
                     Nil
                     (collect rest
                              (list:append token-positions positions))))))))))

  (declare string-prefix? (String -> String -> Boolean))
  (define (string-prefix? prefix string)
    (let ((prefix-length (str:length prefix))
          (string-length (str:length string)))
      (if (> prefix-length string-length)
          False
          (== prefix (str:substring string 0 prefix-length)))))

  (declare word-boundary-character? (Char -> Boolean))
  (define (word-boundary-character? character)
    (or (== character #\-)
        (== character #\_)
        (== character #\/)
        (== character #\.)
        (== character #\Space)))

  (declare word-boundary-position? (String -> UFix -> Boolean))
  (define (word-boundary-position? candidate position)
    (or (== position 0)
        (word-boundary-character?
         (str:ref-unchecked candidate (- position 1)))))

  (declare consecutive-position-bonus ((List UFix) -> Integer))
  (define (consecutive-position-bonus positions)
    (match positions
      ((Nil) 0)
      ((Cons first rest)
       (rec scan ((previous first)
                  (remaining rest)
                  (bonus (the Integer 0)))
         (match remaining
           ((Nil) bonus)
           ((Cons position tail)
            (scan position
                  tail
                  (if (== position (+ previous 1))
                      (+ bonus (the Integer 5))
                      bonus))))))))

  (declare word-boundary-bonus (String -> (List UFix) -> Integer))
  (define (word-boundary-bonus candidate positions)
    (rec scan ((remaining positions)
               (bonus (the Integer 0)))
      (match remaining
        ((Nil) bonus)
        ((Cons position tail)
         (scan tail
               (if (word-boundary-position? candidate position)
                   (+ bonus (the Integer 8))
                   bonus))))))

  (declare fuzzy-token-score-or-negative-one (String -> String -> Integer))
  (define (fuzzy-token-score-or-negative-one token candidate)
    "Score TOKEN against CANDIDATE, or return -1 when it does not match."
    (let ((positions (fuzzy-token-positions token candidate)))
      (if (list:null? positions)
          (the Integer -1)
          (let ((token-length (str:length token))
                (candidate-length (str:length candidate))
                (first-position (list:car positions)))
            (let ((score1
                    (+ (the Integer 1)
                       (if (== token candidate)
                           (the Integer 100)
                           (the Integer 0)))))
              (let ((score2
                      (+ score1
                         (if (and (<= token-length candidate-length)
                                  (string-prefix? token candidate))
                             (the Integer 50)
                             (the Integer 0)))))
                (let ((score3
                        (+ score2
                           (if (str:substring? token candidate)
                               (the Integer 30)
                               (the Integer 0)))))
                  (let ((score4
                          (+ score3
                             (max (the Integer 0)
                                  (- (the Integer 20)
                                     (as Integer first-position))))))
                    (let ((score5
                            (+ score4
                               (consecutive-position-bonus positions))))
                      (+ score5
                         (word-boundary-bonus candidate positions)))))))))))

  (declare fuzzy-score-or-negative-one (String -> String -> Integer))
  (define (fuzzy-score-or-negative-one query candidate)
    "Score QUERY against CANDIDATE, or return -1 when any token fails."
    (let ((tokens (split-query-tokens (str:downcase query)))
          (normalized-candidate (str:downcase candidate)))
      (if (list:null? tokens)
          (the Integer 0)
          (rec score ((remaining tokens)
                      (total (the Integer 0)))
            (match remaining
              ((Nil) total)
              ((Cons token rest)
               (let ((token-score
                       (fuzzy-token-score-or-negative-one token
                                                          normalized-candidate)))
                 (if (< token-score 0)
                     (the Integer -1)
                     (score rest (+ total token-score)))))))))))
