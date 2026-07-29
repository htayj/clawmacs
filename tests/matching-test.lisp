(in-package :rplaca/tests)

(in-suite matching-suite)

(test matching-core-is-loaded
  "The minibuffer matching API is backed by the matching core package."
  (is-true (matching-core-available-p)))

(test split-query-tokens-removes-empty-space-runs
  "Space-separated matching tokens ignore empty segments."
  (is (equal '("foo" "bar" "baz")
             (split-query-tokens " foo  bar baz "))))

(test fuzzy-match-p-is-case-insensitive-and-orderless
  "Every query token must match the same candidate as a subsequence."
  (is-true (fuzzy-match-p "G5" "glm-5"))
  (is-true (fuzzy-match-p "cod gpt" "openai-codex/gpt-5.4"))
  (is-false (fuzzy-match-p "zz" "glm-5")))

(test fuzzy-match-positions-are-sorted-and-unique
  "Position highlighting keeps the old sorted unique position contract."
  (is (equal '(0 1 4)
             (fuzzy-match-positions "gl 5" "glm-5")))
  (is (null (fuzzy-match-positions "" "glm-5")))
  (is (null (fuzzy-match-positions "zz" "glm-5"))))

(test matching-core-exported-contracts-are-stable
  "Direct matching-core calls preserve CL-facing sentinel and list contracts."
  (is (equal '(0 2)
             (rplaca/matching-core:fuzzy-token-positions "gm" "glm-5")))
  (is (null (rplaca/matching-core:fuzzy-token-positions "zz" "glm-5")))
  (is-true (rplaca/matching-core:fuzzy-token-match-p "" "glm-5"))
  (is-false (rplaca/matching-core:fuzzy-token-match-p "zz" "glm-5"))
  (is (= -1 (rplaca/matching-core:fuzzy-token-score-or-negative-one
             "zz" "glm-5")))
  (is (= -1 (rplaca/matching-core:fuzzy-score-or-negative-one
             "zz" "glm-5")))
  (is (= 0 (fuzzy-score "" "glm-5"))))

(test fuzzy-score-preserves-ranking-contract
  "Exact, prefix, substring, early, consecutive, and boundary matches score higher."
  (is (= 0 (fuzzy-score "" "glm-5")))
  (is (null (fuzzy-score "zz" "glm-5")))
  (is (> (fuzzy-score "glm" "glm-5")
         (fuzzy-score "gm" "glm-5")))
  (is (> (fuzzy-score "gpt" "gpt")
         (fuzzy-score "gpt" "zzzgpt"))))
