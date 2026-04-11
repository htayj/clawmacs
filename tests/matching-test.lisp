(in-package :clawmacs/tests)

(in-suite matching-suite)

(test matching-core-is-loaded
  "The minibuffer matching API is backed by the Coalton core package."
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

(test fuzzy-score-preserves-ranking-contract
  "Exact, prefix, substring, early, consecutive, and boundary matches score higher."
  (is (= 0 (fuzzy-score "" "glm-5")))
  (is (null (fuzzy-score "zz" "glm-5")))
  (is (> (fuzzy-score "glm" "glm-5")
         (fuzzy-score "gm" "glm-5")))
  (is (> (fuzzy-score "gpt" "gpt")
         (fuzzy-score "gpt" "zzzgpt"))))
