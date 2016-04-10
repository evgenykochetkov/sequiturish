(ns sequiturish.core-test
  (:require [clojure.test :refer :all]
            [sequiturish.core :refer :all]))

(def rule-symbol #'sequiturish.core/rule-symbol)
(def enforce-dirgam-uniqueness #'sequiturish.core/enforce-dirgam-uniqueness)
(def enforce-rule-utility #'sequiturish.core/enforce-rule-utility)

(deftest rule-symbols-equality
  (testing "rule symbols with same ids must be equal"
    (is (= (rule-symbol 10)
           (rule-symbol 10)))))

(deftest enforcing-dirgam-uniqueness
  (testing "enforcing dirgam uniqueness"
    (is (= {0 [\a \b (rule-symbol 1) (rule-symbol 1)]
            1 [\c \d]}
           (enforce-dirgam-uniqueness {0 [\a \b \c \d \c \d]})))))

(deftest enforcing-rule-utility
  (testing "useless rules must be removed"
    (is (= {0 [\a \b \c \q \w]}
           (enforce-rule-utility {0 [\a \b \c (rule-symbol 1)]
                                  1 [\q \w]})))))

(deftest main
  (testing "example sequence from original paper"
    (is (= {0 [(rule-symbol 3) (rule-symbol 1) (rule-symbol 3)]
            1 [\b \c]
            3 [\a (rule-symbol 1) \d]}
           (sequiturish "abcdbcabcd")))))
