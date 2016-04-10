(ns sequiturish.core
    (:require [sequiturish.utils :as u]))

;;
;; RuleSymbol type
;;
(defrecord RuleSymbol [id]
  Object
    (toString [this]
      (str "RuleSymbol#" (.-id this))))

;; making factory function private
;; see http://stackoverflow.com/questions/15856805/how-to-make-a-local-type-in-clojure
(alter-meta! #'->RuleSymbol assoc :private true)

(defn- rule-symbol [id] (->RuleSymbol id))

(defn rule-symbol? [sym] (= (type sym) RuleSymbol))

(def terminal? (complement rule-symbol?))

(defn rule-id [r] (.-id r))

;;
;; Grammar
;;
(def main-rule-id 0)

(def empty-grammar {main-rule-id []})

(defn main-sequence [grammar]
  (grammar main-rule-id))

;;
;; Rule 1: digram uniqueness
;;
(defn- digrams [grammar]
  (mapcat #(partition 2 1 %) (vals grammar)))

(defn- digram-index [grammar]
  (reduce-kv
   (fn [res rule-id rule-symbols]
     (->> (partition 2 1 rule-symbols)
       (map vector (range))
       (reduce (fn [acc [idx digram]]
                (update-in acc [digram] conj [rule-id idx]))
               res)))
   {}
   grammar))

(defn- overlapping?
  [[rule-id1 index1] [rule-id2 index2]]
  (and (= rule-id1 rule-id2)
       (>= 1 (u/abs (- index1 index2)))))

; Array of [rule-id index]
(defn- remove-overlapping [digram-positions]
  (if (<= 1 (count digram-positions))
    digram-positions
    (reduce (fn [acc curr-pos]
              (let [last-pos (last acc)]
                (if-not (and last-pos (overlapping? curr-pos last-pos))
                  (conj acc curr-pos)
                  acc)))
            []
            (sort digram-positions))))

(defn- find-duplicated-digrams [grammar]
  (reduce-kv (fn [acc digram digram-positions]
               (let [not-overlapping-positions (remove-overlapping digram-positions)]
                 (if (>= 1 (count not-overlapping-positions))
                   acc
                   (conj acc [digram not-overlapping-positions]))))
             nil
             (digram-index grammar)))

(defn- next-rule-id [grammar]
  (inc (apply max (keys grammar))))

;; TODO: refactor and move to utils
(defn- replace-digram
  [sq digram replacement]
  (loop [replaced []
         remaining sq]
    (if-not (seq remaining)
      replaced
      (if (= digram (take 2 remaining))
        (recur (conj replaced replacement) (drop 2 remaining))
        (recur (conj replaced (first remaining)) (rest remaining))))))

(defn- whole-rule? [grammar [r-id idx]]
  (and (not= main-rule-id r-id)
       (zero? idx)
       (= 2 (count (grammar r-id)))))

(defn- enforce-dirgam-uniqueness
  [grammar]
  (if-let [[duplicate-digram positions] (first (find-duplicated-digrams grammar))]
    (if-let [[existing-rule-id _] (first (filter (partial whole-rule? grammar) positions))]
      (let [existing-rule-symbol (rule-symbol existing-rule-id)
            positions-without-existing-rule (remove #(= % [existing-rule-id 0]) positions)]
         (reduce (fn [g [r-id _]]
                    (update g r-id replace-digram duplicate-digram existing-rule-symbol))
                 grammar
                 positions-without-existing-rule))
      (let [new-rule-id (next-rule-id grammar)
            new-rule-symbol (rule-symbol new-rule-id)
            grammar-with-new-rule (assoc grammar new-rule-id (vec duplicate-digram))]
         (reduce (fn [g [r-id _]]
                    (update g r-id replace-digram duplicate-digram new-rule-symbol))
                 grammar-with-new-rule
                 positions)))
    grammar))

;;
;; Rule 2: rule utility
;;
(defn- rules-used-only-once [grammar]
  (->> (vals grammar)
    (mapcat (partial filter rule-symbol?))
    (frequencies)
    (filter (fn [[rule freq]] (= 1 freq)))
    (map first)))

;; TODO: refactor and move to utils
(defn- replace-rule
  [rule replacement sq]
  (loop [replaced []
         remaining sq]
    (if-not (seq remaining)
      replaced
      (if (= rule (first remaining))
        (recur (into replaced replacement) (rest remaining))
        (recur (conj replaced (first remaining)) (rest remaining))))))

(defn- enforce-rule-utility
  [grammar]
  (if-let [useless-rule (first (rules-used-only-once grammar))]
    (let [useless-rule-id (rule-id useless-rule)
          rule-symbols (grammar useless-rule-id)]
       (u/map-vals (partial replace-rule useless-rule rule-symbols)
                   (dissoc grammar useless-rule-id)))
    grammar))

;;
;; Main algorithm
;;
(defn process-symbol [grammar new-symbol]
  (let [new-grammar (update-in grammar [main-rule-id] conj new-symbol)]
    (if (seq (main-sequence grammar)) ; was it not the first inserted symbol?
      (loop [g new-grammar]
        (let [optimized-grammar (-> g enforce-dirgam-uniqueness enforce-rule-utility)]
          (if (= g optimized-grammar)
            g
            (recur optimized-grammar))))
      new-grammar)))

(defn sequiturish [symbols]
  (reduce process-symbol empty-grammar symbols))
