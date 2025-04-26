;; Copyright (c) 2025 Javier de Lope
;;
;; This program and the accompanying materials are made available
;; under the terms of the Eclipse Public License 2.0 which is
;; available at http://www.eclipse.org/legal/epl-2.0.
;;
;; SPDX-License-Identifier: EPL-2.0


(ns fuzzy.core
  "Basic functions for fuzzy inference systems and an example based on
  the tipping problem."
  (:require [fuzzy.mf])
  (:require [fuzzy.ops]))

(def fis
  "Tipping problem.
  Defines the membership functions, rulebase and operators for the
  tipping problem."
  '{:sets
    {:service {:poor (fuzzy.mf/trimf (0.0 0.0 5.0)),
               :good (fuzzy.mf/trimf (0.0 5.0 10.0)),
               :excellent (fuzzy.mf/trimf (5.0 10.0 10.0)) },
     :food {:rancid (fuzzy.mf/trapmf (-2.0 0.0 1.0 3.0)),
            :delicious (fuzzy.mf/trapmf (7.0 9.0 10.0 12.0)) },
     :tip {:cheap (fuzzy.mf/trimf (0.0 5.0 10.0)),
           :average (fuzzy.mf/trimf (10.0 15.0 20.0)),
           :generous (fuzzy.mf/trimf (20.0 25.0 30.0)) } },
    :rules
    (((:or (:is :service :poor) (:is :food :rancid))
      (:is :tip :cheap))
     ((:is :service :good)
      (:is :tip :average))
     ((:or (:is :service :excellent) (:is :food :delicious))
      (:is :tip :generous)) )
    :operators
    {:or max, :and min, :not fuzzy.ops/not} } )

(def input-vars
  "Example of input variables."
  '{:service 2.0 :food 5.0})


(defn fuzzify
  "Fuzzification of a simple variable.
  Computes the fuzzy value for a input variable and the membership
  function according to the definition.
  Example: (fuzzify fis :service :poor 2)"
  [fis variable key value]
  (let [fuzzy-set (get (get (get fis :sets) variable) key)]
    (eval (conj (last fuzzy-set) value (first fuzzy-set)))))


(defn fuzzify-list
  "Fuzzification of a list.
  Computes the fuzzy value for all the input variables and the membership
  function according to the definition.
  Example: (fuzzify-list fis (first rule-5) input-vars)"
  [fis l input]
  (cond
    (empty? l) ()
    (seq? (first l)) (cons (fuzzify-list fis (first l) input) (fuzzify-list fis (rest l) input))
    (= (first l) ':is) (fuzzify fis (second l) (last l) (get input (second l)))
    :else (cons (get (get fis :operators) (first l)) (fuzzify-list fis (rest l) input)))
  )

(defn apply-ops1 [fis rules input]
  (if (empty? rules) ; when-not (?)
    ()
    (cons (reverse (conj (reverse (rest (last (first rules))))
                                  (eval (fuzzify-list fis (first (first rules)) input))))
          (apply-ops1 fis (rest rules) input))))

(defn apply-ops
  "Apply the operators.
  The fuzzy operators are applied for the input vars to a fuzzy inference system.
  Example:  (fuzzy.core/apply-ops fuzzy.core/fis fuzzy.core/input-vars)"
  [fis input]
  (apply-ops1 fis (get fis :rules) input))

(defn get-value1 [fis input variable x list]
  (if (empty? list) ; when-not (?)
    ()
    (if (= (first (first list)) variable)
      (cons (min (fuzzify fis variable (second (first list)) x) ; also prod (implication)
                 (last (first list)))
            (get-value1 fis input variable x (rest list)))
      (get-value1 fis input variable x (rest list)))))

(defn get-value
  "Solve implication and aggregation.
  Get the fuzzy value of a output fuzzy set for x. The min operator is
  used for implication and the max operator for the aggregation.
  Example: (get-value fis input-vars :tip 5.0)"
  [fis input variable x]
  (let [list (apply-ops fis input)]
    (eval (conj (get-value1 fis input variable x list) 'max)))) ; also probor, max (aggregation)

(defn centroid1 [fis input variable x max num den]
  (if (< x max)
    (let [mu (get-value fis input variable x)]
      (centroid1 fis input variable (+ x 0.1) max (+ num (* mu x)) (+ den mu)))
    (/ num den)))

(defn centroid
  "Centroid defuzzification.
  Returns the center of gravity of the fuzzy set along the x-axis.
  Example: (fuzzy.core/centroid fuzzy.core/fis '{:service 0.0 :food 10.0} :tip)"
  [fis input variable]
  (let [fuzzy-set (get (get fis :sets) variable)
        set-ranges (mapcat last (vals fuzzy-set))
        min-val (apply min set-ranges)
        max-val (apply max set-ranges)]
    (centroid1 fis input variable min-val max-val 0 0)))
