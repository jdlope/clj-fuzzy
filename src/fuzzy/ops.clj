;; Copyright (c) 2025 Javier de Lope
;;
;; This program and the accompanying materials are made available
;; under the terms of the Eclipse Public License 2.0 which is
;; available at http://www.eclipse.org/legal/epl-2.0.
;;
;; SPDX-License-Identifier: EPL-2.0


(ns fuzzy.ops
  "Collection of operators that can be used as either fuzzy operators
  when evaluating rule antecedents or aggregation operators when
  combining the output returned by the implication process."
  )

(defn not
  [x]
  (- 1 x))

(defn probor
  "Probabilistic OR (algebraic sum).
  Returns the probabilistic OR of the input values. When the function
  receives more than two values, the function is applied on the first
  two values. Then, the function is applied on the result and the
  third value, and so on."
  [x & ys]
  (reduce (fn [a b]
            (- (+ a b) (* a b)))
          (conj ys x)))
