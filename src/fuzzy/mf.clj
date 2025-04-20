;; Copyright (c) 2025 Javier de Lope
;;
;; This program and the accompanying materials are made available
;; under the terms of the Eclipse Public License 2.0 which is
;; available at http://www.eclipse.org/legal/epl-2.0.
;;
;; SPDX-License-Identifier: EPL-2.0


(ns fuzzy.mf
  "Collection of commonly used membership functions built from several
  basic functions: piecewise linear, Gaussian distribution, sigmoid
  curve and quadratic and cubic polynomial curves functions."
  (:require [clojure.math :as math]))

;; Piecewise linear functions

(defn trimf
  "Triangular membership function.
  Returns the membership value of `x`, `a` and `c` define the _feet_
  of the membership function, and `b` defines its _peak_."
  [x a b c]
  (cond
    (< x a) 0.0
    (< x b) (/ (- x a) (- b a))
    (< x c) (/ (- c x) (- c b))
    :else   0.0))

(defn trapmf
  "Trapezoidal membership function.
  Returns the membership value of `x`, `a` and `d` define the _feet_
  of the membership function, and `b` and `c` define its _shoulders_.
    When `b` is equal to `c`, the resulting member ship function is
  equivalent to a triangular membership function. When `c` is less
  than `b`, the resulting membership function is triangular with a
  maximum value less than 1."
  [x a b c d]
  (cond
    (< x a) 0.0
    (< x b) (/ (- x a) (- b a))
    (< x c) 1.0
    (< x d) (/ (- d x) (- d c))
    :else   0.0))

(defn linzmf
  "Linear z-shaped saturation membership function.
  Returns the membership value of `x`, `a` defines the _shoulder_ of
  the membership function, and `b` defines its _foot_.
    Setting `a` equal to `b` produces a crisp membership function."
  [x a b]
  (cond
    (< x a) 1.0
    (< x b) (/ (- b x) (- b a))
    :else   0.0))

(defn linsmf
  "Linear s-shaped saturation membership function.
  Returns the membership value of `x`, `a` defines the _foot_ of the
  membership function, and `b` defines its _shoulder_. Setting `a`
  equal to `b` produces a crisp membership function."
  [x a b]
  (cond
    (< x a) 0.0
    (< x b) (/ (- x a) (- b a))
    :else   1.0))

;; Gaussian distribution functions

(defn gaussmf
  "Gaussian membership function.
  Returns the membership value of `x`, `s` defines the standard
  deviation and `c` is the mean."
  [x s c]
  (math/exp (/ (- (math/pow (- x c) 2)) (* 2 s s))))

(defn gauss2mf
  "Gaussian combination membership function.
  Returns the membership value of `x`, `s1` and `c1` are the standard
  deviation and mean of the left function, respectively, and `s2` and
  `c2` are the standard deviation and mean of the right function,
  respectively.
    When `c1` is less or equal to `c2`, the function reaches a maximum
  value of 1 over the range [c1,c2]; otherwise, the maximum value is
  less than 1."
  [x s1 c1 s2 c2]
  (cond
    (< x c1) (gaussmf x s1 c1)
    (< x c2) 1.0
    :else    (gaussmf x s2 c2)))

(defn gbellmf
  "Generalized bell-shaped membership function.
  Returns the membership value of `x`, `a` defines the width of the
  membership function, `b` defines the shape of the curve on either
  side of the central plateau (a larger value creates a more steep
  transition) and `c` defines the center of the membership function."
  [x a b c]
  (/ 1.0 (+ 1 (math/pow (abs (/ (- x c) a)) (* 2 b)))))

;; Sigmoid curve functions

(defn sigmf
  "Sigmoidal membership function.
  Returns the membership value of `x`, the magnitude of `a` controls
  the width of the transition area, `c` defines its center.
    To open the membership function to the left or right, specify a
  negative or positive value for `a`, respectively."
  [x a c]
  (/ 1 (+ 1 (math/exp (- (* a (- x c)))))))

(defn dsigmf
  "Difference between two sigmoidal membership functions.
  Returns the membership value of `x`, `a1` and `c1` are the
  parameters of the first sigmoidal function, and `a2` and `c2` are
  the parameters of the second sigmoidal function.
    For each sigmoidal function, to open the function to the left or
  right, specify a negative or positive value for a, respectively. The
  magnitude of a defines the width of the transition area, and
  parameter c defines the center of the transition area.
    To define a unimodal membership function with a maximum value of
  1, specify the same signs for `a1` and `a2`, and select `c` values
  far enough apart to allow for both transition areas to reach 1."
  [x a1 c1 a2 c2]
  (- (sigmf x a1 c1) (sigmf x a2 c2)))

(defn psigmf
  "Product of two sigmoidal membership functions.
  Returns the membership value of `x`, `a1` and `c1` are the
  parameters of the first sigmoidal function, and `a2` and `c2` are
  the parameters of the second sigmoidal function.

    For each sigmoidal function, to open the function to the left or
  right, specify a negative or positive value for a, respectively. The
  magnitude of a defines the width of the transition area, and
  parameter c defines the center of the transition area.

    To define a unimodal membership function with a maximum value of
  1, specify opposite signs for `a1` and `a2`, and select `c` values
  far enough apart to allow for both transition areas to reach 1."
  [x a1 c1 a2 c2]
  (* (sigmf x a1 c1) (sigmf x a2 c2)))

;; Quadratic and cubic polynomial curves functions

(defn zmf
  "Z-shaped membership function.
  Returns the membership value of `x`, `a` defines the _shoulder_ of
  the membership function and `b` defines its _foot_."
  [x a b]
  (cond
    (< x a) 1.0
    (< x (/ (+ a b) 2)) (- 1 (* 2 (math/pow (/ (- x a) (- b a)) 2)))
    (< x b) (* 2 (math/pow (/ (- x b) (- b a)) 2))
    :else   0.0))

(defn smf
  "S-shaped membership function.
  Returns the membership value of `x`, `a` defines the _foot_ of
  the membership function and `b` defines its _shoulder_."
  [x a b]
  (cond
    (< x a) 0.0
    (< x (/ (+ a b) 2)) (* 2 (math/pow (/ (- x a) (- b a)) 2))
    (< x b) (- 1 (* 2 (math/pow (/ (- x b) (- b a)) 2)))
    :else   1.0))

(defn pimf
  "Pi-shaped membership function.
  Returns the membership value of `x`, `a` and `d` define the _feet_
  of the membership function and `b` and `c` define its _shoulders_."
  [x a b c d]
  (cond
    (< x a) 0.0
    (< x (/ (+ a b) 2)) (* 2 (math/pow (/ (- x a) (- b a)) 2))
    (< x b) (- 1 (* 2 (math/pow (/ (- x b) (- b a)) 2)))
    (< x c) 1.0
    (< x (/ (+ c d) 2)) (- 1 (* 2 (math/pow (/ (- x c) (- d c)) 2)))
    (< x d) (* 2 (math/pow (/ (- x d) (- d c)) 2))
    :else   0.0))
