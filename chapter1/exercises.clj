;; Chapter 1 book examples and exercises

;; 1.3
(defn larger-sum-squares
  "Takes 3 numbers and returns the sum of the squares of the 2 larger numbers"
  [x y z]
  (reduce + (map #(* % %) (rest (sort [x y z])))))

;; 1.4
(defn a-plus-abs-b
  [a b]
  ((if (> b 0)
    +
    -) a b))

;; Newton's square root method
(defn square
  [x]
  (* x x))

(defn good-enough
  [guess x]
  (< (Math/abs (- (square guess) x)) 0.001))

(defn average
  [x y]
  (/ (+ x y) 2))

(defn improve
  [guess x]
  (average guess (/ x guess)))

(defn sqrt-iter
  [guess x]
  (if (good-enough guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt
  [x]
  (sqrt-iter 1.0 x))


;; Newton's square root method with new-if
(defn new-if
  [predicate then-clause else-clause]
  (cond predicate
        then-clause
        :else
        else-clause))

(defn sqrt-iter-new
  [guess x]
  (new-if (good-enough guess x) guess (sqrt-iter-new (improve guess x) x)))

(defn sqrt-new
  [x]
  (sqrt-iter-new 1.0 x))

;; `new-if` will not work with `sqrt-new`, because lisp uses applicative-
;; order evaluation (first evaluate the operator and operands and then apply
;; resulting procedure to resulting arugments) - meaning that the `else-clause`
;; is always evaluated even if the `predicate` evaluates to `true`, which
;; eventually calls itself and causes a `StackOverflowError`)

;; new good-enough
(defn good-enough-new
  [guess x]
  (< (/ (Math/abs (- (improve guess x) guess)) guess) 0.001))

(defn sqrt-iter-new
  [guess x]
  (if (good-enough-new guess x)
    guess
    (sqrt-iter-new (improve guess x) x)))

(defn sqrt-new
  [x]
  (sqrt-iter-new 1.0 x))


;; 1.8 Newton's method for cube roots
(defn cube
  [x]
  (* x (* x x)))

(defn good-enough-cube
  [guess x]
  (< (/ (Math/abs (- (improve-cube guess x) guess)) guess) 0.001))

(defn improve-cube
  [guess x]
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(defn cube-iter
  [guess x]
  (if (good-enough-cube guess x)
    guess
    (cube-iter (improve-cube guess x) x)))

(defn cube-rt
  [x]
  (cube-iter 1.0 x))

;; Square root in block structure
(defn sqrt-block
  [x]
  (defn good-enough
    [guess x]
    (< (/ (Math/abs (- (improve guess x) guess)) guess) 0.001))
  (defn improve
    [guess x]
    (average guess (/ x guess)))
  (defn sqrt-iter
    [guess x]
    (if (good-enough guess x)
      guess
      (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;; Square root in block structure with lexical scoping
;; (allow the value of x to be shared by the internal functions)
(defn sqrt-block-lex
  [x]
  (defn good-enough
    [guess]
    (< (/ (Math/abs (- (improve guess) guess)) guess) 0.001))
  (defn improve
    [guess]
    (average guess (/ x guess)))
  (defn sqrt-iter
    [guess]
    (if (good-enough guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
