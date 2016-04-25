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


;; Section 1.2


;; Factorials

;; Method 1: linear recursive process - length of chain of deferred
;; multiplications (and amount of information needed to keep track of it)
;; grows linearly with n
(defn factorial
  [n]
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

;; Method 2: linear iterative - state can be summarized by a fixed number
;; of state variables with a fixed rule that describes how the variables should
;; be updated from one state to another. Number of steps grows linearly
;; with n
(defn fact-iter
  [product counter max-count]
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               (max-count))))

(defn factorial
  [n]
  (fact-iter 1 1 n))


;; Example: Counting Change

(defn count-change
  [amount]
  (cc amount 5))

(defn cc
  [amount kinds-of-coins]
  (cond (= amount 0)
        1
        (or (< amount 0) (= kinds-of-coins 0))
        0
        :else
        (+ (cc amount (- kinds-of-coins 1))
           (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins))))

(defn first-denomination
  [kinds-of-coins]
  (cond (= kinds-of-coins 1)
        1
        (= kinds-of-coins 2)
        5
        (= kinds-of-coins 3)
        10
        (= kinds-of-coins 4)
        25
        (= kinds-of-coins 5)
        50))


;; 1.11

;; Recursive process
(defn my-fn
  [n]
  (cond (< n 3)
        n
        :else
        (+ (my-fn (- n 1))
           (* 2 (my-fn (- n 2)))
           (* 3 (my-fn (- n 3)))
        )
  )
)

;; Iterative process
(defn my-fn-iter
  [a b c count]
  (if (= count 0)
    a
    (my-fn-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))

(defn my-fn-new
  [n]
  (my-fn-iter 0 1 2 n))


;; 1.12

(defn pascal-tri
  [row col]
  (cond (> col row)
        0
        (< col 0)
        0
        (= col 1)
        1
        :else
        (+ (pascal-tri (- row 1) (- col 1))
           (pascal-tri (- row 1) col))))

;; 1.15
(defn cube
  [x]
  (* x x x))

(defn p
  [x]
  (- (* 3 x) (* 4 (cube x))))

(defn sine
  [angle]
  (if (not (> (Math/abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

;; 1.16
(defn is-even
  [x]
  (= (mod x 2) 0))

(defn exp-iter
  [a b n]
  (cond (= n 0)
        a
        (is-even n)
        (exp-iter a (square b) (/ n 2))
        :else
        (exp-iter (* a b) b (- n 1))))

(defn fast-exp
  "Compute b^n"
  [b n]
  (exp-iter 1 b n))


;; 1.17
(defn times-2
  [x]
  (+ x x))

(defn halve
  [x]
  (/ x 2))

(defn mult
  [a b]
  (mult-iter 0 a b))

(defn mult-iter
  [a b]
  (cond (= b 0)
        0
        (is-even b)
        (times-2 (mult-iter a (halve b)))
        :else
        (+ a (mult-iter a (- b 1)))))


;; 1.18
(defn mult-iter
  [s a b]
  (cond (= b 0)
        s
        (even b)
        (mult-iter s (times-2 a) (halve b))
        :else
        (mult-iter (+ s a) a (- b 1))))


;; 1.19
;; lol no

;; 1.20
;; Normal order: "fully expand and then reduce"
;; Applicative order: "evaluate arguments then apply"
;; if statements: predicate expression is evaluated first, and the result
;; determines whether to evaluate the consequent or alternative expression
(defn gcd
  [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))

;; normal order
(gcd 206 40)

(gcd 40 (mod 206 40))

(if (= (mod 206 40) 0) 0) ; one
(gcd (mod 206 40) (mod 40 (mod 206 40)))

(if (= (mod 40 (mod 206 40))) 0) ; two
(gcd (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))

(if (= (mod (mod 206 40) (mod 40 (mod 206 40))) 0)) ; four
(gcd (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))

(if (= (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))) 0)) ; seven
(mod (mod 206 40) (mod 40 (mod 206 40))) ; four
;; `mod` performed 18 times in normal order

;; applicative order
(gcd 206 40)
(gcd 40 (mod 206 40)) ; one
(gcd 40 6)
(gcd 6 (mod 40 6)) ; one
(gcd 6 4)
(gcd 4 (mod 6 4)) ; one
(gcd 4 2)
(gcd 2 (mod 4 2)) ; one
(gcd 2 0)
2
;; `mod` performed 4 times in applicative order


;; Searching for divisors
(defn divides
  [a b]
  (= (mod b a) 0))

(defn find-divisor
  [n test-divisor]
  (cond (> (square test-divisor) n)
        n
        (divides test-divisor n)
        test-divisor
        :else
        (find-divisor n (+ test-divisor 1))))

(defn smallest-divisor
  [n]
  (find-divisor n 2))

(defn is-prime
  [n]
  (= n (smallest-divisor n)))

;; 1.21
(smallest-divisor 199) ;; 199
(smallest-divisor 1999) ;; 1999
(smallest-divisor 19999) ;; 7

;; Fermat's Little Theorem
(defn expmod
  [base exp m]
  (cond (= exp 0)
        1
        (is-even exp)
        (mod (square (expmod base (/ exp 2) m)))
        :else
        (mod (* base (expmod base (- exp 1) m)) m)))

(defn fermat-test
  [n]
  (defn try-it
    [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (rand-int (- n 1)))))

(defn fast-prime
  [n times]
  (cond (= times 0)
        true
        (fermat-test n)
        (fast-prime n (- times 1))
        :else
        false))


;; 1.22
(defn report-prime
  [elapsed-time]
  (pprint " *** ")
  (pprint elapsed-time))

(defn start-prime-test
  [n start-time]
  (if (is-prime n)
    (report-prime (- (time) start-time))))

(defn timed-prime-test
  [n]
  (newline)
  (pprint n)
  (start-prime-test n (time)))


;; 1.23
(defn next
  [n]
  (if (= 2 n)
    3
    (+ n 2)))

(defn divides
  [a b]
  (= (mod b a) 0))

(defn find-divisor-next
  [n test-divisor]
  (cond (> (square test-divisor) n)
        n
        (divides test-divisor n)
        test-divisor
        :else
        (find-divisor n (next n))))

(defn smallest-divisor-next
  [n]
  (find-divisor-next n 2))

(defn timed-prime-test-next
  [n]
  (= n (smallest-divisor-next n)))
