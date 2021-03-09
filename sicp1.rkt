#lang sicp

(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (max x y)
  (if (> x y) x y))

; This is a comment
; 1.2
;; (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;1.3
(define (max-sum-of-squares x y z)
  (if (and (>= x y) (>= y z))
      (sum-of-squares x y)
      (if (and (>= x y) (>= z y))
          (sum-of-squares x z)
          (sum-of-squares y z))))

;1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))



(define (average x y)
  (/ (+ x y) 2))


(define (sqrt x)
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x)))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (sqrt-iter 1.0 x))

;1.6 If used in sqrt-iter instead of special form if will result in infinate method calls being made to evaluate (sqrt-iter (improve guessx) x), the third parameter passed to "if", due to applicative order execution
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;1.7
;; (sqrt 0.001)
;; (square (sqrt 0.001))
;(sqrt 900000000000000000)
;(square (sqrt 900000000000000000))

(define (sqrtt x)
  (define (sqrt-iter-diff guess lastguess x)
    (define (close-enough? guess lastguess x)
      (< (abs (- guess lastguess)) (/ x 10000000)))
    (define (improve guess x)
      (average guess (/ x guess)))
    (if (close-enough? guess lastguess x)
        guess
        (sqrt-iter-diff (improve guess x)
                        guess
                        x)))
  (sqrt-iter-diff 1.0 0 x))



;1.8
(define (cbrt x)
  (define (cube-iter guess lastguess x)
    (define (close-enough? guess lastguess x)
      (< (abs (- guess lastguess)) (/ x 10000000)))
    (define (cube-improve guess x)
      (/ (+ (/ x (square guess)) (* 2 guess)) 3))
    (if (close-enough? guess lastguess x)
        guess
        (cube-iter (cube-improve guess x)
                   guess
                   x)))
  (cube-iter 1.0 0 x))


;1.9
;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))
; recursive process
;; (+ 4 5)
;; (inc (+ (dec 4) 5))
;; (inc (+ 3 5))
;; (inc (inc (+ (dec 3) 5)))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ dec(2) 5))))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ dec(1) 5)))))
;; (inc (inc (inc (inc ( + 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; (9)


;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))
; iterative process
;; (+ 4 5)
;; (+ (dec 4) (inc 5))
;; (+ 3 6)
;; (+ (dec 3) (inc 6))
;; (+ 2 7)
;; (+ (dec 2) (inc 7))
;; (+ 1 8)
;; (+ (dec 1) (inc 8))
;; (+ 0 9)
;; (9)


;1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; (A 2 4)
;; (A (- 2 1) (A 2 (- 4 1)))
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 (* 2 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (A 0 (A 0 (* 2 2))))
;; (A 1 (A 0 (A 0 4)))
;; (A 1 (A 0 (* 2 4)))
;; (A 1 (A 0 8))
;; (A 1 (* 2 8))
;; (A 1 16)



;; (f n) = 2n
;; (g n) = n^2
;; (h n) = ((n^2)^2)^2...   n^2 for n-1 times

;; (A 1 10)
;; (A (- 1 1) (A 1 (- 10 1)))
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;; (A 0 (A 0 (A 0 (A 0 (* 2 32)))))
;; (A 0 (A 0 (A 0 (A 0 64))))
;; (A 0 (A 0 (A 0 (* 2 64))))
;; (A 0 (A 0 (A 0 128)))
;; (A 0 (A 0 (* 2 128)))
;; (A 0 (A 0 256))
;; (A 0 (* 2 256))
;; (A 0 512)
;; (* 2 512)
;; ()

;; (A 2 3)
;; (A (- 2 1) (A 2 (- 3 1)))
;; (A 1 (A 2 2))
;; (A 1 (A 1 (A 2 (- 2 1))))
;; (A 1 (A 1 (A 2 1)))
;; (A 1 (A 1 2))
;; (A 1 (A (- 1 1) (A 1 (- 2 1))))
;; (A 1 (A 0 (A 1 1)))
;; (A 1 (A 0 2))
;; (A 1 (* 2 2))
;; (A 1 4)


;; (A (- 1 1) (A 1 (- n 1)))
;; (A 0 (A 0 (A 1 (- (- n 1) 1))))

;; (A 1 3)
;; (A (- 1 1) (A 1 (- 3 1)))
;; (A 0 (A 1 2))
;; (A 0 (A (- 1 1) (A 1 (- 2 1))))
;; (A 0 (A 0 (A 1 1)))
;; (A 0 (A 0 (2))

;; 1.2.2
(define (inefficient-fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (inefficient-fib (- n 1))
                 (inefficient-fib (- n 2))))))

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; Excercise 1.11
;; f(n) = n if n < 3
;; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3

(define (recfuncal n)
  (if (< n 3)
      n
      (+ (recfuncal (- n 1))
         (* 2 (recfuncal (- n 2)))
         (* 3 (recfuncal (- n 3))))))

(define (iterfuncal n)
  (funcal-iter 0 1 2 n))


(define (funcal-iter  a b c count)
  (if (= 0 count)
      a
      (funcal-iter b
                   c
                   (+ c (* 2 b) (* 3 a))
                   (- count 1))))

;; Excercie 1.12
;; 1
;; 11
;; 121
;; 1331
;; 14641


(define (calc-pascal r c)
  (cond ((= c 1) 1)
        ((= r c) 1)
        (else (+ (calc-pascal (- r 1) c) (calc-pascal (- r 1) (- c 1))))))



;; Excercie 1.13
;; phi = (1 + root(5))/2
;; (define (phi (/ (+ 1 (sqrt 5)) 2)))
;; (define sqrt5 (sqrt 5))
;; (define phi (/ (+ 1 sqrt5) 2))
;; (define psi (/ (- 1 sqrt5) 2))
;; See paper notes for proof

;; 1.14 paper notes

;; 1.15
(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; a
(define (howManyPs angle count)
  (if (not (> (abs angle) 0.1))
      count
      (howManyPs (/ angle 3.0) (+ 1 count))))

;; (howManyPs 12.15 0)

;; b
;; (howManyPs 24.3 0)
;; (howManyPs 36.45 0)
;; (howManyPs 50 0)

;; Theta = log n (base 3) because angle is reduced at n/3

;;1.2.4 Exponentiation

;; (define (expt b n)
;;   (if (= n 0)
;;       1
;;       (* b (expt b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


;; 1.16 bove definition is recursive, design an iterative exponentiation process
;; Hint:(b^(n/2))^2 = (b^2)^(n/2) keep state var a such that ab^n is constant between states
;; at the beginign of the process a = 1

(define (fast-iter-expt b n)
  (fast-iter-expt-iter b n 1))

(define (fast-iter-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-iter-expt-iter (square b) (/ n 2) a))
        (else (fast-iter-expt-iter b (- n 1) (* a b)))))


;; 1.17
(define (mult a b)
  (define (double x)
    (+ x x))
  (define (half x)
    (/ x 2))
  (cond ((= b 0) 0)
        ((even? b) (mult (double a) (half b)))
        (else (+ a (mult a (- b 1))))))

;; 1.18
(define (mult-it a b)
  (mult-iter a b 0))

(define (mult-iter a b n)
  (define (double x)
    (+ x x))
  (define (half x)
    (/ x 2))
  (cond ((= b 0) n)
        ((even? b) (mult-iter (double a) (half b) n))
        (else (mult-iter a (- b 1) (+ n a)))))


;; 1.19 (pg47)
(define (fast-fib n)
  (fib-iterr 1 0 0 1 n))

(define (fib-iterr a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iterr a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iterr (+ (* b q) (* a q) (* a p ))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;;1.20
;; Applicative
;; (remainder 206 40)
;; (remainder 40 6)
;; (remainder 6 4)
;; (remainder 4 2)
;; (remainder 2 0)
;;Normal order:
;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))....


(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next x)
  (if (= x 2) 3 (+ x 2)))


(define (divides? a b )
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;Compute the remainder of the exponetial "exp" of a number "base" modulo another number "m"
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; (define (expmod base exp m)
;;   (remainder (fast-expt base exp) m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n ) a))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;;1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (search-for-primes b n)
  (cond ((> b n) 0)
        ((even? b) (search-for-primes (+ b 1) n))
        (else
         (timed-prime-test b)
         (search-for-primes (+ b 2) n))))



;;1009 *** 2
;;1013 *** 1
;;1019 *** 2

;;10007 *** 5
;;10009 *** 5
;;10037 *** 5

;;100003 *** 14
;;100019 *** 14
;;100043 *** 15

;; 1000003 *** 41
;; 1000033 *** 41
;; 1000037 *** 41

;;1.23


;; 1009 *** 1
;; 1013 *** 2
;; 1019 *** 1

;; 10007 *** 3
;; 10009 *** 3
;; 10037 *** 3

;; 100003 *** 9
;; 100019 *** 9
;; 100043 *** 9

;; 1000003 *** 28
;; 1000033 *** 28
;; 1000037 *** 28
;;Number of steps is not the only factor in execution time.

;; 1009 *** 8
;; 1013 *** 8
;; 1019 *** 7

;; 10007 *** 12
;; 10009 *** 12
;; 10037 *** 12

;; 100003 *** 39
;; 100019 *** 20
;; 100043 *** 22
;; 100049 *** 14

;; 1000003 *** 15
;; 1000033 *** 15
;; 1000037 *** 14

;;1.25
;;1.26
;;Expmod will have to execute twice for each even... tree execution structure. Tree recursion as opposed to linear recursion
;; (expmod a n n)

;;1.27
(define (carm? n)
  (if (carm-loop n (- n 1))
      (if (prime? n) false true)
      (if (prime? n) true false)))

(define (carm-loop n current)
  (cond ((= current 0) true)
        ((= current (expmod current n n)) (carm-loop n (- current 1)))
        (else false)))

;; 1.28 TODO
;;Miller-Rabin test 


;;Section 1.3
;; 1.29

;; Simposon's Rule: a method for numerical integration 
;; integral between a and b for a function f is approximated as :

;; h = (b-a)/n for some even integer n, and yk = f(a+kh)


;; (define (sum-integers a b)
;;   (if (> a b)
;;       0
;;       (+ a (sum-integers (+ a 1) b))))

;; ;; (define (sum-cubes a b)
;; ;;   (if (> a b)
;; ;;       0
;; ;;       (+ (cube a) (sum-cubes (+ a 1) b))))

;; (define (pi-sum a b)
;;   (if (> a b)
;;       0
;;       (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;; Method for summing the values of a given series
;; a - b is the range of values to sum
;; term is a function of a that computes the current value to be summed
;; next is a fucntion of a that computes the next value of a to pass to sum
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (define y (f (+ a (* k h))))
    (if (or (= k 0) (= k n))
        (* 1 y)
        (if (even? k)
            (* 4 y)
            (* 2 y))))
  (* (/ h 3.0) (sum term 0 inc n)))

;; 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (sum-integers-iter a b)
  (sum-iter identity a inc b))

(define (sum-cubes-iter a b)
  (sum-iter cube a inc b))

;; 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial a)
  (product identity 1 inc a))

(define (pi-prod a)
  (define (pi-term x)
    (if (odd? x)
        (/ (+ x 1.0) (+ x 2.0))
        (/ (+ x 2.0) (+ x 1.0))))
  (* 4 (product pi-term 1 inc a)))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial-iter a)
  (product-iter identity 1 inc a))

;; 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (acc-prod-factorial a)
  (accumulate * 1 identity 1 inc a))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (acc-iter-prod-factorial a)
  (accumulate-iter * 1 identity 1 inc a))

;; 1.33
(define (filter-accumulate filt combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filt  a)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
  (iter a null-value))

(define (sum-even a b)
  (filter-accumulate even? + 0 identity a inc b))

(define (sum-prime-squares a b)
  (filter-accumulate prime? + 0 square a inc b))

(define (prod-rel-primes n)
  (define (valid? x)
    (gcd-one? n x))
  (filter-accumulate valid? * 1 identity 1 inc n))

(define (gcd-one? x y)
  (if (= 1 (gcd x y))
      true
      false))


;; 1.3.2 Constructing Procedures Using Lambda

;; (define (pi-sum-lambda a b)
;;   (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
;;        a
;;        (lambda (x) (+ x 4))
;;        b))


;; (define (integral-lamda f a b dx)
;;   (* (sum f
;;           (+ a (/ dx 2.0))
;;           (lambda (x) (+ x dx))
;;           b)
;;      dx))

;; ((lambda (x y z) (+ x y (square z ))) 1 2 3)

;; (define (f x y)
;;   (let ((a (+ 1 (* x y)))
;;         (b (- 1 y)))
;;     (+ (* x (square a))
;;        (* y b)
;;        (* a b))))

;; 1.34
;; (define (f g)
;;   (g 2))

;; Calling (f f) will cause an error because it will attempt to call (f 2) but f requires a procedure with an arity of 1

