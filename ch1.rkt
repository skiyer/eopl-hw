#lang eopl
; ********************
; *** Exercise 1.1 ***
; ********************
;
; ---------------------------------------------
; 1) { 3n + 2 | n in N}
;
; top-down:
;   A natural number n is in S if and only if either
;     1. n = 2, or
;     2. n - 3 in S.
;
; bottom-up:
;   S is the smallest set of number satisfying the following properties:
;     1. 2 in S, and
;     2. if n in S, then n + 3 in S.
;
; rule of inference:
;                 n in S
;   ------      ------------
;   2 in S       n + 3 in S
;
; ---------------------------------------------
; 2) {2n + 3m + 1 | n, m in N}
;
; top-down:
;   A natural number n is in S if and only if either
;     1. n = 1, or
;     2. n - 2 in S, or
;     3. n - 3 in S.
;
; bottom-up:
;   S is the smallest set of number satisfying the following properties:
;     1. 1 in S, and
;     2. if n in S, then n + 2 in S, and
;     3. if n in S, then n + 3 in S.
;
; rule of inference:
;                 n in S          n in S
;   ------      ------------    -----------
;   1 in S       n + 2 in S      n + 3 in S
;
; -------------------------------------------------
; 3) { (n, 2n + 1) | n in N}
;
; top-down:
;   A pair of natural number (n, m) is in S if and only if either
;     1. n = 0, m = 1 or
;     2. (n - 1, m - 2) in S.
;
; bottom-up:
;   S is the smallest set of number satisfying the following properties:
;     1. (0, 1) in S, and
;     2. if (n, m) in S, then (n + 1, m + 2) in S.
;
; rule of inference:
;                         (n, m) in S
;   -----------      ---------------------
;   (0, 1) in S       (n + 1, m + 2) in S
;
; -------------------------------------------------
; 4) { (n, n^2 ) | n in N}
;
; top-down:
;   A pair of natural number (n, m) is in S if and only if either
;     1. n = 0, m = 0 or
;     2. (n - 1, m - 2n + 1) in S.
;
; bottom-up:
;   S is the smallest set of number satisfying the following properties:
;     1. (0, 0) in S, and
;     2. if (n, m) in S, then (n + 1, m + 2n + 1) in S.
;
; rule of inference:
;                           (n, m) in S
;   -----------      --------------------------
;   (0, 0) in S       (n + 1, m + 2n + 1) in S
;
; -------------------------------------------------
;
; ********************
; *** Exercise 1.2 ***
; ********************
;
; -------------------------------------------------
; 1)                     (n, k) in S
;     (0, 1) in S     ----------------------
;                      (n + 1, k + 7) in S
;
; Ans:  { (n, 7n + 1) | n in N}
;
; -------------------------------------------------
; 2)                     (n, k) in S
;     (0, 1) in S     -------------------
;                      (n + 1, 2k) in S
;
; Ans:  { (n, 2^n) | n in N}
;
; -------------------------------------------------
; 3)                       (n, i, j) in S
;     (0, 0, 1) in S   ------------------------
;                       (n + 1, j, i + j) in S
;
; Ans:  { (n, Fib(n), Fib(n + 1)) | n in N}
;
; -------------------------------------------------
; 4)                         (n, i, j) in S
;     (0, 1, 0) in S   -----------------------------
;                       (n + 1, i + 2, i + j) in S
;
; Ans:  { (n, 2n + 1, 2^n) | n in N}
;
; -------------------------------------------------
;
; ********************
; *** Exercise 1.3 ***
; ********************
;
; Ans: T = N
;
; ********************
; *** Exercise 1.4 ***
; ********************
;
; Ans:
;      List-of-Int
;   => (Int . List-of-Int)
;   => (-7 . (Int . List-of-Int))
;   => (-7 . (3 . (Int . List-of-Int)))
;   => (-7 . (3 . (14. List-of-Int))))
;   => (-7 . (3 . (14. ())))
;
; ********************
; *** Exercise 1.5 ***
; ********************
;
; Proof:
;   IH(k) is that any e in LcExp whose number of left parentheses <= k
; has the same number of right parentheses.
;   1. When k = 0, e = Identifier, so IH(0) holds.
;   2. When k > 0, IH(k) holds. If e have <= k + 1 left parentheses:
;     a) e is of the form (lambda (Identifier) e1), then e1 must have <= k - 1
;   left parentheses. If e1 has t left parentheses, e1 must have t right
;   parentheses. Therefore e has t + 2 left parentheses and t + 2 right
;   parentheses.
;     b) e is of the form (e1, e2), then e1 and e2 must have <= k left
;   parentheses. If e1 has t1 left parentheses, e2 has t2 left parentheses,
;   then e1 must have t1 right parentheses and e2 must have t2 right
;   parentheses. Therefore e has t1 + t2 + 1 left parentheses and t1 + t2 + 1
;   right parentheses.
;     So IH(k + 1) holds and therefore the induction completes.
;
; ********************
; *** Exercise 1.6 ***
; ********************
;
; Ans: The program can not terminate.
;
; ********************
; *** Exercise 1.7 ***
; ********************
;
(define nth-element-helper
  (lambda (lst n)
    (if (null? lst)
        #f
        (if (zero? n)
            (car lst)
            (nth-element-helper (cdr lst) (- n 1))))))

(define nth-element
  (lambda (lst n)
    (let ((r (nth-element-helper lst n)))
      (if r
          r
          (eopl:error 'my-nth-element
                      "~s does not have ~s elements.~%" lst (+ n 1))))))
;
; ********************
; *** Exercise 1.8 ***
; ********************
;
; Ans: drop-until-first
;
; ********************
; *** Exercise 1.9 ***
; ********************
;
(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (remove s (cdr los))
            (cons (car los) (remove s (cdr los)))))))
;
; ********************
; *** Exercise 1.10 ***
; ********************
;
; Ans: "exclusive or"
;
; ********************
; *** Exercise 1.11 ***
; ********************
;
; Ans: because the sexp in subst is getting smaller and will finally
; end up with null.
;
; ********************
; *** Exercise 1.12 ***
; ********************
;
(define subst-inline
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
         (let ((sexp (car slist)))
           (if (symbol? sexp)
               (if (eqv? sexp old) new sexp)
               (subst-inline new old sexp)))
         (subst-inline new old (cdr slist))))))
;
; ********************
; *** Exercise 1.13 ***
; ********************
;
(define subst-map
  (lambda (new old slist)
    (let ((subst-in-s-exp
           (lambda (sexp)
             (if (symbol? sexp)
               (if (eqv? sexp old) new sexp)
               (subst-map new old sexp)))))
      (map subst-in-s-exp slist))))
;
; ********************
; *** Exercise 1.14 ***
; ********************
;
