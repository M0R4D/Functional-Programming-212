#lang racket
;; F-numerals are a perverse variation on Church numerals
;; they defined as: "actually they are functions and applications not numbers" 
;; 0 : c
;; 1 : (a(a(b c)))
;; 2 : (a(a(b (a(a(b c))) )))
;; 3 : (a(a(b (a(a(b (a(a(b c))) ))) )))
;; n : (a(a(b (a(a(b  .... (a(a(b c))) ))) ))) - n times (a(a(b ...)))
;; fn: λaλbλc.(a(a(b [..n-times.. (a(a(b] c))) )))
;; etc..


;; f0 defined as: λabc.c (λaλbλc.c)
;; this is our zero or "zoro" (like munos in the lecture :) )
(define f0
  (lambda (a)
    (lambda (b)
      (lambda (c)
        c))))


;; f1 is the successor of f0 and it is defined as: λabc.(a(a(b c)))
;; we did not use it here in our solution because we didn't need it, we just use it to make it clear for us what we were required to do 
(define f1
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (a (a (b c)))))))


;; integer->f: returns the F-numeral of an Integer (or Natural Number)
(define integer->f
  (lambda (n)
    (if (zero? n)
        f0
        (fs+ (integer->f (- n 1))))))

;; f->integer: gets an F-numerals and returns the Natural Number that represent it
(define f->integer
  (lambda (fn)
    (((fn (lambda (n) n)) (lambda (n) (+ n 1))) 0)))


;; fs+: the Successor function, gets an F-numeral and returns it's successor (in another simple words: apply it on (a(a(b ...))) )
;; λnabc.(a(a(b(((n a)b)c)))) ;; where n is f-numeral
;; f0 to f1, f5 to f6, fn to fn+1
(define fs+
  (lambda (fn)
    (lambda (a)
      (lambda (b)
        (lambda (c)
          (a (a (b (((fn a) b) c)))))))))


;; fdouble: doubled the F-numerl
;; λnabc.(((n a)b)(((n a)b)c)) ;; where n is f-numeral
;; f0 to f0, f5 to f10, fn to f2*n
(define fdouble
  (lambda (fn)
    (lambda (a)
      (lambda (b)
        (lambda (c)
          (((fn a) b) (((fn a) b) c)))))))


;; ftriplee: tripled the F-numerl
;; λnabc.(((n a)b)(((fn a)b)(((n a)b)c))) ;; where n is f-numeral
;; f0 to f0, f5 to f15, fn to f3*n
(define ftriple
  (lambda (fn)
    (lambda (a)
      (lambda (b)
        (lambda (c)
          (((fn a) b) (((fn a) b) (((fn a) b) c))))))))


;; f+: Plus Function - adding two F-numerals and returns the result
;; λnmabc.(((n a) b)(((m a)b)c)) ;; where n,m are f-numerals
(define f+
  (lambda (fn)
    (lambda (fm)
      (lambda (a)
        (lambda (b)
          (lambda (c)
            (((fn a) b) (((fm a) b) c))))))))


;; f*: Multiply Function - multiply two F-numerals and returns the result
;; λnmabc.((((n I)((m a)b))c) ;; where n,m are f-numerals and I is the Identity function
(define f*
  (lambda (fn)
    (lambda (fm)
      (lambda (a)
        (lambda (b)
          (lambda (c)
            (((fn (lambda (i) i)) ((fm a) b)) c) ))))))


;; f^: Power Function - gets two F-numerals fn, fm and returns the result of fn^fm
;; to be honest, this function makes a lot of problems to us, and until the last minute we did not success how to implement it properly
;; λnmabc.-------------  :((  ;; where n,m are f-numerals
(define f^
  (lambda (fn)
    (lambda (fm)
      (lambda (a)
        (lambda (b)
          (lambda (c)
            ((((fm a)b)c) (((fn a)b)c))))))))



;; show: the most challenged part of the assignment, show function
;; this function returns the concrete syntax for the source-code of the numeral.
;; gets a F-numeral - fn , and returns:  λaλbλc.(a(a(b ..n-times then.. c)))
;; we implement it based on this course licturer Prof. Mayer Goldberg implementation in his office hours
(define show
  (lambda (fn)
    (let* ((pair
            (lambda (a)
              (lambda (b)
                (lambda (x)
                  ((x a) b)))))
           (M (lambda (m)
                (lambda (b)
                  (lambda (a)
                    ((pair m) `(,a ,b)))))))
      ((((fn
          ((pair M) 'a))
         ((pair M) 'b))
        ((pair M) 'c))
       (lambda (_whatEver)
         (lambda (body)
           `(lambda (a)
              (lambda (b)
                (lambda (c)
                  ,body)))))))))



(define f2 (fs+ f1 ))
(define f3 (fs+ f2 ))
(define f4 (fs+ f3 ))



;; ----------------------------------------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------------------------------------------------------
;; Tests Here:
;; for each function, we will write many tests to ensure that it works properly, including: Extreme Cases and absolutely Random Cases
;; and on our way we will test f->integer function with all others (plus, multiply, double ...)


> ( show f0)
    (lambda (a) (lambda (b) (lambda (c) c)))
> ( show f1)
    (lambda (a) (lambda (b) (lambda (c) (a (a (b c))))))
> ( show f2)
    (lambda (a) (lambda (b) (lambda (c) (a (a (b c))))))
> ( show f3)
    (lambda (a) (lambda (b) (lambda (c) (a (a (b (a (a (b (a (a (b c))))))))))))
> ( show f4)
    (lambda (a) (lambda (b) (lambda (c) (a (a (b (a (a (b (a (a (b (a (a (b c)))))))))))))))


> ( f->integer ( integer->f  12345))
    12345
> ( f->integer ( integer->f  3456))
    3456


;; Extreme Cases: zero
;; Random Cases: we choose 1, 4, 5, 10, 16
> ( show ( fs+ f0 ))
    (lambda (a) (lambda (b) (lambda (c) (a (a (b c))))))
> ( show ( fs+ f1 ))
    (lambda (a) (lambda (b) (lambda (c) (a (a (b (a (a (b c)))))))))
> ( show ( fs+ (integer->f 5) ))
    (lambda (a) (lambda (b) (lambda (c) (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b c)))))))))))))))))))))
> ( show ( fs+ (integer->f 10) ))
    (lambda (a)
      (lambda (b)
        (lambda (c)
          (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b c))))))))))))))))))))))))))))))))))))
> ( f->integer (fs+ f4))
    5
> ( f->integer (fs+ (integer->f 16)))
    17


;; Extreme Cases: zero, one are the closest to make a problems for us
;; Random Cases: we choose 2, 3, 14, 19
> ( show ( fdouble f0 ))
    (lambda (a) (lambda (b) (lambda (c) c)))
> ( show ( fdouble f1 ))
    (lambda (a) (lambda (b) (lambda (c) (a (a (b (a (a (b c)))))))))
> ( show ( fdouble f3 ))
    (lambda (a) (lambda (b) (lambda (c) (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b c)))))))))))))))))))))
> ( show ( fdouble (integer->f 14) ))
    (lambda (a)
      (lambda (b)
        (lambda (c)
          (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b
             (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b
                (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b c)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    
> ( f->integer ( fdouble (integer->f 2) ))
    4
> ( f->integer ( fdouble (integer->f 134239) ))
    268478


;; Extreme Cases: zero, one are the closest to make a problems also here
;; Random Cases: we choose 4, 25
> ( show ( ftriple f0 ))
    (lambda (a) (lambda (b) (lambda (c) c)))
> ( show ( ftriple f1 ))
    (lambda (a) (lambda (b) (lambda (c) (a (a (b (a (a (b (a (a (b c))))))))))))
> ( show ( ftriple f4 ))
    (lambda (a)
      (lambda (b)
        (lambda (c)
          (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b c)))))))))))))))))))))))))))))))))))))))

> ( show ( ftriple (integer->f 25)))
    (lambda (a)
      (lambda (b)
        (lambda (c)
          (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b
          (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b
          (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b
          (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b
          (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b  c)
           ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                             )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

> ( f->integer ( ftriple (integer->f 100) ))
    300
> ( f->integer ( ftriple (integer->f 302) ))
    906
    

;; Extreme Cases: zero, one are the closest to make a problems also here, we test every combination of them including cases when there is just one of them
;; Random Cases: we choose 7, 15, 886+2637 ...
> ( show ((f+ f0) f0 )) ;; 0 + 0
    (lambda (a) (lambda (b) (lambda (c) c)))
> ( show ((f+ f0) f1 )) ;; 0 + 1
    (lambda (a) (lambda (b) (lambda (c) (a (a (b c))))))
> ( show ((f+ f1) f0 )) ;; 1 + 0
    (lambda (a) (lambda (b) (lambda (c) (a (a (b c))))))
> ( show ((f+ f1) f1 )) ;; 1 + 1
    (lambda (a) (lambda (b) (lambda (c) (a (a (b (a (a (b c)))))))))
> ( show ((f+ (integer->f 15)) f0 )) ;; random + 0
    (lambda (a)
      (lambda (b)
        (lambda (c)
          (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b
          (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b  c))))))))))))))))))))))))))))))))))))))))))))))))
> ( show ((f+ f0) (integer->f 15) )) ;; 0 + random
    (lambda (a)
      (lambda (b)
        (lambda (c)
          (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b
          (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b  c))))))))))))))))))))))))))))))))))))))))))))))))
> ( show ((f+ (integer->f 7)) f1 ))  ;; random + 1
    (lambda (a) (lambda (b) (lambda (c) (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b c)))))))))))))))))))))))))))
> ( show ((f+ f1) (integer->f 7) ))  ;; 1 + random
    (lambda (a) (lambda (b) (lambda (c) (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b c)))))))))))))))))))))))))))
> ( show ((f+ f4) f3 )) ;; random + random
    (lambda (a) (lambda (b) (lambda (c) (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b c))))))))))))))))))))))))
> ( f->integer ((f+ f4) f3 )) ;; expected 7
    7
> ( f->integer ((f+ (integer->f 886)) (integer->f 2637) )) ;; expected 3523
    3523

;; Extreme Cases: zero, one are the closest to make a problems also here, we test every combination of them including cases when there is just one of them
;; Random Cases: we choose 39, 48, 9*3, 53*756
> ( show ((f* f0) f0 )) ;; 0 * 0
    (lambda (a) (lambda (b) (lambda (c) c)))
> ( show ((f* f0) f1 )) ;; 0 * 1
    (lambda (a) (lambda (b) (lambda (c) c)))
> ( show ((f* f1) f0 )) ;; 1 * 0
    (lambda (a) (lambda (b) (lambda (c) c)))
> ( show ((f* f1) f1 )) ;; 1 * 1
    (lambda (a) (lambda (b) (lambda (c) (a (a (b c))))))
> ( show ((f* (integer->f 39)) f0 )) ;; random * 0
    (lambda (a) (lambda (b) (lambda (c) c)))
> ( show ((f* f0) (integer->f 39) )) ;; 0 * random
    (lambda (a) (lambda (b) (lambda (c) c)))
> ( show ((f* (integer->f 48)) f1 ))  ;; random * 1
    (lambda (a)
      (lambda (b)
        (lambda (c)
          (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b
             (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b
                (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b
                   (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a
                      (b  c)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
> ( show ((f* f1) (integer->f 48) ))  ;; 1 * random
    (lambda (a)
      (lambda (b)
        (lambda (c)
          (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b
             (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b
                (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b
                   (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a
                      (b  c)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
> ( show ((f* f4) f3 )) ;; random * random
    (lambda (a)
      (lambda (b)
          (lambda (c)
                  (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b (a (a (b  c)))))))))))))))))))))))))))))))))))))))

> ( f->integer ((f* (integer->f 9)) f3 )) ;; expected 27
    27
> ( f->integer ((f* (integer->f 53)) (integer->f 756) )) ;; expected 40068
    40068


;; ( show ((f^ f2) f3 ))
;; Extreme Cases: zero, one are the closest to make a problems also here, we test every combination of them including cases when there is just one of them
;; Random Cases: we choose 39, 48, 9^3, 53^4
;; ( show ((f^ f0) f0 )) ;; 0 ^ 0
;; ( show ((f^ f0) f1 )) ;; 0 ^ 1
;; ( show ((f^ f1) f0 )) ;; 1 ^ 0
;; ( show ((f^ f1) f1 )) ;; 1 ^ 1
;; ( show ((f^ (integer->f 39)) f0 )) ;; random ^ 0
;; ( show ((f^ f0) (integer->f 39) )) ;; 0 ^ random
;; ( show ((f^ (integer->f 48)) f1 ))  ;; random ^ 1
;; ( show ((f^ f1) (integer->f 48) ))  ;; 1 ^ random
;; ( show ((f^ f4) f3 )) ;; random ^ random
;; ( f->integer ((f^ (integer->f 9)) f3 )) ;; expected 729
;; ( f->integer ((f^ (integer->f 53)) (integer->f 4) )) ;; expected 7890481
