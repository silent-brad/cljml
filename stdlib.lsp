(fn o (f g) (lambda (x) (f (g x))))
(val caar (o car car))
(val cadr (o car cdr))
(val caddr (o cadr cdr))
(val cadar (o car (o cdr car)))
(val caddar (o car (o cdr (o cdr car))))

(val cons pair)

(val newline (itoc 10))
(val space (itoc 32))

(fn println (s)
  (let [ok (print s)]
    (print newline)))

(macro when (cond body)
  `(if ,cond ,body))

(macro unless (cond body)
  `(if ,cond nil ,body))

(fn getline ()
  (let [ic (getchar)
        c (itoc ic)]
    (if (or (eq c newline) (eq ic ~1))
      empty-symbol
      (cat c (getline)))))

(fn null? (xs)
  (eq xs '()))

(fn length (ls)
  (if (null? ls)
    0
    (+ 1 (length (cdr ls)))))

(fn take (n ls)
  (if (or (< n 1) (null? ls))
    '()
    (cons (car ls) (take (- n 1) (cdr ls)))))

(fn drop (n ls)
  (if (or (< n 1) (null? ls))
    ls
    (drop (- n 1) (cdr ls))))

(fn merge (xs ys)
  (if (null? xs)
    ys
    (if (null? ys)
      xs
      (if (< (car xs) (car ys))
        (cons (car xs) (merge (cdr xs) ys))
        (cons (car ys) (merge xs (cdr ys)))))))

(fn mergesort (ls)
  (if (null? ls)
    ls
    (if (null? (cdr ls))
      ls
      (let [size (length ls)
            half (/ size 2)
            first (take half ls)
            second (drop half ls)]
        (merge (mergesort first) (mergesort second))))))
