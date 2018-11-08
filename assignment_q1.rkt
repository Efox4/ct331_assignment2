#lang racket
>(cons 1 2);
>(cons 3(cons 1 (cons 2 empty)));
>(cons "a string"(cons 2(cons (list 4 5 6) empty)));
>(list "another string" 2 (list 5 6 7));
>(append (list "last string") (list 4) (list '(8 5 6)));