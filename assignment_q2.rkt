#lang racket

;This is an example implementation of ins_beg,
;It obviously doesn't do what it should, so you
;can edit this function to get started.
;
;Please note the provide function is necessary
;for the unit tests to work. Please include a
;(provide) for each function you write in your
;submitted assignment.
;
;You may delete these comments!

;(provide ins_beg)
;(provide ins_end)
;(provide count_top_level)
;(provide count_instances)
;(provide tail_count_instances)
(provide count_instances_deep)

(define (ins_beg el lst)   
 (cons el lst))

(define (ins_end el lst)
  (append lst (list el )))

(define (count_top_level lst)
  (if (null? lst)
      0
  (+ 1 (count_top_level(cdr lst)))))

(define (count_instances el lst)
  (cond [(null? lst)0]
        [ (equal? (car lst) el)
          ( + 1 (count_instances el (cdr lst)))]
        [(not (equal? (car lst) el))
         (+ 0 (count_instances el (cdr lst)))]))

(define (tcount el lst)
  (tail_count_instances el lst 0))
(define (tail_count_instances el lst running_total)
  (cond [(null? lst) running_total]
        [(not (equal? (car lst) el))  (tail_count_instances el (cdr lst) running_total)]
        [else (tail_count_instances el (cdr lst) (+ 1 running_total))]))
        


(define (tcount_deep el lst)
  (count_instances_deep el lst 0))
(define (count_instances_deep el lst running_total)
  (cond [(null? lst) running_total]       
        [(list? (car lst))
         (count_instances_deep el (car lst) (count_instances_deep el (cdr lst) running_total)) ]
        [ (equal? (car lst) el)
          (count_instances_deep el (cdr lst) (+ 1 running_total))]
        [(not (equal? (car lst) el))
         (count_instances_deep el (cdr lst) running_total)]))    
  
        
  

(ins_beg '(2 3) (list 3 4));         
(ins_end 7 (list 2 4)  );
(count_top_level '( 2 3 '(5 6) 4));
(count_instances 4 '(5 3 4 2 ))
(tcount 4 '(4 3 4 4 2))
(tcount_deep 4 '( 4 3 ( 1 (4 2 4 4))  3  4 4(4 2)))
 