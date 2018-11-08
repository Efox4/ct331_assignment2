#lang racket
(provide node-left)
(define tree '(((()1())3(()6())) 8 (()10 ())))


(define (create_tree left root right)
  (list left root right))

(define(node-left tree)
   (car tree))    
        
   
(define(node-right tree)
   (caddr tree))         
  
(define (node-root tree)
   (cadr tree))
         
        


(define (in_order_traverse tree)
 (if (null? tree)
     '()
     (append (in_order_traverse (node-left tree))
             (list (node-root tree))             
             (in_order_traverse (node-right tree)))))

(define (present_in el tree)
  (cond [(member el (in_order_traverse tree)) #t ]
        [(not(member el (in_order_traverse tree))) #f]))

(define (insert el tree)
    (cond [(null? tree) (create_tree '() el '())]
        [(= el (node-root tree))  tree]
        [(> el (node-root tree))
         (list  (node-left tree) (node-root tree) (insert el (node-right tree)))]
        [(< el (node-root tree))
         (list  (insert el (node-left tree))  (node-root tree) (node-right tree))]))

(define (insert_list lst tree)
  (cond ((null? lst) tree)
        (else (insert_list (cdr lst)
                           (insert (car lst)tree)))))

(define (tree_sort lst)
   (if (null? lst) '()          
         (in_order_traverse (insert_list lst '()))))
;;higher tree sort with descending inserts, descending when num is anything but 1
(define (higher_tree_sort lst num)
  (if (= num 1) (tree_sort lst)
      (descending_tree_sort lst)))

(define (insert_descending_list lst tree)
  (cond ((null? lst) tree)
        (else (insert_descending_list (cdr lst)
                           (descending_insert (car lst)tree)))))

(define (descending_tree_sort lst)
  (if (null? lst) '()          
         (in_order_traverse (insert_descending_list lst '()))))

(define (descending_insert el tree)
   (cond [(null? tree) (create_tree '() el '())]
        [(= el (node-root tree))  tree]
        [(< el (node-root tree))
         (list  (node-left tree) (node-root tree) (descending_insert el (node-right tree)))]
        [(> el (node-root tree))
         (list  (descending_insert el (node-left tree))  (node-root tree) (node-right tree))]))

                    
  
(node-left (node-left tree))
(node-right '(3 (6(empty 6)) 7)) 
(node-root tree)
(in_order_traverse '((()1()) 4 (()6 ())))
(in_order_traverse tree)
(present_in 5 tree)
(insert_descending_list '(7 10 9) '(() 8 ()))
(in_order_traverse(insert 2 (insert 9 tree)))
(higher_tree_sort '(1 2 5 4 3) 0)
