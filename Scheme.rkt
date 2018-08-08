(define insertR
  (lambda (new old list)
    (cond
      ((null? list) list)
      ((eq? (car list) old) (cons old (cons new list)))
      (else (cons (car list) (insertR new old (cdr list))))
      )
    )
  )


(define intersect
  (lambda (l1 l2)
    (cond
      ((null? l1) l1)
      ((intersectHelper (car l1) l2) (cons (car l1) (intersect (cdr l1) l2)))
      (else (intersect (cdr l1) l2))
      )
    )
  )


(define intersectHelper
  (lambda (ele l)
    (cond
      ((null? l) #f)
      ((eq? ele (car l)) #t)
      (else (intersectHelper ele (cdr l)))
      )
    )
  )