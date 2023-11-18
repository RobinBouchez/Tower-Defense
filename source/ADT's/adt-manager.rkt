;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Manager ADT                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-stack-manager)
  (let ((stack '()))

    (define (top)
      (if (not (null? stack))
          (car stack)))
  
    (define (last l)
      (if (null? (cdr l))
          (car l)
          (last (cdr l))))

    (define (add! new-object)
      (set! stack (cons new-object stack)))

    (define (remove! el)
      (define (loop l e)
        (if (eq? e (car l))
            (cdr l)
            (cons (car l)(loop (cdr l) e))))
      (if (not (null? stack))
          (set! stack (loop stack el))))
  
    (define (update! delta-time)
      (define (loop lst)
        (if (not (null? lst))
            (begin
              (((car lst) 'update!) delta-time)
              (loop (cdr lst)))))
      (loop stack))
      
    (define (dispatch-manager msg)
      (cond ((eq? msg 'add!) add!)
            ((eq? msg 'remove!) remove!)
            ((eq? msg 'update!) update!)
            ((eq? msg 'clear!)(set! stack '()))
            ((eq? msg 's-list) stack)
            ((eq? msg 'length) (length stack))
            ((eq? msg 'top) (top))
            ((eq? msg 'last) (last stack))))
    dispatch-manager))