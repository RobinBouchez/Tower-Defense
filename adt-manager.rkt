;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Manager ADT                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-manager)
  (define game-object-list '())

  (define max-length 2)
  
  (define (set-max-length len)
    (set! max-length len))

  (define (first)
    (if (not (null? game-object-list))
    (car game-object-list)))
  
  (define (last l)
    (if (null? (cdr l))
        (car l)
        (last (cdr l))))

  (define (add new-object)
    (if (<= (length game-object-list) max-length)
    (set! game-object-list (cons new-object game-object-list))))

  (define (remove!)
    (if (> (length game-object-list) 1)
    (set-cdr! (cdr game-object-list))))
  
  (define (draw!)
    (define (loop lst)
      (if (not (null? lst))
          (loop ((car lst) 'draw) (cdr lst))))
    (loop game-object-list))

  (define (update! delta-time)
    (define (loop lst)
      (if (not (null? lst))
          (begin
            (((car lst) 'update!) delta-time)
            (loop (cdr lst)))))
    (loop game-object-list))

      
  (define (dispatch-manager msg)
    (cond ((eq? msg 'add) add)
          ((eq? msg 'remove!) remove!)
          ((eq? msg 'update!) update!)
          ((eq? msg 'draw!) draw!)
          ((eq? msg 'list) game-object-list)
          ((eq? msg 'length) (length game-object-list))
          ((eq? msg 'set-max-length) set-max-length)
          ((eq? msg 'max-length) max-length)
          ((eq? msg 'first) (first))
          ((eq? msg 'last) (last game-object-list))))
  dispatch-manager)