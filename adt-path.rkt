;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Pad ADT                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-path start-position end-position)
  (define path-list '())
    
    (define (add-point position)
      (set! path-list (cons position path-list)))
    
    (define (path-start-pos) start-position)        

    (define (dispatch msg)
      (cond ((eq? msg 'add-point) add-point)
            ((eq? msg 'start-pos) path-start-pos)
            ((eq? msg 'path-list) path-list)))
    dispatch)
