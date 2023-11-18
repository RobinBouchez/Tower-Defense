;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Money ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-money amount)
    (define (add-amount! x)
      (define new-amount (+ amount x))
      (if (>= new-amount 0)
            (set! amount new-amount)))
    
    (define (dispatch-money msg)
      (cond ((eq? msg 'amount) amount)
            ((eq? msg 'add-amount!) add-amount!)))
    dispatch-money)
