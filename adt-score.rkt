;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Score ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-score score)
 
  (define (score! new-score)
    (if (and (>= score 0)
             (>= (- score new-score) 0))
    (set! score new-score)))
  
  (define (dispatch-score msg)
    (cond ((eq? msg 'score) score)
          ((eq? msg 'score!) score!)))
  dispatch-score)
