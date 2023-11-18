;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Health ADT                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-health position)
  (let ((lives 3))

    (define (lose-life!)
      (set! lives (- lives 1)))
        
    (define (dispatch-health msg)
      (cond ((eq? msg 'position) position)
            ((eq? msg 'lives) lives)
            ((eq? msg 'lose-life!)(lose-life!))))
    dispatch-health))
