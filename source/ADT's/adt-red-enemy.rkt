;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Red Enemy ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-red-enemy enemy)
  (let ((health 1))

    (define (health! new-health)
      (set! health new-health))
      
    (define (this msg)
      (cond ((eq? msg 'health) health)
            ((eq? msg 'health!) health!)
            (else (enemy msg))))
    this))



