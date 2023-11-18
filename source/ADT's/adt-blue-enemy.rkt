;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Blue Enemy ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-blue-enemy enemy)
  (let ((health 3)
        (speed-change? #t)
        (slow-speed 0.04)
        (normal-speed 0.8))

    (define (initialize!)
      ((enemy 'speed!) normal-speed))
    
    (define (update! delta-time)
      (when (and (= health 1) speed-change?)
        ((enemy 'speed!) slow-speed)
        (set! speed-change? #f)))

    (define (health! new-health)
      (set! health new-health))
    
    (initialize!)
     
    (define (this msg)
      (cond ((eq? msg 'health) health)
            ((eq? msg 'health!) health!)
            ((eq? msg 'update!) update!)
            (else (enemy msg))))
    this))



