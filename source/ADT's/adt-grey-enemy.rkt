;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Grey Enemy ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-grey-enemy enemy)
  (let ((health 3)
        (speed-change? #t)
        (upper-random-number 250)
        (lower-random-number 1)
        (guess 50)
        (min-speed-change 95)
        (max-speed-change 125))

    (define (health! new-health)
      (set! health new-health))
      
    (define (update! delta-time)
      (if (= guess (generate-random-number lower-random-number upper-random-number))
          ((this 'speed!) (* (this 'speed) (/ (generate-random-number min-speed-change max-speed-change) 100)))))
     
    (define (this msg)
      (cond ((eq? msg 'health) health)
            ((eq? msg 'health!) health!)
            ((eq? msg 'update!) update!)
            (else (enemy msg))))
    this))



