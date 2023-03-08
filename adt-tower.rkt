;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Tower ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; maak-toren :: number, number -> punt
(define (new-tower position range damage)
  
  (define (tower-gun)
    (new-gun (new-point (position 'x)(position 'y)) range damage))
 
  (define (position! new-position)
    (set! position new-position))

  (define (range! new-range)
    (set! range new-range))

  (define (damage! new-damage)
    (set! damage new-damage))
  
  (define (update! delta-time)
    ((tower-gun) 'update!))
  
  (define (attack? enemy-x enemy-y)
    (let ((afstand
           (sqrt (+ (expt (- x enemy-x) 2) (expt (- y enemy-y) 2)))))
      (if (< afstand range)
          #t
          #f)))
  
  (define (shoot! enemy)
    (if (aanvallen? enemy)
        ((new-bullet position range damage) 'shoot!)))
  
  (define (dispatch-toren msg)
    (cond ((eq? msg 'position) position)
          ((eq? msg 'range) range)
          ((eq? msg 'damage) damage)
          ((eq? msg 'attack?) attack?)
          ((eq? msg 'update!) update!)
          ((eq? msg 'position!) position!)
          ((eq? msg 'tower-gun) tower-gun)
          ((eq? msg 'range!) range!)
          ((eq? msg 'damage!) damage!)))
  dispatch-toren)