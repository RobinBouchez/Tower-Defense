;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Bomber Tower ADT                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-bomber-tower tower)
  (let ((range 2)
        (damage 3)
        (cooldown 4000)
        (bullet-velocity ((tower 'bullet) 'velocity))
        (rotated? #t))
    
    (define (rotated! new-bool)
      (set! rotated? new-bool))
    
    (define (rotate!)
      (((tower 'gun-tile) 'rotate-clockwise!))
      (if rotated?
          (let ((tower-angle (tower 'angle)))
            (set! bullet-velocity (new-vector2 (* (bullet-velocity 'y) 1)  (* (bullet-velocity 'x) -1)))
            ((tower 'angle!) (modulo (+ tower-angle 90) 360))
            (rotated! #f))))
  
    (define (this msg)
      (cond ((eq? msg 'range) range)
            ((eq? msg 'damage) damage)
            ((eq? msg 'bullet-velocity) bullet-velocity)
            ((eq? msg 'rotated?) rotated?)
            ((eq? msg 'rotated!) rotated!)
            ((eq? msg 'cooldown) cooldown)
            ((eq? msg 'rotate!) rotate!)
            (else (tower msg))))
    this))