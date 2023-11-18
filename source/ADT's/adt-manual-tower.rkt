;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Manual Tower ADT                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-manual-tower tower)
  (let* ((range 3)
         (damage 1)
         (cooldown 3000)
         (bullet-speed 0.05)
         (bullet-velocity (new-vector2 0 (* -1 bullet-speed)))
         (can-shoot? #t))

    (define (can-shoot! bool)
      (set! can-shoot? bool))

    (define (rotate! . pos)
      (define (adjust-to angle offset)
        (modulo (inexact->exact (round (+ (* -1 angle) offset))) 360))
      
      (let* ((angle (calculate-angle (this 'position) (caar pos)))
             (adjusted-angle (adjust-to angle 90))
             (velocity-angle (adjust-to angle 180)))
        (((tower 'gun-tile) 'rotate!) adjusted-angle)
        (when can-shoot?
          (set! bullet-velocity (new-vector2 (* 0.1  (cos (* velocity-angle (/ pi 180))))
                                             (* -0.1 (sin (* velocity-angle (/ pi 180))))))
          ((tower 'angle!) (+ adjusted-angle 180))
          ((tower 'shoot!))
          ((tower 'has-shot!) #t)
          (set! can-shoot? #f))))
  
    (define (this msg)
      (cond ((eq? msg 'range) range)
            ((eq? msg 'damage) damage)
            ((eq? msg 'cooldown) cooldown)
            ((eq? msg 'bullet-velocity) bullet-velocity)
            ((eq? msg 'can-shoot!) can-shoot!)
            ((eq? msg 'rotate!) rotate!)
            (else (tower msg))))
    this))