;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Game ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-game)
  (let* ((level-idx 1)
         (draw (new-draw))
         (level (new-level level-idx))
         (health (new-health (new-point health-position-x health-position-y)))
         (start-game? #f)       
         (enemy-amount 0)
         (enemy-type 'red)
         (has-enemy-spawned? #f)
         (timer 0)
         (cooldown 1000))

    (define (start!)
      ((draw 'set-update-callback!) update!) 
      ((draw 'set-key-callback!) set-key-callback!)
      ((draw 'set-mouse-click-callback!) mouse-click-procedure)
      (draw 'start!)(spawn-new-level!))

    (define (restart!)
      (draw 'empty!)
      (set! level-idx 1)
      (set! level (new-level level-idx))
      (spawn-new-level!)
      (set! health (new-health (new-point health-position-x health-position-y)))
      (start!))

    (define lost-life? #f)

    (define (update! delta-time)
      ((draw 'draw-game!) dispatch-game)
      (if start-game?
          (begin
            (enemy-spawner delta-time)
            (if (level 'grey-enemy-died?)
                (begin
                  (set-enemy-spawner! 'red 2)
                  ((level 'grey-enemy-died!) #f)))
            (if (level 'enemy-reached-end?)
                (if lost-life?
                    (begin
                      (health 'lose-life!)
                      (set! lost-life? #f))))
            (if (not (= (health 'lives) 0))
                ((level 'update!) delta-time)
                (restart!)))))
    
    (define (set-key-callback! state key)
      (cond ((and (eq? state 'pressed)(eq? key 'down))
             (restart!))
            ((and (eq? state 'pressed)(eq? key 'up)(not start-game?))
             (begin (set! start-game? #t)
                    (((level 'wave-adt) 'create!) (draw 'foreground-layer))))
            ((and (eq? state 'pressed)(eq? key 'right) start-game?  (= (length ((level 'enemies) 's-list)) 0))
             (begin
               ((level 'wave-adt) 'increment!)
               (update-round! ((level 'wave-adt) 'wave))
               (set! lost-life? #t)))
            ((and (eq? state 'pressed) start-game?)
             ((level 'key-press!) key))))
    
    
    (define (mouse-click-procedure btn event x y)
      (cond ((and (eq? event 'pressed) start-game?)
             (let ((x-cel (quotient x tile-width-px))
                   (y-cel (quotient y tile-height-px)))
               ((level 'mouse-click!) draw btn x-cel y-cel)))))

    (define (update-round! wave)
      (cond ((= wave 1)
             (begin
               (spawn-new-wave)
               (set-enemy-spawner! 'red (* 2 level-idx))))
            ((= wave 2)
             (begin
               (spawn-new-wave)
               (set-enemy-spawner! 'red (* 2 level-idx))
               (set-enemy-spawner! 'blue (* 2 level-idx))))
            ((= wave 3)
             (begin
               (spawn-new-wave)
               (set-enemy-spawner! 'red (* 2 level-idx))
               (set-enemy-spawner! 'grey (* 2 level-idx))))
            ((= wave 4)
             (begin
               (spawn-new-wave)
               (set-enemy-spawner! 'red (* 2 level-idx))
               (set-enemy-spawner! 'blue (* 2 level-idx))
               (set-enemy-spawner! 'grey (* 2 level-idx))))
            ((> wave 4)
             (begin
               (((draw 'background-layer) 'empty!))
               (((draw 'tower-layer) 'empty!))
               (((draw 'gun-layer) 'empty!))
               (set! level-idx (+ level-idx 1))
               ((level 'wave-adt) 'reset!)
               (spawn-new-wave)
               (set! level (new-level level-idx))
               (spawn-new-level)))))

    (define (spawn-new-wave)
      ((((level 'menu) 'money) 'add-amount!) wave-bonus)
      (((level 'wave-adt) 'create!) (draw 'foreground-layer)))

    (define (spawn-new-level!)
      ((draw 'draw-level!) level-idx))
    
    (define enemy-spawner-queue '())
    (define (set-enemy-spawner! type amount)
      (if (= enemy-amount 0)
          (begin  
            (set! enemy-amount amount)
            (set! enemy-type type))
          (set! enemy-spawner-queue (cons (cons type amount) enemy-spawner-queue))))

    (define (enemy-spawner delta-time)    
      (if (not (= enemy-amount 0))
          (if has-enemy-spawned?
              (begin
                (set! timer (+ timer delta-time))
                (if (>= timer cooldown)
                    (begin
                      (set! timer 0)
                      (set! has-enemy-spawned? #f))))
              (begin   ((level 'create-enemy!) enemy-type (draw 'enemy-layer))
                       (set! has-enemy-spawned? #t)
                       (set! enemy-amount  (- enemy-amount 1))))
          (if (not (null? enemy-spawner-queue))
              (begin (set-enemy-spawner! (car (car enemy-spawner-queue))(cdr (car enemy-spawner-queue)))
                     (set! enemy-spawner-queue (cdr enemy-spawner-queue))))))
    
 
    (define (dispatch-game msg)
      (cond ((eq? msg 'start!) start!)
            ((eq? msg 'level) level)
            ((eq? msg 'health) health)
            ((eq? msg 'start-game?) start-game?)))
    dispatch-game))
