;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Level ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-level level-index)
  (let* ((enemies (new-stack-manager))
         (towers (new-stack-manager))
         (power-ups (new-stack-manager))
         (menu (new-menu (new-point menu-position-x menu-position-y)
                         menu-width menu-height))
         (wave-adt (new-wave 0))
         (grey-enemy-died? #f)
         (manual-tower-placed? #f)
         (power-up-active? #f)
         (timer 0)
         (power-up-cooldown 7000)
         (cooldown-x 0)
         (cooldown-tile-width (* tile-width-px level-width))
         (path (new-path level-index))
         (cool-down-tile (make-tile cooldown-tile-width 10)))
    
    (define (check-price tag)
      (cond ((eq? tag 'standard-tower)
             (set! tower-price 100))
            ((eq? tag 'canon-tower)
             (set! tower-price 400))
            ((eq? tag 'manual-tower)
             (set! tower-price 300))
            ((eq? tag '8-bullet-tower)
             (set! tower-price 300))
            ((eq? tag 'bomber-tower)
             (set! tower-price 200))
            ((eq? tag 'teleporter-power-up)
             (set! power-up-price 400))
            ((eq? tag 'bomber-power-up)
             (set! power-up-price 500))))

    (define (on-path? x y)
      (pair? (and (assoc x (map (lambda (a)(cons (a 'x)(a 'y))) (path 'points)))
                  (assoc y (map (lambda (a)(cons (a 'y)(a 'x))) (path 'points))))))

    (define (tower-placed? x y)
      (define bool #f)
      (for-each (lambda (t)
                  (if (is-overlapping? (new-rect ((t 'position) 'x) ((t 'position) 'y) 1 1)
                                       (new-rect x y 1 1))
                      (set! bool #t)))
                (towers 's-list))
      bool)

    (define (enemy-reached-end?)
      (if (not (null? (enemies 's-list)))
          (car (map (lambda (e) (e 'end-reached?))(enemies 's-list)))
          #f))

    
    ;;
    ;; Create Objects
    ;;
    
    (define (create-enemy! type layer)
      ((enemies 'add!)
       (new-enemy layer level-index type)))

    (define (create-tower! type tower-layer gun-layer x y)
      (if (and (eq? type 'manual-tower) manual-tower-placed?)
          #f
          (if (not (tower-placed? x y))
              (begin
                (check-price type)
                (if (>= ((menu 'money) 'amount) tower-price)
                    (begin
                      (((menu 'money) 'add-amount!)(* tower-price -1))
                      ((towers 'add!)
                       (new-tower type (new-point x y) tower-layer gun-layer))
                      ((((towers 'top) 'position) 'x!) x)
                      ((((towers 'top) 'position) 'y!) y))))))
      (if (eq? type 'manual-tower)
          (set! manual-tower-placed? #t)))

    (define (create-power-up! type power-up-layer)
      (when (not power-up-active?)
        (check-price type)
        (if (> ((menu 'money) 'amount) power-up-price)
            (begin
              (((menu 'money) 'add-amount!) (* power-up-price -1))
              (let loop ((i 0)
                         (x ((list-ref (path 'points)(generate-random-number 0 (length (path 'points)))) 'x))
                         (y ((list-ref (path 'points)(generate-random-number 0 (length (path 'points)))) 'y)))
                (when (<= i power-up-amount) 
                  ((power-ups 'add!)
                   (new-power-up type power-up-layer (new-point x y)))
                  (loop (+ i 1)
                        ((list-ref (path 'points)(generate-random-number 0 (length (path 'points)))) 'x)
                        ((list-ref (path 'points)(generate-random-number 0 (length (path 'points)))) 'y))))            
              (set! power-up-active? #t)
              (set! power-up-cooldown (+ power-up-cooldown ((power-ups 'top) 'cooldown)))
              (create-tile! power-up-layer cool-down-tile)))))


      
    ;;
    ;; Update
    ;;

    (define (update! delta-time)
      ((enemies 'update!) delta-time)
      ((towers 'update!) delta-time)
      ((power-ups 'update!) delta-time)
      ((menu 'update!) delta-time)
      (on-hit!)
      (delete-enemy!)
      (timed-if (power-up-active? as-long power-up-cooldown)
                (do (set! cooldown-x (+ cooldown-x (/ power-up-cooldown 4000)))
                  ((cool-down-tile 'draw-line!) 0 0 cooldown-x 0 5 "white"))
                (else (set! cooldown-x 0)
                      (cool-down-tile 'clear!)
                      (power-ups 'clear!))
                for timer delta-time))

    (define (delete-enemy!)
      (for-each (lambda (e)
                  (if (or (e 'deleted?) (e 'end-reached?))
                      (begin
                        (if (and (e 'deleted?)(not (e 'end-reached?)))
                            (((menu 'money) 'add-amount!) 20))
                        (if (and (e 'deleted?)(eq? (e 'type) 'grey))
                            (set! grey-enemy-died? #t))
;                        (when (and (e 'deleted?)(eq? chance (generate-random-number 0 (+ chance 2))))
;                          ((power-ups 'add!)
;                           (new-power-up 'bomber-power-up layer (new-point
;                                                                 ((list-ref (path 'points)(generate-random-number 0 (length (path 'points)))) 'x)
;                                                                 ((list-ref (path 'points)(generate-random-number 0 (length (path 'points)))) 'y))))
;                          (set! power-up-active? #t))
                        ((enemies 'remove!) e))))
                (enemies 's-list)))

    (define (on-hit!)
      (for-each (lambda (enemy)
                  (for-each (lambda (tower)
                              (if (is-overlapping? (enemy 'boundaries)((tower 'bullet) 'boundaries))
                                  (if (eq? (tower 'type) 'canon-tower)
                                      (begin
                                        ((enemy 'take-hit!)(tower 'damage))
                                        ((enemy 'push-back!)))
                                      ((enemy 'take-hit!)(tower 'damage)))))
                            (towers 's-list))
                  (for-each (lambda (power-up)
                              (if (is-overlapping? (enemy 'boundaries)(power-up 'boundaries))
                                  ((enemy 'take-hit!)(power-up 'damage))))             
                            (power-ups 's-list)))
                (enemies 's-list)))
    
    (define (rotate-tower! x y)
      (for-each (lambda (t)
                  (if (eq? (t 'type) 'manual-tower)
                      ((t 'rotate!) (new-point x y))
                      (if (is-overlapping? (new-rect ((t 'position) 'x) ((t 'position) 'y) 1 1)
                                           (new-rect x y 1 1))
                          ((t 'rotate!)))))
                (towers 's-list)))
    
    (define (grey-enemy-died! bool)
      (set! grey-enemy-died? bool))

    (define (spawn-object! draw x y)
      (cond (((menu 'select-item?)   'standard-tower x y)
             ((menu 'selected-item!) 'standard-tower))
            (((menu 'select-item?)   'manual-tower x y)
             ((menu 'selected-item!) 'manual-tower))
            (((menu 'select-item?)   'canon-tower x y)
             ((menu 'selected-item!) 'canon-tower))
            (((menu 'select-item?)   'bomber-tower x y)
             ((menu 'selected-item!) 'bomber-tower))
            (((menu 'select-item?)   'bomber-power-up x y)
             ((menu 'selected-item!) 'bomber-power-up))
            (((menu 'select-item?)   'teleporter-power-up x y)
             ((menu 'selected-item!) 'teleporter-power-up))
            (else 
             (when (not ((menu 'select-in-menu?) x y))
               (if (or (eq? (menu 'selected-item) 'bomber-power-up)
                       (eq? (menu 'selected-item) 'teleporter-power-up))
                   (create-power-up! (menu 'selected-item) (draw 'power-up-layer))
                   (create-tower! (menu 'selected-item) (draw 'tower-layer) (draw 'gun-layer) x y)))))
      (display (menu 'selected-item))
      (newline))
    
    (define (mouse-click! draw btn x y)
      (cond ((eq? btn 'left)
             (spawn-object! draw x y))
            ((eq? btn 'right)
             (rotate-tower! x y))))

    (define (key-press! key)
      (cond ((eq? key 'left)
             (menu 'visibility!))))
    
    ;;
    ;; Dispatch
    ;;
    
    (define (dispatch-level msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'enemies) enemies)
            ((eq? msg 'wave-adt) wave-adt)
            ((eq? msg 'mouse-click!) mouse-click!)
            ((eq? msg 'key-press!) key-press!)
            ((eq? msg 'grey-enemy-died?) grey-enemy-died?)
            ((eq? msg 'grey-enemy-died!) grey-enemy-died!)
            ((eq? msg 'create-enemy!) create-enemy!)
            ((eq? msg 'enemy-reached-end?)(enemy-reached-end?))
            ((eq? msg 'menu) menu)
            (else (display msg))))
    dispatch-level))
