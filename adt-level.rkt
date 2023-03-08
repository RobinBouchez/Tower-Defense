;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Level ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-level width height)
  (let* ((path-start-point (new-point 2 10))
         (path-end-point (new-point 20 8))
         (path (new-path path-start-point path-end-point))
         (enemies (new-manager))
         (towers (new-manager))
         (score-adt (new-score 0)))

    ((enemies 'add)
     (new-enemy path-start-point 1))

    (define (create-path)    
      ((path 'add-point)(new-point 20 8))
      ((path 'add-point)(new-point 15 8))
      ((path 'add-point)(new-point 15 1))
      ((path 'add-point)(new-point 8 1))
      ((path 'add-point)(new-point 8 4))
      ((path 'add-point)(new-point 2 4)))
    
    (define (update! delta-time)
      ((enemies 'update!) delta-time)
      ;(map (lambda (x)((update! x) delta-time)) (towers 'list))
      (follow-path (enemies 'first) (path 'path-list)))

    ((towers 'set-max-length) 5)
    (define (click-tower x y)
      ((towers 'add)
       (new-tower (new-point x y) tower-range tower-damage))
      ((((towers 'last) 'position) 'x!) x)
      ((((towers 'last) 'position) 'y!) y))

    (define (is-end-of-path-reached? enemy end-point)
      (and (>= ((enemy 'position) 'x) (end-point 'x))
           (>= ((enemy 'position) 'y) (end-point 'y))))
    
    (define (follow-path enemy path)
      (if (not (is-end-of-path-reached? enemy path-end-point))
          (if (null? path)
              (create-path)
              (begin
                (cond ((and (>= ((enemy 'position) 'x)  15)
                            (> ((enemy 'position) 'y)  8))
                       ((enemy 'speed!) (new-point enemy-speed 0)))
                      ((>= ((enemy 'position) 'x)  15)
                       ((enemy 'speed!) (new-point 0 enemy-speed)))
                      ((and (>= ((car path) 'y) ((enemy 'position) 'y))
                            (<= ((car path) 'x) ((enemy 'position) 'x)))    
                       (begin
                         ((enemy 'speed!) (new-point 0 0))
                         (follow-path enemy (cdr path))))
                      ((> ((car path) 'x) ((enemy 'position) 'x))
                       ((enemy 'speed!) (new-point enemy-speed 0)))
                      ((< ((car path) 'y) ((enemy 'position) 'y))
                       ((enemy 'speed!) (new-point 0 (* -1 enemy-speed)))))))
          (begin
            (enemy 'remove!)
            ((score-adt 'score!)(- (score-adt 'score) 1)))))
    ;;
    ;; Dispatch
    ;;
    
    (define (dispatch-level msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'enemies) enemies)
            ((eq? msg 'score-adt) score-adt)
            ((eq? msg 'click-tower) click-tower)
            ((eq? msg 'key!) key-press!)
            ((eq? msg 'towers) towers)))
    dispatch-level))
