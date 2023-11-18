;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Menu ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-menu position width height)
  (let* ((visible? #f)
         (selected-item 'standard-tower)
         (money (new-money money-start-amount))
         (pos-vis position)
         (pos-cl (new-point (+ (position 'x) menu-width) (position 'y)))
         (menu-item-list (list (cons 'standard-tower (new-point 17 2))
                               (cons 'manual-tower (new-point 17 4))
                               (cons 'canon-tower (new-point 18 4))
                               (cons 'bomber-tower (new-point 18 2))                                 
                               (cons 'bomber-power-up (new-point 18 6))
                               (cons 'teleporter-power-up (new-point 17 6)))))
  
    (define (visibility!)
      (set! visible? (not visible?)))
    
    (define (selected-item! item)
      (set! selected-item item))

    (define (update! delta-time)
      (if visible?
          (if (not (= (position 'x)(pos-vis 'x)))
              (set! position (new-point (- (position 'x) 1) (position 'y)))
              (set! position pos-vis))
          (if (not (= (position 'x)(pos-cl 'x)))
              (set! position (new-point (+ (position 'x) 1) (position 'y)))
              (set! position pos-cl))))

    (define (select-in-menu? x y)
      (and visible? (is-overlapping? (new-rect x y 1 1)
                                     (new-rect (position 'x) (position 'y) menu-width menu-height))))
    
    (define (select-item? item-tag x y)
      (if visible?
          (if (>= x (+ menu-position-x 1))
              (is-overlapping? (new-rect x y 0 0)
                               (new-rect ((cdr (assoc item-tag menu-item-list)) 'x)
                                         ((cdr (assoc item-tag menu-item-list)) 'y) 0 0))
              #f)
          #f))

    (define (dispatch-menu msg)
      (cond ((eq? msg 'position) position)
            ((eq? msg 'selected-item) selected-item)
            ((eq? msg 'selected-item!) selected-item!)
            ((eq? msg 'select-item?) select-item?)
            ((eq? msg 'select-in-menu?) select-in-menu?)
            ((eq? msg 'money) money)
            ((eq? msg 'visibility!) (visibility!))
            ((eq? msg 'update!) update!)))
    dispatch-menu))