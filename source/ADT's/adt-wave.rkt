;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Wave ADT                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-wave wave)
  (define (reset!)
    (set! wave 0))

  (define (increment!)
    (set! wave (+ wave 1)))

  (define wave-tile-list '())
  
  (define (create! layer)
    (define wave-tile
      (make-bitmap-tile (string-append "../images/num_" (number->string wave) ".png")
                        (string-append "../images/num_" (number->string wave) "_mask.png")))
    (when (null? wave-tile-list)
      ((create-tile! layer wave-tile)
       (draw-tile! wave-tile (new-point wave-position-x wave-position-y))
       (set! wave-tile-list (cons wave-tile wave-tile-list)))
      (else
       (delete! (car wave-tile-list) layer)
       (create-tile! layer wave-tile)
       (draw-tile! wave-tile (new-point wave-position-x wave-position-y))
       (set-car! wave-tile-list wave-tile))))

  
  (define (dispatch-wave msg)
    (cond ((eq? msg 'wave) wave)
          ((eq? msg 'reset!) (reset!))
          ((eq? msg 'increment!) (increment!))
          ((eq? msg 'create!) create!)))
  dispatch-wave)

