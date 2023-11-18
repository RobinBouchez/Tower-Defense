;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Rect ADT                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-rect x y width height)
 
  (define (dispatch-rect msg)
    (cond ((eq? msg 'x) x)
          ((eq? msg 'y) y)
          ((eq? msg 'width) width)
          ((eq? msg 'height) height)))
  dispatch-rect)
