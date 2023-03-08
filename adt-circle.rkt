;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Cirkel ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; maak-cirkel :: number, number -> punt
(define (new-circle center radius)

  (define (center! new-center)
    (set! center new-center))
  
  (define (radius! new-radius)
    (set! radius new-radius))
  
  (define (dispatch-circle msg)
    (cond ((eq? msg 'center) center)
          ((eq? msg 'radius) radius)          
          ((eq? msg 'radius!) radius!)
          ((eq? msg 'center!) center!)))
  dispatch-circle)
