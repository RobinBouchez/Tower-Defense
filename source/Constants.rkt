;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Const Variables                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tile-width-px 64)
(define tile-height-px 64)

(define level-width 20)
(define level-height 10)

(define bullet-width 10)
(define bullet-height 10)

(define window-width (* tile-width-px level-width))
(define window-height (* tile-height-px level-height))

(define wave-position-x 0.75)
(define wave-position-y 0.6)

(define menu-width 4)
(define menu-height 10)

(define menu-position-x (- level-width menu-width))
(define menu-position-y 0)

(define health-position-x 18)
(define health-position-y 0.5)
(define health-offset-x 0.5)

(define path-corner-x 15)
(define path-corner-y 8)

(define money-pos-offset-x 2.25)
(define money-pos-offset-y 8.78)
(define money-start-amount 1000)
(define money-text-fontsize 18)
(define money-text-x 0)
(define money-text-y 0)
(define money-text-color "white")

(define tower-price 100)
(define power-up-price 400)

(define start-tile-height 11)

(define power-up-amount 4)

(define level-pos-x 0)
(define level-pos-y 0)

(define chance 4)

(define wave-bonus 100)
