;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Const Variables                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tile-width-px 64)
(define tile-height-px 64)

(define level-width 20)
(define level-height 10)

(define bullet-speed 10)
(define enemy-speed 0.02)

(define score-position-x 0.75)
(define score-position-y 0.6)

(define window-width (* tile-width-px level-width))
(define window-height (* tile-height-px level-height))

(define tower-range 4); (new-circle (new-point 2 2) 2))
(define tower-damage 2)