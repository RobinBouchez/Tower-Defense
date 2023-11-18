(#%require (only racket random))
(#%require "Graphics.rkt")
(#%require (only racket/base random))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Globale Values en Procedures                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "Constants.rkt")
(load "DrawFunctions.rkt")
(load "MathFunctions.rkt")
(load "Macro's.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   ADT's                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "./ADT's/adt-point.rkt")
(load "./ADT's/adt-vector2.rkt")
(load "./ADT's/adt-rect.rkt")

(load "./ADT's/adt-game-object.rkt")
(load "./ADT's/adt-draw.rkt")

(load "./ADT's/adt-tower.rkt")
(load "./ADT's/adt-standard-tower.rkt")
(load "./ADT's/adt-manual-tower.rkt")
(load "./ADT's/adt-canon-tower.rkt")
(load "./ADT's/adt-bomber-tower.rkt")

(load "./ADT's/adt-bullet.rkt")

(load "./ADT's/adt-enemy.rkt")
(load "./ADT's/adt-red-enemy.rkt")
(load "./ADT's/adt-blue-enemy.rkt")
(load "./ADT's/adt-grey-enemy.rkt")

(load "./ADT's/adt-path.rkt")
(load "./ADT's/adt-menu.rkt")
(load "./ADT's/adt-wave.rkt")
(load "./ADT's/adt-health.rkt")
(load "./ADT's/adt-money.rkt")
(load "./ADT's/adt-power-up.rkt")

(load "./ADT's/adt-manager.rkt")

(load "./ADT's/adt-level.rkt")
(load "./ADT's/adt-game.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Game Start Function                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Game (new-game))
((Game 'start!)) 