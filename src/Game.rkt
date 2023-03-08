(#%require (only racket random))
(#%require "Graphics.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Globale Values en Procedures                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   ADT's                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic ADT's
(load "adt-point.rkt")
(load "adt-circle.rkt")
   
;; Game object ADT's
(load "adt-tower.rkt")
(load "adt-gun.rkt")
(load "adt-path.rkt")
(load "adt-enemy.rkt")
(load "adt-score.rkt")
(load "adt-button.rkt")
;(load "adt-wave.rkt")
(load "adt-bullet.rkt")

;; Game Object Managers
(load "adt-manager.rkt")

;; Game Logic ADT's
(load "adt-draw.rkt")
(load "adt-level.rkt")
(load "adt-game.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Game Start Function                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define My-Cool-Tower-Defense-Game-for-programmeerproject-1 (new-game-adt))
((My-Cool-Tower-Defense-Game-for-programmeerproject-1 'start!))
