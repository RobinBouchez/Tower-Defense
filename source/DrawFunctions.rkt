;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Draw functions                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
(define (draw-tile! tile position)
  (let*  ((screen-x (* tile-width-px (position 'x)))
          (screen-y (* tile-height-px (position 'y))))
    ((tile 'set-x!) screen-x)
    ((tile 'set-y!) screen-y)))

(define (delete! tile layer)
  (if (not (layer 'empty?))
      (begin
        (tile 'clear!)
        ((layer 'remove-drawable!) tile))))
 
(define (create-tile! layer tile)
  ((layer 'add-drawable!) tile))

(define (is-overlapping? r1 r2)
  (cond ((or (< (+ (r1 'x) (r1 'width)) (r2 'x)) (< (+ (r2 'x)(r2 'width)) (r1 'x)))
         #f)
        ((or (> (r1 'y)(+ (r2 'y) (r2 'height))) (> (r2 'y) (+ (r1 'y)(r1 'height))))
         #f)
        (else #t)))
   