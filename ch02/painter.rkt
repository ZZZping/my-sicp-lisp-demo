#lang scheme/gui
(define (frame-coord-map frame)
  (lambda (v) (add-vect
                        (origin-frame frame)
                        (add-vect (scale-vect (xcor-vect v)
                                              (edge1-frame frame))
                                  (scale-vect (ycor-vect v)
                                              (edge2-frame frame))))))
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect x y)
  (cons (+ (xcor-vect x) (xcor-vect y)) (+ (ycor-vect x) (ycor-vect y))))
(define (sub-vect x y)
  (cons (- (xcor-vect x) (xcor-vect y)) (- (ycor-vect x) (ycor-vect y))))
(define (scale-vect s a)
  (cons (* (xcor-vect a) s) (* (ycor-vect a) s)))
(define (make-frame origin edge1 edge2) (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define no-pen (make-object pen% "BLACK" 1 'transparent'))
(define red-pen (make-object pen% "RED" 2 'solid'))
(define black-pen (make-object pen% "BLACK" 2 'solid'))
(define no-brush (make-object brush% "BLACK" 'transparent'))
(define yellow-brush (make-object brush% "YELLOW" 'solid'))
(define red-brush (make-object brush% "RED" 'solid'))
(define (edge2-frame frame) (caddr frame))
(define (draw-face dc)
  (define (draw-line start end)
    (define (draw-line-coef coef)
      (send dc draw-line (* coef (car start)) (* coef (cdr start)) (* coef (car end)) (* coef (cdr end))))
    (draw-line-coef 50))
  (define (make-segment a b c d)
    (list (cons (/ a 4.1) (/ b 4.1)) (cons (/ c 4.1) (/ d 4.1))))
  (define (start-segment segment)
    (car segment))
  (define (end-segment segment)
    (cdr segment)))
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line ((frame-coord-map frame) (start-segment segment))
                  ((frame-coord-map frame) (end-segment segment))))
     segment-list)))
(send dc set-smoothing 'smoothed)
(send sc set-pen black-pen)
#|

|#

(define (wave frame)
  ((segments->painter (list
                       (make-segment 0    0.7   0.6   1.7)
                       (make-segment 0.6  1.7   1.2   1.5)
                       (make-segment 1.2  1.5   1.6   1.5)
                       (make-segment 1.6  1.5   1.45  0.6)
                       (make-segment 1.45 0.6   1.6   0)

                       (make-segment 2.45 0     2.65  0.61)
                       (make-segment 2.65 0.61  2.5   1.45)
                       (make-segment 2.5  1.45  3.1   1.5)
                       (make-segment 3.1  1.5   4.1   2.7)

                       (make-segment 0    1.5   0.6   2.5)
                       (make-segment 0.6  2.5   1.2   1.7)
                       (make-segment 1.2  1.7   1.4   2.2)
                       (make-segment 1.4  2.2   1     4.1)

                       (make-segment 1.6  4.1   2.05  3)
                       (make-segment 2.05 3     2.4   4.1)

                       (make-segment 3.2  4.1   2.45  2.35)
                       (make-segment 2.45 2.35  4.1   3.5)
                       )) frame))

(define (beside left right)
  (lambda (frame)
    ((transform-painter left
                        (make-vect 0.0 0.0)
                        (make-vect 0.5 0.0)
                        (make-vect 0.0 1)) frame)
    ((transform-painter right
                        (make-vect 0.5 0.0)
                        (make-vect 1   0.0)
                        (make-vect 0.0 1)) frame)))
(define (below left right)
  (lambda (frame)
    ((transform-painter left
                        (make-vect 0.0 0.0)
                        (make-vect 1   0.0)
                        (make-vect 0.0 0.5)) frame)
    ((transform-painter right
                        (make-vect 0.0 0.5)
                        (make-vect 1   0.5)
                        (make-vect 0.0 1)) frame)))
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave wave2))
(define (right-split painter n)
  (if (= n 0) painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (4-corner painter n)
  (beside (flip-horiz (below (flip-vert (corner-split painter n))
                             (corner-split painter n)))
          (below (flip-vert (corner-split painter n))
                 (corner-split painter n))))
(define myWindow (new frame% [label "example window"]
                      [width 300]
                      [height 300]))
(define myCanvas (new canvas%
                      [parent myWindow]
                      [paint-callback (lambda (canvas dc) (draw-face dc))]))
(send myWindow show #t)