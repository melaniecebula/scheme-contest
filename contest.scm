;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: Now in 3D (oh god I haven't slept)
;;;
;;; Description:
;;;   <Invest in this code
;;;    Get your money back and more
;;;    A Pyramid Scheme>

(define (draw)
    ; *YOUR CODE HERE*
  (speed 0)

  ;;; setpos3d introduces variables and projection matrix, matrix multiplies, and redefines a set of 3D points in 2D.  
  ;;;  The other functions deal with creating the 3D sierpinski triangles.
  (define (setpos3d x y z) 
    (define width 600)
    (define height 600)
    (define aspect (/ width height))
    (define fov 100)
    (define zfar 1000)
    (define znear 1)
    (define projmat (list 
                      (/ (/ 1 (tan (radians (/ fov 2)))) aspect) 
                      0 0 0 0 
                      (/ 1 (tan (radians (/ fov 2)))) 
                      0 0 0 0 
                      (/ (* -1 (+ zfar znear)) (- zfar znear)) 
                      (/ (* -1 2 zfar znear) (- zfar znear)) 
                      0 0 -1 0))
    
    (define (matrix-multiply matrix vector new-array count)
      (if (> count 12) new-array 
        (let ((next-array
          (cond
              ((null? matrix) new-array)
              ((or (= count 0) (= count 4) (= count 8) (= count 12))
                (append new-array (list 
                  (+ 
                    (* (car matrix) (car vector)) 
                    (* (cadr matrix) (cadr vector)) 
                    (* (caddr matrix) (caddr vector)) 
                    (* (cadddr matrix) (cadddr vector))))))
              (else (append '() new-array)))))
        (matrix-multiply (cdr matrix) vector next-array (+ count 1)))))

    (define projected-coord (matrix-multiply projmat (list x y z 1) '() 0))
    (define x0 (/ (car projected-coord) (caddr projected-coord)))
    (define y0 (/ (cadr projected-coord) (caddr projected-coord)))
    (define x0 (- (* (+ x0 1) (/ width 2)) (/ width 2)))
    (define y0 (- (* (+ y0 1) (/ height 2)) (/ height 2)))
    (setpos x0 y0))

  ;;;finds mid
    (define (mid v1 v2)
      (list 
        (/ (+ (car v1) (car v2)) 2)
        (/ (+ (cadr v1) (cadr v2)) 2)
        (/ (+ (caddr v1) (caddr v2)) 2)))

  ;;;makes calls to setpos3d to create 3d shapes
    (define (polygon v1 v2 v3)
      (pu)
      (setpos3d (car v1) (cadr v1) (caddr v1))
      (pd)
      (setpos3d (car v2) (cadr v2) (caddr v2))
      (setpos3d (car v3) (cadr v3) (caddr v3))
      (setpos3d (car v1) (cadr v1) (caddr v1))
      (pu))

  ;;;creates a pyramid
    (define (pyramid c1 c2 c3 ctop)
      (polygon c1 c2 ctop)
      (polygon c2 c3 ctop)
      (polygon c3 c1 ctop)
      (polygon c1 c2 c3))

    ;;;recursively creates pyramids in order to create sierpinski's triangles in 3D
    (define (sierpinski c1 c2 c3 ctop depth)
      (if (> depth 0)
        (begin
          (pyramid c1 c2 c3 ctop)
          (sierpinski c1 (mid c1 c2) (mid c1 c3) (mid c1 ctop) (- depth 1))
          (sierpinski c2 (mid c2 c3) (mid c2 c1) (mid c2 ctop) (- depth 1))
          (sierpinski c3 (mid c3 c1) (mid c3 c2) (mid c3 ctop) (- depth 1))
          (sierpinski (mid c1 ctop) (mid c2 ctop) (mid c3 ctop) ctop (- depth 1)))
        (pyramid c1 c2 c3 ctop)))

    ;;;initiating the pyramid
    (sierpinski (list -50 -20 -50) (list 50 -20 -40) (list 10 0 -85) (list 5 40 -65) 4)
    (pu)
    (fd 500)
    (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.  All Scheme tokens in this file (including the one below) count
; toward the token limit.

(draw)