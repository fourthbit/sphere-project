(define window #f)
(define screen-width)
(define screen-height)


(define ellapsed-time 0)
(define bkg-color-period 1)
(define current-color '(0.0 0.0 0.0 1.0))

(define color-program-id #f)
(define tex2d-program-id #f)

(define tri-vertex-id* #f)
(define quad-vertex-id* #f)

(define sprite-id* #f)
(cond-expand
 (host
  (define sprite-sampler* (alloc-GLuint* 1)))
 (else #!void))

(define perspective-matrix #f)
(define perspective-matrix-gl #f)

(define attr1 #f)
(define attr2 #f)

; Vertex coordinates for the triangle
(define tx1 -0.25)
(define tx2 0.25)
(define ty1 0.25)
(define ty2 -0.25)

; Vertex coordinates for the quad (two triangles)
(define qx1 50.0)
(define qx2 300.0)
(define qy1 50.0)
(define qy2 300.0)

(define triangle-data-vector (f32vector tx1 ty1 0.0 1.0
                                        tx1 ty2 0.0 1.0
                                        tx2 ty2 0.0 1.0 ))

(define quad-data-vector (f32vector qx1 qy1 0.0 0.0
                                    qx1 qy2 0.0 1.0
                                    qx2 qy1 1.0 0.0

                                    qx2 qy1 1.0 0.0
                                    qx1 qy2 0.0 1.0
                                    qx2 qy2 1.0 1.0))
