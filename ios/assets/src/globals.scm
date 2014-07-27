;;-------------------------------------------------------------------------------
;; SDL

(define window #f)
(define screen-width #f)
(define screen-height #f)

;;-------------------------------------------------------------------------------
;; OpenGL

;; Matrices
(define perspective-matrix #f)
(define gl-perspective-matrix #f)

;; Shader programs
(define gl-programs (make-table))

;; Buffers
(define gl-buffers (make-table))

;; Textures
(define gl-textures (make-table))
(cond-expand
 (host (define sprite-sampler* (alloc-GLuint* 1)))
 (else #!void))

;; Uniform variables
(define gl-uniforms (make-table))





; Vertex coordinates for the triangle
(define tx1 0.0)
(define ty1 0.0)

(define tx2 640.0)
(define ty2 960.0)

; Vertex coordinates for the quad (two triangles)
(define qx1 50.0)
(define qy1 50.0)

(define qx2 300.0)
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

;;-------------------------------------------------------------------------------
;; Time

(define ellapsed-time 0)
(define current-ticks 0)
(define previous-ticks 0)
(define time-step 0)
