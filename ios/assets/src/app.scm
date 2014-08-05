;;-------------------------------------------------------------------------------
;; App

(define app
  (make-app
   create-world:
   (lambda (world)
     (let ((texture (make-texture 'the-lambda "assets/images/lambda.png")))
       (make-world (list (make-sprite 50.0 50.0 texture
                                      on-mouseup: (lambda (self world event)
                                                    (println "Mouse up within sprite: ")
                                                    (world-sprites-set! world
                                                                        (cons (make-sprite 50.0 550.0 texture)
                                                                              (world-sprites world)))
                                                    world)
                                      on-mouseover: (lambda args (println "MOUSE OVER"))
                                      on-mouseout: (lambda args (println "MOUSE OUT"))
                                      on-mousemove: (lambda args (println "MOUSE MOVE")))
                         (make-sprite 250.0 250.0 texture)))))
   pre-render: (lambda (world)
                 ;;(define current-color (list (random-real) (random-real) (random-real) 1.0))
                 (define current-color (list 1.0 0.0 0.0 1.0))
                 (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
                 (glEnable GL_BLEND)
                 (glDisable GL_CULL_FACE)
                 (glCullFace GL_BACK)
                 (apply glClearColor current-color)
                 (glClear (bitwise-ior GL_COLOR_BUFFER_BIT)))))

(app)
