;;-------------------------------------------------------------------------------
;; App

(define app
  (make-app
   create-world:
   (lambda (world)
     (let ((texture1 (make-texture 'cordoba1 "assets/images/cordoba1.png"))
           (texture2 (make-texture 'cordoba2 "assets/images/cordoba2.png"))
           (texture3 (make-texture 'cordoba3 "assets/images/cordoba3.png")))
       (make-world (list (make-sprite 20.0 170.0 texture1
                                      on-mouseup: (lambda (self world event)
                                                    (SDL_Log "MOUSE UP")
                                                    (world-sprites-set! world
                                                                        (cons (make-sprite 20.0 670.0 texture3)
                                                                              (world-sprites world)))
                                                    world)
                                      on-mouseover: (lambda args (SDL_Log "MOUSE OVER"))
                                      on-mouseout: (lambda args (SDL_Log "MOUSE OUT"))
                                      on-mousemove: (lambda args (SDL_Log "MOUSE MOVE")))
                         (make-sprite 20.0 420.0 texture2)))))
   pre-render: (let ((color-r (random-real)))
                 (lambda (world)
                   (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
                   (glEnable GL_BLEND)
                   (glDisable GL_CULL_FACE)
                   (glCullFace GL_BACK)
                   (apply glClearColor (list color-r 0.0 1.0 1.0))
                   (glClear (bitwise-ior GL_COLOR_BUFFER_BIT))
                   (if (>= color-r 1.0)
                       (set! color-r 0.0)
                       (set! color-r (+ 0.02 color-r)))))
   post-render: (lambda (world) (SDL_Delay 100))))

(app)
