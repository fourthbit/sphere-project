;;-------------------------------------------------------------------------------
;; The App

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
                         (make-sprite 250.0 250.0 texture)))))))
