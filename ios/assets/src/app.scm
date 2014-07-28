;;-------------------------------------------------------------------------------
;; Events

;; TODO: decouple form graphics
(define (handle-events)
  (define (get-key-code event)
    (SDL_Keysym-sym
     (SDL_KeyboardEvent-keysym
      (SDL_Event-key event))))
  (let ((event (alloc-SDL_Event))
        (filled #f))
    (let ev-poll ()
      (when (= 1 (SDL_PollEvent event))
            (let ((event-type (SDL_Event-type event)))
              (cond
               ((or (= SDL_QUIT event-type)
                    (and
                     (= SDL_KEYUP event-type)
                     (or (= (get-key-code event) SDLK_ESCAPE)
                         (= (get-key-code event) SDLK_AC_BACK))))
                (SDL_Quit))
               ;; Touch events
               ((= SDL_FINGERMOTION event-type)
                'ignore)
               ((= SDL_FINGERDOWN event-type)
                'ignore)
               ((= SDL_FINGERUP event-type)
                ;; (let ((finger-event (SDL_Event-tfinger event)))
                ;;   (log "Finger -"
                ;;        "X:" (SDL_TouchFingerEvent-x finger-event)
                ;;        "Y:" (SDL_TouchFingerEvent-y finger-event)))
                'ignore)
               ;; Mouse events
               ((= SDL_MOUSEMOTION event-type)
                'ignore)
               ((= SDL_MOUSEBUTTONDOWN event-type)
                'ignore)
               ((= SDL_MOUSEBUTTONUP event-type)
                (let ((mouse-event (SDL_Event-button event)))
                  (log "Mouse button -"
                       "X:" (SDL_MouseButtonEvent-x mouse-event)
                       "Y:" (SDL_MouseButtonEvent-y mouse-event))))
               ;; Window events
               ((and
                 (= SDL_WINDOWEVENT event-type)
                 (or (= SDL_WINDOWEVENT_SIZE_CHANGED (SDL_WindowEvent-event (SDL_Event-window event)))
                     (= SDL_WINDOWEVENT_RESIZED (SDL_WindowEvent-event (SDL_Event-window event)))))
                (let ((resize (SDL_Event-window event)))
                  (resize-graphics! (SDL_WindowEvent-data1 resize)
                                    (SDL_WindowEvent-data2 resize))))
               (else
                (log "Unhandled event - " event-type))))
            (ev-poll)))))

(define (init-events!)
  (SDL_SetEventFilter *sdl-events-filter-proxy* #f)
  (sdl-events-filter-set!
   (lambda (userdata event)
     (let ((event-type (SDL_Event-type event)))
       (cond ((= SDL_APP_TERMINATING event-type)
              ;; Terminate the app. Shut everything down before returning from this function.
              0)
             ((= SDL_APP_LOWMEMORY event-type)
              ;; You will get this when your app is paused and iOS wants more memory.
              ;; Release as much memory as possible.
              0)
             ((= SDL_APP_WILLENTERBACKGROUND event-type)
              ;; Prepare your app to go into the background.  Stop loops, etc.
              ;; This gets called when the user hits the home button, or gets a call.
              0)
             ((= SDL_APP_DIDENTERBACKGROUND event-type)
              ;; This will get called if the user accepted whatever sent your app to the background.
              ;; If the user got a phone call and canceled it, you'll instead get an SDL_APP_DIDENTERFOREGROUND event and restart your loops.
              ;; When you get this, you have 5 seconds to save all your state or the app will be terminated.
              ;; Your app is NOT active at this point.
              0)
             ((= SDL_APP_WILLENTERFOREGROUND event-type)
              ;; This call happens when your app is coming back to the foreground.
              ;; Restore all your state here.
              0)
             ((= SDL_APP_DIDENTERFOREGROUND event-type)
              ;; Restart your loops here.
              ;; Your app is interactive and getting CPU again.
              0)
             (else
              ;; No special processing, add it to the event queue
              1))))))

;;-------------------------------------------------------------------------------
;; Application World

(define (update-world world)
  world)

;;-------------------------------------------------------------------------------
;; Drawing

;; (define (draw-triangle)
;;   (gl-draw-vbo (table-ref gl-buffers 'tri-vertices)
;;                (table-ref gl-programs 'color)
;;                GL_TRIANGLES 3
;;                (lambda ()
;;                  (check-gl-error
;;                   (glUniformMatrix4fv (table-ref gl-uniforms 'perspective)
;;                                       1 GL_FALSE gl-perspective-matrix))
;;                  (glEnableVertexAttribArray 0)
;;                  (glVertexAttribPointer 0 4 GL_FLOAT GL_TRUE 0 #f))))

(define (draw-sprite sprite)
  (gl-draw-vbo (buffer-id (sprite-buffer sprite))
               (table-ref gl-programs 'tex2d)
               GL_TRIANGLES 6
               (lambda ()
                 (cond-expand (host (glBindSampler 0 (*->GLuint sprite-sampler*)))
                              (else #!void))
                 (check-gl-error
                  (glUniform1i (table-ref gl-uniforms 'texture) 0))
                 (check-gl-error
                  (glUniformMatrix4fv (table-ref gl-uniforms 'perspective) 1 GL_FALSE gl-perspective-matrix))
                 (glEnableVertexAttribArray 0)
                 (glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) #f)
                 (glEnableVertexAttribArray 1)
                 (glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size)
                                        (integer->void* (* 2 GLfloat-size)))
                 (glActiveTexture GL_TEXTURE0)
                 (glBindTexture GL_TEXTURE_2D (*->GLuint (texture-id (table-ref gl-textures 'the-lambda)))))))

(define (draw world)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glEnable GL_BLEND)
  (glDisable GL_CULL_FACE)
  (glCullFace GL_BACK)
  (apply glClearColor current-color)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT))
  ;;(draw-triangle)
  (if my-sprite (draw-sprite my-sprite))
  (if my-sprite2 (draw-sprite my-sprite2))
  (SDL_GL_SwapWindow window))

;;-------------------------------------------------------------------------------
;; Graphics

;; Loads the shaders and sets the location of the necessary attributes
(define (init-shaders!)
  ;; Creates a new program with the given vertex and shader files paths.
  ;; A callback function to set up the attributes must be provided
  (let ((color-program-id
         (gl-create-program (list (gl-create-shader
                                   GL_VERTEX_SHADER
                                   (load-text-file "assets/shaders/color.vert"))
                                  (gl-create-shader
                                   GL_FRAGMENT_SHADER
                                   (load-text-file "assets/shaders/color.frag")))
                            (lambda (program-id)
                              (glBindAttribLocation program-id 0 "position"))
                            delete-shaders?: #t)))
    (table-set! gl-programs 'color color-program-id)
    (glUseProgram color-program-id)
    (check-gl-error
     (table-set! gl-uniforms 'perspective
                 (glGetUniformLocation color-program-id "perspectiveMatrix")))
    (glUseProgram 0))
  (let ((tex2d-program-id
         (gl-create-program (list (gl-create-shader
                                   GL_VERTEX_SHADER
                                   (load-text-file "assets/shaders/tex2d.vert"))
                                  (gl-create-shader
                                   GL_FRAGMENT_SHADER
                                   (load-text-file "assets/shaders/tex2d.frag")))
                            (lambda (program-id)
                              (glBindAttribLocation program-id 0 "position")
                              (glBindAttribLocation program-id 1 "texCoord"))
                            delete-shaders?: #t)))
    (table-set! gl-programs 'tex2d tex2d-program-id)
    (glUseProgram tex2d-program-id)
    (check-gl-error
     (table-set! gl-uniforms 'texture
                 (glGetUniformLocation tex2d-program-id "colorTexture")))
    (check-gl-error
     (table-set! gl-uniforms 'perspective (glGetUniformLocation tex2d-program-id "perspectiveMatrix")))
    (glUseProgram 0)))

;; Initialize the OpenGL buffers
;; (define (init-buffers!)
;;   (update-buffers!))

;; Loads the texture used by the quad and creates a sampler if running on host
;; (define (init-textures! window)
;;   (table-set! gl-textures 'sprite1
;;               (load-texture->gl-texture window "assets/images/lambda.png"))
;;   ;; Sampler
;;   (cond-expand
;;    (host (glGenSamplers 1 sprite-sampler*)
;;          (let ((sampler-id (*->GLuint sprite-sampler*)))
;;            (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
;;            (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
;;            (glSamplerParameteri sampler-id GL_TEXTURE_MAG_FILTER GL_NEAREST)
;;            (glSamplerParameteri sampler-id GL_TEXTURE_MIN_FILTER GL_NEAREST)))
;;    (else #!void)))

;; Initializes the graphic system
(define my-sprite #f) ;; XXX
(define my-sprite2 #f) ;; XXX

(define (init-graphics!)
  (resize-graphics! screen-width screen-height)
  (init-shaders!)
  ;;(init-buffers!)
  ;;(init-textures! window)
  (make-texture 'the-lambda "assets/images/lambda.png")
  (set! my-sprite (make-sprite 50.0 50.0 'the-lambda))
  (set! my-sprite2 (make-sprite 250.0 250.0 'the-lambda)))

;; Handle window resizing and orientation
(define (resize-graphics! screen-width screen-height)
  (set! screen-width screen-width)
  (set! screen-height screen-height)
  (set! perspective-matrix
        (matrix:* (make-translation-matrix -1.0 1.0 0.0)
                  (matrix:* (make-scaling-matrix (/ 2.0 screen-width)
                                                 (/ -2.0 screen-height)
                                                 1.0)
                            (make-identity-matrix))))
  (set! gl-perspective-matrix (matrix->GLfloat*
                               (matrix:map exact->inexact
                                           perspective-matrix))))

;; Updates VBOs from the static vectors defined at the top of the file
;; (define (update-buffers!)
;;   (table-set! gl-buffers 'tri-vertices (f32vector->gl-buffer triangle-data-vector))
;;   (table-set! gl-buffers 'quad-vertices (f32vector->gl-buffer quad-data-vector)))

;; Tear down all OpenGL structures
(define (destroy-graphics!)
  (table-for-each (lambda (buffer) (glDeleteBuffers 1 buffer)) gl-buffers)
  (set! gl-buffers (make-table))
  (table-for-each (lambda (buffer) (glDeleteTextures 1 buffer)) gl-textures)
  (set! gl-textures (make-table)))

;;-------------------------------------------------------------------------------
;; Application life cycle

;; Single command for running the app and initializing if necessary
(define (run!)
  (when (zero? (SDL_WasInit 0))
        (init-app!))
  (update-app!)
  (handle-events)
  (draw (update-world '())))

;; Application main loop
;; TODO
(define (run-loop!)
  (when (zero? (SDL_WasInit 0))
        (init-app!))
  (cond-expand
   (ios
    (SDL_iPhoneSetAnimationCallback window 1 *sdl-ios-animation-callback-proxy* #f)
    (sdl-ios-animation-callback-set!
     (let ((world '()))
       (lambda (params)
         (update-app!)
         (handle-events)
         (set! world (update-world))
         (draw world)))))
   (else
    (let loop ((world '()))
      (update-app!)
      (handle-events)
      (let ((new-world (update-world world)))
        (draw new-world)
        (loop new-world))))))

;; Initializes the App
(define (init-app!)
  (let ((mode* (alloc-SDL_DisplayMode))
        (flags-sdl (bitwise-ior SDL_INIT_VIDEO SDL_INIT_AUDIO))
        (flags-img (bitwise-ior IMG_INIT_JPG IMG_INIT_PNG)))
    (when (< (SDL_Init flags-sdl) 0)
          (error-log "Couldn't initialize SDL!"))
    ;; Initialize events. Do it as soon as possible, for platforms such as iOS
    (init-events!)
    ;; SDL_image initialization
    (when (not (= (IMG_Init flags-img) flags-img))
          (error-log "Couldn't initialize SDL Image!"))
    (cond-expand
     (host #!void)
     (else
      (SDL_GL_SetAttribute SDL_GL_CONTEXT_PROFILE_MASK SDL_GL_CONTEXT_PROFILE_ES)
      (SDL_GL_SetAttribute SDL_GL_CONTEXT_MAJOR_VERSION 2)
      (SDL_GL_SetAttribute SDL_GL_CONTEXT_MINOR_VERSION 0)))
    (SDL_GL_SetAttribute SDL_GL_ALPHA_SIZE 8)
    (SDL_GL_SetAttribute SDL_GL_RED_SIZE 8)
    (SDL_GL_SetAttribute SDL_GL_GREEN_SIZE 8)
    (SDL_GL_SetAttribute SDL_GL_BLUE_SIZE 8)
    (SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)
    (SDL_GL_SetAttribute SDL_GL_DEPTH_SIZE 0)
    (SDL_GL_SetAttribute SDL_GL_RETAINED_BACKING 1)
    ;; Get screen size, Portrait orientation by default
    (SDL_GetDisplayMode 0 0 mode*)
    (let ((reported-width (SDL_DisplayMode-w mode*))
          (reported-height (SDL_DisplayMode-h mode*)))
      (set! screen-width (min reported-width reported-height))
      (set! screen-height (max reported-width reported-height)))
    (set! window
          (SDL_CreateWindow "SDL/GL" SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED
                            screen-width screen-height
                            (bitwise-ior SDL_WINDOW_OPENGL
                                         SDL_WINDOW_RESIZABLE
                                         SDL_WINDOW_BORDERLESS
                                         SDL_WINDOW_ALLOW_HIGHDPI)))
    (unless window (error-log "Unable to create render window" (SDL_GetError)))
    ;; OpenGL/ES context
    (let ((ctx (SDL_GL_CreateContext window)))
      (SDL_Log (string-append "SDL screen size: " (object->string screen-width) " x " (object->string screen-height)))
      (SDL_Log (string-append "OpenGL Version: " (*->string (glGetString GL_VERSION))))
      ;; Glew: initialize extensions
      (cond-expand (host (glewInit)) (else #!void)))
    ;; OpenGL viewport
    (glViewport 0 0 screen-width screen-height)
    (glScissor 0 0 screen-width screen-height)
    (init-graphics!)))

;; Update global variables
(define bkg-color-period 1) ;; TODO: REMOVE
(define current-color '(0.0 0.0 0.0 1.0))

;; Update App system and globals
(define (update-app!)
  (set! current-ticks (SDL_GetTicks))
  (set! time-step (/ (- current-ticks previous-ticks) 1000.0))
  ;; TODO: REMOVE
  (set! ellapsed-time (+ ellapsed-time time-step))
  (set! previous-ticks current-ticks)
  (when (> ellapsed-time bkg-color-period)
        (set! current-color (list (random-real) (random-real) (random-real) 1.0))
        (set! ellapsed-time 0)))

;; Cleanup resources and quit the application
(define (quit-app!)
  (destroy-graphics!)
  (SDL_DestroyWindow window)
  (SDL_Quit)
  (exit))

;;-------------------------------------------------------------------------------
;; Automatically run App on load

(run!)
