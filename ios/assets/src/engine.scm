;;-------------------------------------------------------------------------------
;; Application Life Cycle

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
    ;; Init graphics: shaders, textures...
    (init-graphics!)))

;; Cleanup resources and quit the application
(define (destroy-app!)
  (destroy-graphics!)
  (SDL_DestroyWindow window)
  (SDL_Quit)
  (exit))

;;! Single command for running the app and initializing if necessary
(define* (make-app (create-world:))
  (unless (procedure? create-world) (error-log make-app: "create-world parameter should be a procedure"))
  (when (zero? (SDL_WasInit 0)) (init-app!))
  (draw-world-wrapper
   (update-world-wrapper
    (process-events-wrapper
     (create-world-wrapper create-world)))))

;; Application main loop
;; TODO
;; (define (run-loop!)
;;   (when (zero? (SDL_WasInit 0))
;;         (init-app!))
;;   (cond-expand
;;    (ios
;;     (SDL_iPhoneSetAnimationCallback window 1 *sdl-ios-animation-callback-proxy* #f)
;;     (sdl-ios-animation-callback-set!
;;      (let ((world '()))
;;        (lambda (params)
;;          (update-app!)
;;          (handle-events world)
;;          (set! world (update-world))
;;          (draw world)))))
;;    (else
;;     (let loop ((world '()))
;;       (update-app!)
;;       (handle-events world)
;;       (loop (draw (update-world world)))))))

;;-------------------------------------------------------------------------------
;; Events

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

;; Process events and input
(define (process-events-wrapper world)
  (define (get-key-code event)
    (SDL_Keysym-sym
     (SDL_KeyboardEvent-keysym
      (SDL_Event-key event))))
  (let ((event (alloc-SDL_Event))
        (filled #f))
    (let ev-poll ((output '()))
      (if (= 1 (SDL_PollEvent event))
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
              ;;(ev-poll (cons '(fingermotion) output))
              (ev-poll output))
             ((= SDL_FINGERDOWN event-type)
              ;;(ev-poll (cons '(fingerdown) output))
              (ev-poll output))
             ((= SDL_FINGERUP event-type)
              ;; (let ((finger-event (SDL_Event-tfinger event)))
              ;;   (log "Finger -"
              ;;        "X:" (SDL_TouchFingerEvent-x finger-event)
              ;;        "Y:" (SDL_TouchFingerEvent-y finger-event)))
              ;;(ev-poll (cons '(fingerup) output))
              (ev-poll output))
             ;; Mouse events
             ((= SDL_MOUSEMOTION event-type)
              (let ((mouse-event (SDL_Event-motion event)))
                (let ((x (SDL_MouseMotionEvent-x mouse-event))
                      (y (SDL_MouseMotionEvent-y mouse-event))
                      (x-relative (SDL_MouseMotionEvent-xrel mouse-event))
                      (y-relative (SDL_MouseMotionEvent-yrel mouse-event)))
                  (ev-poll (cons `(mousemotion x: ,x y: ,y x-relative: ,x-relative y-relative: ,y-relative)
                                 output)))))
             ((= SDL_MOUSEBUTTONDOWN event-type)
              (let ((mouse-event (SDL_Event-button event)))
                (let ((x (SDL_MouseButtonEvent-x mouse-event))
                      (y (SDL_MouseButtonEvent-y mouse-event)))
                  (ev-poll (cons `(mousedown x: ,x y: ,y) output)))))
             ((= SDL_MOUSEBUTTONUP event-type)
              (let ((mouse-event (SDL_Event-button event)))
                (let ((x (SDL_MouseButtonEvent-x mouse-event))
                      (y (SDL_MouseButtonEvent-y mouse-event)))
                  (ev-poll (cons `(mouseup x: ,x y: ,y) output)))))
             ;; Window events
             ((= SDL_WINDOWEVENT event-type)
              (let ((window-event (SDL_WindowEvent-event (SDL_Event-window event))))
                (if (or (= SDL_WINDOWEVENT_SIZE_CHANGED window-event)
                        (= SDL_WINDOWEVENT_RESIZED window-event))
                    (let* ((resize (SDL_Event-window event))
                           (width (SDL_WindowEvent-data1 resize))
                           (height (SDL_WindowEvent-data2 resize)))
                      (ev-poll (cons `(window-resized width: ,width height: ,height) output))))))
             (else
              (log "Unhandled event - " event-type)
              (ev-poll output))))
          (world-events-set! world (reverse! output))))
    world))

;;-------------------------------------------------------------------------------
;; Graphics

;; Initialize the Openg GL/ES graphics and assets
(define (init-graphics!)
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
  (resize-graphics! screen-width screen-height)
  (init-shaders!)
  ;; Init sampler
  (cond-expand
   (host (glGenSamplers 1 sprite-sampler*)
         (let ((sampler-id (*->GLuint sprite-sampler*)))
           (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
           (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
           (glSamplerParameteri sampler-id GL_TEXTURE_MAG_FILTER GL_NEAREST)
           (glSamplerParameteri sampler-id GL_TEXTURE_MIN_FILTER GL_NEAREST)))
   (else #!void)))

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

;; Tear down all OpenGL structures
(define (destroy-graphics!)
  (table-for-each (lambda (buffer) (glDeleteBuffers 1 buffer)) gl-buffers)
  (set! gl-buffers (make-table))
  (table-for-each (lambda (buffer) (glDeleteTextures 1 buffer)) gl-textures)
  (set! gl-textures (make-table)))

;;! Type: Buffers
(define-type buffer
  constructor: buffer-constructor
  uuid
  id
  usage
  type
  data)

(define make-buffer
  (let ((f32vector->gl-buffer
         (lambda* (vertex-data-vector (buffer-type GL_STATIC_DRAW))
           (let ((buffer-id* (alloc-GLuint* 1)))
             (glGenBuffers 1 buffer-id*)
             (glBindBuffer GL_ARRAY_BUFFER (*->GLuint buffer-id*))
             (glBufferData GL_ARRAY_BUFFER
                           (* (f32vector-length vertex-data-vector) GLfloat-size)
                           (f32vector->GLfloat* vertex-data-vector)
                           GL_STATIC_DRAW)
             (glBindBuffer GL_ARRAY_BUFFER 0)
             buffer-id*))))
    ;; TODO: handle usage and data type
    (lambda* (uuid type data (usage 'static))
      (let ((instance (buffer-constructor
                       uuid
                       (case type
                         ((f32vector)
                          (f32vector->gl-buffer data))
                         (else (error-log "make-buffer - unknown data type")))
                       usage
                       type
                       data)))
        (table-set! gl-buffers uuid instance)
        instance))))

;;! Type: Texture
(define-type texture
  constructor: texture-constructor
  id ;; the OpenGL identifier
  key ;; a symbol identifier
  width
  height)

;;! Textures are automatically registered and can be later accessed from the global table
(define make-texture
  (let ((load-texture->gl-texture
         (lambda (path)
           (let* ((texture-img* (IMG_Load path)) ;; default format: ARGB8888
                  (texture-id* (alloc-GLuint* 1))
                  (texture-height (SDL_Surface-h texture-img*))
                  (texture-width (SDL_Surface-w texture-img*)))
             ;; Alternative method (using GL_RGBA). Remember that PixelFormat is backwards in SDL
             ;; (SDL_ConvertSurfaceFormat texture-img-unformatted* SDL_PIXELFORMAT_ABGR8888 0)
             ;; Generate and bind texture
             (glGenTextures 1 texture-id*)
             (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
             ;; Check errors
             (check-gl-error
              (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA ; internal format
                            texture-width texture-height
                            0 GL_BGRA_EXT GL_UNSIGNED_BYTE
                            (SDL_Surface-pixels texture-img*)))
             ;; FILTER: Necessary for NPOT textures in GLES2
             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
             ;; WRAP: Necessary for NPOT textures in GLES2
             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
             ;; Unbind and free the surface
             (glBindTexture GL_TEXTURE_2D 0)
             (SDL_FreeSurface texture-img*)
             (values texture-id*
                     texture-width
                     texture-height)))))
    (lambda* (key path (register?: #t))
        (receive (texture-id* w h)
                 (load-texture->gl-texture path)
                 (let ((instance (texture-constructor texture-id* key w h)))
                   (if register? (table-set! gl-textures key instance))
                   instance)))))

;;! Draw the given sprite in the current window
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

;;! Draw all elements in the world (internal wrapper)
(define (draw-world-wrapper world)
  (define current-color (list (random-real) (random-real) (random-real) 1.0))
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glEnable GL_BLEND)
  (glDisable GL_CULL_FACE)
  (glCullFace GL_BACK)
  ;; Background
  (apply glClearColor current-color)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT))
  ;; Draw the different elements
  (for-each draw-sprite (world-sprites world))
  ;; Render
  (SDL_GL_SwapWindow window)
  world)

;;-------------------------------------------------------------------------------
;; Application World

(define (create-world-wrapper create-proc)
  (create-proc
   (make-world '())))

(define (update-world-wrapper world)
  (let ((events (world-events world))
        (sprites (world-sprites world)))
    (let recur ((events events)
                (world world))
      (let ((handle-event-up&down
             (lambda (event-proc) ;; produce a lambda with event-proc specialization
               (lambda (event)
                 (recur
                  (cdr events)
                  (let ((x (cadr (memq x: event)))
                        (y (cadr (memq y: event))))
                    ;; If a world isn't produced, then pass along the original one
                    (aif new-world world?
                         (call/cc
                          (lambda (leave) ;; Consume the event if captured by one element
                            (fold (lambda (element world)
                                    (aif proc (event-proc element)
                                         (let ((element-x (sprite-x element))
                                               (element-y (sprite-y element))
                                               (element-width (sprite-width element))
                                               (element-height (sprite-height element)))
                                           (if (and (> x element-x)
                                                    (> y element-y)
                                                    (< x (+ element-x element-width))
                                                    (< y (+ element-y element-height)))
                                               (leave (proc element world event))))
                                         world))
                                  world
                                  sprites)))
                         new-world
                         world))))))
            (handle-event-motion
             (lambda (event)
               (recur
                (cdr events)
                (let ((x (cadr (memq x: event)))
                      (y (cadr (memq y: event)))
                      (x-relative (cadr (memq x-relative: event)))
                      (y-relative (cadr (memq y-relative: event))))
                  (aif new-world world?
                       (call/cc
                        (lambda (leave) ;; Consume the event once is captured by one element
                          (fold (lambda (element world)
                                  (let ((mouseover-proc (interactive-on-mouseover element))
                                        (mouseout-proc (interactive-on-mouseout element))
                                        (mousemove-proc (interactive-on-mousemove element)))
                                    (if (or mouseover-proc mouseout-proc mousemove-proc)
                                        (let ((element-x (sprite-x element))
                                              (element-y (sprite-y element))
                                              (element-width (sprite-width element))
                                              (element-height (sprite-height element))
                                              (x-previous (- x x-relative))
                                              (y-previous (- y y-relative)))
                                          (let ((element-x2 (+ element-x element-width))
                                                (element-y2 (+ element-y element-height)))
                                            (cond
                                             ;; If mouse entered the element (wasn't previously inside)
                                             ((and (> x element-x)
                                                   (> y element-y)
                                                   (< x element-x2)
                                                   (< y element-y2)
                                                   (not (and (> x-previous element-x)
                                                             (> y-previous element-y)
                                                             (< x-previous element-x2)
                                                             (< y-previous element-y2))))
                                              (leave (mouseover-proc element world event)))
                                             ;; If mouse left the element (was previously inside)
                                             ((and (> x-previous element-x)
                                                   (> y-previous element-y)
                                                   (< x-previous element-x2)
                                                   (< y-previous element-y2)
                                                   (not (and (> x element-x)
                                                             (> y element-y)
                                                             (< x element-x2)
                                                             (< y element-y2))))
                                              (leave (mouseout-proc element world event)))
                                             ;; If mouse is moving within the element (is AND was inside)
                                             ((and (> x-previous element-x)
                                                   (> y-previous element-y)
                                                   (< x-previous element-x2)
                                                   (< y-previous element-y2)
                                                   (> x element-x)
                                                   (> y element-y)
                                                   (< x element-x2)
                                                   (< y element-y2))
                                              (leave (mousemove-proc element world event)))
                                             (else world)))))))
                                world
                                sprites)))
                       new-world
                       world))))))
        (cond ((null? events) #!void)
              ((car events) (lambda (e) (eq? (car e) 'mousedown)) =>
               (handle-event-up&down interactive-on-mousedown))
              ((car events) (lambda (e) (eq? (car e) 'mouseup)) =>
               (handle-event-up&down interactive-on-mouseup))
              ((car events) (lambda (e) (eq? (car e) 'mousemotion)) =>
               handle-event-motion)
              (else
               (recur (cdr events) world)))))
    ;; Handle resize event
    (aif resize (assq 'window-resized events)
         (let ((width (cadr (memq width: resize-event)))
               (height (cadr (memq height: resize-event))))
           (resize-graphics! width height)))
    ;; Update world time
    (let* ((time (world-time world))
           (current-ticks (SDL_GetTicks))
           (previous-ticks (cadr (assq current-ticks: time)))
           (time-step (/ (- current-ticks previous-ticks) 1000.0)))
      (world-time-set! world
                       `((current-ticks: ,current-ticks)
                         (previous-ticks: ,previous-ticks)
                         (time-step: ,time-step)))
      world)))


;;-------------------------------------------------------------------------------
;;!! World elements

;;! Type: interactive
(define-type interactive
  extender: define-type-of-interactive
  (on-mouseover init: #f)
  (on-mouseout init: #f)
  (on-mousedown init: #f)
  (on-mouseup init: #f)
  (on-mousemove init: #f))

;;! Type: Sprite
(define-type-of-interactive sprite
  constructor: sprite-constructor
  uuid
  x
  y
  width
  height
  texture-key
  
  unprintable:
  texture-id
  buffer)

;; make-sprite
;; .parameter x The x coordinate of the top-left corner
;; .parameter y The y coordinate of the top-left corner
;; .parameter texture/key The texture or the texture key associated to the sprite
(define* (make-sprite x y texture/key
                      (on-mouseover:)
                      (on-mouseout:)
                      (on-mousedown:)
                      (on-mouseup:)
                      (on-mousemove:))
  (let* ((tex (cond ((texture? texture/key) texture/key)
                    ((table-ref gl-textures texture/key #f) => values)
                    (else
                     (error-log make-sprite:
                                "texture/key parameter requires either a texture or a texture key"))))
         (texture-w (texture-width tex))
         (texture-h (texture-height tex))
         (buffer-uuid (random-integer 9999999999999999999)) ;; UID: TODO
         (sprite
          (sprite-constructor
           (random-integer 99999999999999999999) ;; UID: TODO
           x
           y
           (when tex texture-w)
           (when tex texture-h)
           texture-key
           (texture-id tex)
           (make-buffer buffer-uuid
                        'f32vector
                        (let ((qx1 x)
                              (qy1 y))
                          (let ((qx2 (+ qx1 texture-w))
                                (qy2 (+ qy1 texture-h)))
                            (f32vector qx1 qy1 0.0 0.0
                                       qx1 qy2 0.0 1.0
                                       qx2 qy1 1.0 0.0
                                       qx2 qy1 1.0 0.0
                                       qx1 qy2 0.0 1.0
                                       qx2 qy2 1.0 1.0)))))))
    (when on-mouseover (interactive-on-mouseover-set! sprite on-mouseover))
    (when on-mouseout (interactive-on-mouseout-set! sprite on-mouseout))
    (when on-mousedown (interactive-on-mousedown-set! sprite on-mousedown))
    (when on-mouseup (interactive-on-mouseup-set! sprite on-mouseup))
    (when on-mousemove (interactive-on-mousemove-set! sprite on-mousemove))
    ;; Will to automatically remove the associated OpenGL/ES vertex buffer that was created
    ;; along with this sprite, as soon as this instance is destroyed
    (make-will sprite (lambda (s) (table-set! gl-buffers buffer-uuid)))
    sprite))

;;! Type: World
(define-type world
  constructor: world-constructor
  time
  events
  sprites)

(define (make-world elements)
  (world-constructor
   '((current-ticks: 0)
     (previous-ticks: 0)
     (time-step: 0))
   ;; bootstrapped world has no events in queue
   '()
   ;; elements get splitted into different kinds
   elements))
