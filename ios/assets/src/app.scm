;;-------------------------------------------------------------------------------
;; Events

;; TODO: decouple form art
(define (handle-events!)
  (define (get-key-code event)
    (SDL_Keysym-sym
     (SDL_KeyboardEvent-keysym
      (SDL_Event-key event))))
  (define (resize-event? event)
    (and
     (= SDL_WINDOWEVENT (SDL_Event-type event))
     (or (= SDL_WINDOWEVENT_SIZE_CHANGED (SDL_WindowEvent-event (SDL_Event-window event)))
         (= SDL_WINDOWEVENT_RESIZED (SDL_WindowEvent-event (SDL_Event-window event))))))
  (define (click-action? event)
    (or (= SDL_FINGERDOWN (SDL_Event-type event))
        (= SDL_MOUSEBUTTONDOWN (SDL_Event-type event))))
  (define (exit-application? event)
    (or (= SDL_QUIT (SDL_Event-type event))
        (and
         (= SDL_KEYUP (SDL_Event-type event))
         (or (= (get-key-code event) SDLK_ESCAPE)
             (= (get-key-code event) SDLK_AC_BACK)))))
  (let ((event (alloc-SDL_Event)))
    (let ev-poll ()
      (when (= 1 (SDL_PollEvent event))
            (cond ((exit-application? event)
                   (SDL_Quit))
                  ((click-action? event)
                   (SDL_Log "CLICK!"))
                  ((resize-event? event)
                   (let ((resize (SDL_Event-window event)))
                     (resize-graphics! (SDL_WindowEvent-data1 resize)
                                       (SDL_WindowEvent-data2 resize)))))
            'continue))))


;;-------------------------------------------------------------------------------
;; Application World

(define (update-world world)
  world)

;;-------------------------------------------------------------------------------
;; Drawing

(define (draw-triangle)
  (draw-vbo (table-ref gl-buffers 'tri-vertices)
            (table-ref gl-programs 'color)
            GL_TRIANGLES 3
            (lambda ()
              (check-gl-error
               (glUniformMatrix4fv (table-ref gl-uniforms 'perspective)
                                   1 GL_FALSE gl-perspective-matrix))
              (glEnableVertexAttribArray 0)
              (glVertexAttribPointer 0 4 GL_FLOAT GL_TRUE 0 #f))))

(define (draw-sprite)
  (draw-vbo (table-ref gl-buffers 'quad-vertices)
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
              (glBindTexture GL_TEXTURE_2D (*->GLuint (table-ref gl-textures 'sprite1))))))

(define (draw world)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glEnable GL_BLEND)
  (glDisable GL_CULL_FACE)
  (glCullFace GL_BACK)
  (apply glClearColor current-color)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT))
  (draw-triangle)
  (draw-sprite)
  (SDL_GL_SwapWindow window))

;;-------------------------------------------------------------------------------
;; App life cycle

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
(define (init-buffers!)
  (update-buffers!))

;; Loads the texture used by the quad and creates a sampler if running on host
(define (init-textures! window)
  (table-set! gl-textures 'sprite1
              (load-texture->gl-texture window "assets/images/lambda.png"))
  ;; Sampler
  (cond-expand
   (host (glGenSamplers 1 sprite-sampler*)
         (let ((sampler-id (*->GLuint sprite-sampler*)))
           (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
           (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
           (glSamplerParameteri sampler-id GL_TEXTURE_MAG_FILTER GL_NEAREST)
           (glSamplerParameteri sampler-id GL_TEXTURE_MIN_FILTER GL_NEAREST)))
   (else #!void)))

;; Initializes the graphic system
(define (init-graphics!)
  (resize-graphics! screen-width screen-height)
  (init-shaders!)
  (init-buffers!)
  (init-textures! window))

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
(define (update-buffers!)
  (table-set! gl-buffers 'tri-vertices (f32vector->gl-buffer triangle-data-vector))
  (table-set! gl-buffers 'quad-vertices (f32vector->gl-buffer quad-data-vector)))

;; Tear down all OpenGL structures
(define (destroy-graphics!)
  (table-for-each (lambda (buffer) (glDeleteBuffers 1 buffer)) gl-buffers)
  (set! gl-buffers (make-table))
  (table-for-each (lambda (buffer) (glDeleteTextures 1 buffer)) gl-textures)
  (set! gl-textures (make-table)))

;;-------------------------------------------------------------------------------
;; Utils

;; Executes the given form and checks if GL's state is valid
(define-macro (check-gl-error exp)
  `(begin
     ,exp
     (let ((error (glGetError)))
       (if (= error GL_NO_ERROR)
           #t
           (begin
             (SDL_Log (string-append "GL Error: " (object->string error) " - " (object->string ',exp)))
             #f)))))

;; Loads an image from the given path and creates a texture object to hold it
(define (load-texture->gl-texture window path)
  (let* ((texture-img* (IMG_Load path)) ;; default format: ARGB8888
         (texture-id* (alloc-GLuint* 1)))
    ;; Alternative method (using GL_RGBA). Remember that PixelFormat is backwards in SDL
    ;; (SDL_ConvertSurfaceFormat texture-img-unformatted* SDL_PIXELFORMAT_ABGR8888 0)
    ;; Generate and bind texture
    (glGenTextures 1 texture-id*)
    (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
    ;; Check errors
    (check-gl-error
     (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA ; internal format
                   (SDL_Surface-w texture-img*) (SDL_Surface-h texture-img*)
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
    texture-id*))

;; Creates a new OpenGL VBO from a given f32vector.
(define* (f32vector->gl-buffer vertex-data-vector
                               (buffer-type GL_STATIC_DRAW))
  (let ((buffer-id* (alloc-GLuint* 1)))
    (glGenBuffers 1 buffer-id*)
    (glBindBuffer GL_ARRAY_BUFFER (*->GLuint buffer-id*))
    (glBufferData GL_ARRAY_BUFFER
                  (* (f32vector-length vertex-data-vector) GLfloat-size)
                  (f32vector->GLfloat* vertex-data-vector)
                  GL_STATIC_DRAW)
    (glBindBuffer GL_ARRAY_BUFFER 0)
    buffer-id*))

;; Draws the given vbo with a particular program. The callback is
;; used to set up the attributes of the dynamic attributes
(define (draw-vbo vbo-id* program-id type count attribs-callback)
  (let ((vbo-id (*->GLuint vbo-id*)))
    (glUseProgram program-id)
    (when (check-gl-error (glBindBuffer GL_ARRAY_BUFFER vbo-id))
          (attribs-callback)
          (check-gl-error (glDrawArrays type 0 count))
          (glBindBuffer GL_ARRAY_BUFFER 0))
    (glUseProgram 0)))

;;-------------------------------------------------------------------------------
;; Application life cycle

;; Single command for running the app and initializing if necessary
(define (run!)
  (when (zero? (SDL_WasInit 0))
        (init-app!))
  (handle-events!)
  (update-app!)
  (draw (update-world '())))

;; Initializes the App
(define (init-app!)
  (let ((mode* (alloc-SDL_DisplayMode))
        (flags-sdl (bitwise-ior SDL_INIT_VIDEO SDL_INIT_AUDIO))
        (flags-img (bitwise-ior IMG_INIT_JPG IMG_INIT_PNG)))
    (when (< (SDL_Init flags-sdl) 0)
          (error-log "Couldn't initialize SDL!"))
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
    ;; Get screen size, Portrait orientation by default
    (SDL_GetDisplayMode 0 0 mode*)
    (let* ((reported-width (SDL_DisplayMode-w mode*))
           (reported-height (SDL_DisplayMode-h mode*))
           (width (min reported-width reported-height))
           (height (max reported-width reported-height)))
      (set! screen-width width)
      (set! screen-height height))
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
