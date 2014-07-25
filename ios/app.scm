;; Executes the given form and checks if GL's state is valid
(define-macro (check-gl-error exp)
  `(begin
     ,exp
     (let ((error (glGetError)))
       (if (not (= error GL_NO_ERROR))
           (begin
             (SDL_Log (string-append "GL Error: " (object->string error) " - " (object->string ',exp)))
             #f)
           #t))))

;; Loads an image from the given path and creates a texture object to hold it
(define (load-texture window path)
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

;; Loads a shader using the given relative path
(define (load-shader path)
  (fusion:load-text-file (string-append "assets/shaders/" path)))

;; Creates a new program with the given vertex and shader files paths.
;; A callback function to set up the attributes must be provided
(define (create-program vertex-shader fragment-shader callback)
  (let* ((shaders (list (fusion:create-shader GL_VERTEX_SHADER
                                              (load-shader vertex-shader))
                        (fusion:create-shader GL_FRAGMENT_SHADER
                                              (load-shader fragment-shader))))
         (program-id (fusion:create-program shaders callback)))
    (for-each glDeleteShader shaders)
    program-id))

;; Creates a new OpenGL VBO from a given f32vector.
(define* (create-buffer-from-vector vertex-data-vector (buffer-type GL_STATIC_DRAW))
  (let ((buffer-id* (alloc-GLuint* 1)))
    (glGenBuffers 1 buffer-id*)
    (let* ((buffer-id (*->GLuint buffer-id*))
           (vertex-data (f32vector->GLfloat* vertex-data-vector)))
      (glBindBuffer GL_ARRAY_BUFFER buffer-id)
      (glBufferData GL_ARRAY_BUFFER
                    (* (f32vector-length vertex-data-vector) GLfloat-size)
                    vertex-data
                    GL_STATIC_DRAW)
      (glBindBuffer GL_ARRAY_BUFFER 0))
    buffer-id*))

;; Draws the given vbo  with a particular program. The callback is
;; used to set up the attributes of the dynamic attributes
(define (draw-vbo vbo-id* program-id type count attribs-callback)
  (let ((vbo-id (*->GLuint vbo-id*)))
    (glUseProgram program-id)
    (if (check-gl-error (glBindBuffer GL_ARRAY_BUFFER vbo-id))
        (begin
          (attribs-callback)
          (check-gl-error (glDrawArrays type 0 count))
          (glBindBuffer GL_ARRAY_BUFFER 0)))
    (glUseProgram 0)))



;;;;;;;;;;;;;;;;;


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
(define float-matrix #f)

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

;; Loads the shaders and sets the location of the necessary attributes
(define (init-shaders)
  (set! color-program-id
        (create-program "color.vert"
                        "color.frag"
                        (lambda (program-id)
                          (glBindAttribLocation program-id 0 "position"))))
  (set! tex2d-program-id
        (create-program "tex2d.vert"
                        "tex2d.frag"
                        (lambda (program-id)
                          (glBindAttribLocation program-id 0 "position")
                          (glBindAttribLocation program-id 1 "texCoord"))))
  (glUseProgram tex2d-program-id)
  (check-gl-error (set! attr1 (glGetUniformLocation tex2d-program-id "colorTexture")))
  (check-gl-error (set! attr2 (glGetUniformLocation tex2d-program-id "perspectiveMatrix")))
  (glUseProgram 0))

;; Creates VBOs from the static vectors defined at the top of the file
(define (init-buffers)
  (set! tri-vertex-id* (create-buffer-from-vector triangle-data-vector))
  (set! quad-vertex-id* (create-buffer-from-vector quad-data-vector)))

;; Loads the texture used by the quad and creates a sampler if running on host
(define (init-images window)
  (set! sprite-id* (load-texture window "assets/images/lambda.png"))
  ;; Sampler
  (cond-expand
   (host (glGenSamplers 1 sprite-sampler*)
         (let ((sampler-id (*->GLuint sprite-sampler*)))
           (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
           (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
           (glSamplerParameteri sampler-id GL_TEXTURE_MAG_FILTER GL_NEAREST)
           (glSamplerParameteri sampler-id GL_TEXTURE_MIN_FILTER GL_NEAREST)))
   (else #!void)))

(define (resize-gui screen-width screen-height)
  (set! perspective-matrix
        (matrix:* (make-translation-matrix -1.0 1.0 0.0)
                  (matrix:* (make-scaling-matrix (/ 2.0 screen-width)
                                                 (/ -2.0 screen-height) 1.0)
                            (make-identity-matrix))))
  (set! float-matrix (matrix->GLfloat*
                      (matrix:map exact->inexact
                                  perspective-matrix))))

;; Initializes the required components for drawing
(define (init-gui window screen-width screen-height)
  (resize-gui screen-width screen-height)
  (init-shaders)
  (init-buffers)
  (init-images window))

(define (destroy-gui)
  (glDeleteBuffers 1 tri-vertex-id*)
  (glDeleteBuffers 1 quad-vertex-id*)
  (glDeleteTextures 1 sprite-id*)
  (glDeleteProgram color-program-id)
  (glDeleteProgram tex2d-program-id))

(define (draw-triangle window)
  (draw-vbo tri-vertex-id* color-program-id GL_TRIANGLES 3
            (lambda ()
              (glEnableVertexAttribArray 0)
              (glVertexAttribPointer 0 4 GL_FLOAT GL_FALSE 0 #f))))

(define (draw-sprite window)
  (draw-vbo quad-vertex-id* tex2d-program-id GL_TRIANGLES 6
            (lambda ()
              (cond-expand
               (host (glBindSampler 0 (*->GLuint sprite-sampler*)))
               (else #!void))

              (check-gl-error (glUniform1i attr1 0))
              (check-gl-error (glUniformMatrix4fv attr2 1 GL_FALSE float-matrix))

              (glEnableVertexAttribArray 0)
              (glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) #f)
              (glEnableVertexAttribArray 1)
              (glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) (integer->void* (* 2 GLfloat-size)))

              (glActiveTexture GL_TEXTURE0)
              (glBindTexture GL_TEXTURE_2D (*->GLuint sprite-id*)))))

(define (draw-gui time-step window)
  (set! ellapsed-time (+ ellapsed-time time-step))

  (if (> ellapsed-time bkg-color-period)
      (begin
        (set! current-color (list (random-real) (random-real) (random-real) 1.0))
        (set! ellapsed-time 0)))

  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glEnable GL_BLEND)

  (glDisable GL_CULL_FACE)
  (glCullFace GL_BACK)

  (apply glClearColor current-color)

  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT))

  (draw-triangle window)
  (draw-sprite window))


(define (draw time-step window)
  (draw-gui time-step window)
  (SDL_GL_SwapWindow window))




;;;;;;;;;;;;;;;;;;;




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

(define (init-app)
  (let ((mode* (alloc-SDL_DisplayMode))
        (flags-sdl (bitwise-ior SDL_INIT_VIDEO SDL_INIT_AUDIO))
        (flags-img (bitwise-ior IMG_INIT_JPG IMG_INIT_PNG)))
    (when (< (SDL_Init flags-sdl) 0)
          (fusion:error "Couldn't initialize SDL!"))
    (when (not (= (IMG_Init flags-img) flags-img))
          (fusion:error "Couldn't initialize SDL Image!"))
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
    ;; Get screen size
    (SDL_GetDisplayMode 0 0 mode*)
    (set! screen-width (SDL_DisplayMode-w mode*))
    (set! screen-height (SDL_DisplayMode-h mode*))
    (set! window
          (SDL_CreateWindow "SDL/GL" SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED screen-width screen-height
                            (bitwise-ior SDL_WINDOW_OPENGL SDL_WINDOW_RESIZABLE SDL_WINDOW_BORDERLESS)))
    (unless window (fusion:error "Unable to create render window" (SDL_GetError)))
    ;; OpenGL/ES context
    (let ((ctx (SDL_GL_CreateContext window)))
      (SDL_Log (string-append "SDL screen size: " (object->string screen-width) " x " (object->string screen-height)))
      (SDL_Log (string-append "OpenGL Version: " (*->string (glGetString GL_VERSION))))
      ;; Glew: initialize extensions
      (cond-expand (host (glewInit)) (else #!void)))
    ;; OpenGL viewport
    (glViewport 0 0 screen-width screen-height)
    (glScissor 0 0 screen-width screen-height)
    ;; Init GUI
    (init-gui window screen-width screen-height)))

(define current-ticks 0)
(define previous-ticks 0)
(define time-step 0)

(define (test)
  (let ((event (alloc-SDL_Event))
        (exit-app #f))
    (let ev-poll ()
      (when (= 1 (SDL_PollEvent event))
            (cond ((exit-application? event)
                   (set! exit-app #t))
                  ((resize-event? event)
                   (let ((resize (SDL_Event-window event)))
                     (resize-gui (SDL_WindowEvent-data1 resize) (SDL_WindowEvent-data2 resize)))))))

    (set! current-ticks (SDL_GetTicks))
    (set! time-step (/ (- current-ticks previous-ticks) 1000.0))
    (set! previous-ticks current-ticks)
    (draw time-step window)
    ;;(free event)
    ))

(define (quit-app)
  (destroy-gui)
  (SDL_DestroyWindow window)
  (SDL_Quit)
  (exit))
