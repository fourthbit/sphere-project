;; Executes the given form and checks if GL's state is valid
(define-macro (check-gl-error exp)
  `(let* ((result ,exp)
          (error (glGetError)))
     (unless (= error GL_NO_ERROR)
             (error-log (string-append "GL Error -- " (object->string error)
                                       " - " (object->string ',exp))))
     result))


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

;; type: Buffers
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

;; Textures
(define gl-textures (make-table))

;; type: Texture
(define-type texture
  constructor: texture-constructor
  id ;; the OpenGL identifier
  key ;; a symbol identifier
  width
  height)

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

;; (cond-expand
;;  (host (define sprite-sampler* (alloc-GLuint* 1)))
;;  (else #!void))


;; Uniform variables
(define gl-uniforms (make-table))



(define world (make-table))


;; type: Sprite
(define-type sprite
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

(define (make-sprite x y texture-key)
  (let* ((tex (table-ref gl-textures texture-key #f))
         (texture-w (texture-width tex))
         (texture-h (texture-height tex)))
   (sprite-constructor
    (random-integer 99999999) ;; UID: TODO
    x
    y
    (when tex texture-w)
    (when tex texture-h)
    texture-key
    (texture-id tex)
    (make-buffer (random-integer 99999999) ;; UID: TODO
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


; Vertex coordinates for the quad (two triangles)
(define qx1 50.0)
(define qy1 50.0)

(define qx2 300.0)
(define qy2 300.0)

(define quad-data-vector (f32vector qx1 qy1 0.0 0.0
                                    qx1 qy2 0.0 1.0
                                    qx2 qy1 1.0 0.0

                                    qx2 qy1 1.0 0.0
                                    qx1 qy2 0.0 1.0
                                    qx2 qy2 1.0 1.0))

(define triangle-data-vector
  (let ((tx1 0.0)
        (ty1 0.0)
        (tx2 640.0)
        (ty2 960.0))
    (f32vector tx1 ty1 0.0 1.0
               tx1 ty2 0.0 1.0
               tx2 ty2 0.0 1.0)))



;;-------------------------------------------------------------------------------
;; Time

(define ellapsed-time 0)
(define current-ticks 0)
(define previous-ticks 0)
(define time-step 0)
