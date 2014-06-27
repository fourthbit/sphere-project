(define-cond-expand-feature compile-to-c)
(define-cond-expand-feature debug)
(define-cond-expand-feature android)
(define-cond-expand-feature mobile)
(define frame-delay .4)
(define (main)
  (SDL_Log "***** Trying to connect to Gambit Debug Server *****")
  (if (remote-repl-setup! "localhost" port: 20000)
      (begin
        (remote-repl-run!)
        (SDL_Log "***** Successfully connected to Gambit Debug Server *****"))
      (SDL_Log "***** Unable to connect to Gambit Debug Server. Are you running 'sense'? *****"))
  (let ((mode* (alloc-SDL_DisplayMode)))
    (when (< (SDL_Init SDL_INIT_VIDEO) 0)
          (fusion:error "Couldn't initialize SDL!"))
    (cond-expand
     (mobile (SDL_GL_SetAttribute
              SDL_GL_CONTEXT_PROFILE_MASK
              SDL_GL_CONTEXT_PROFILE_ES)
             (SDL_GL_SetAttribute SDL_GL_CONTEXT_MAJOR_VERSION 2))
     (else #!void))
    (SDL_GL_SetAttribute SDL_GL_DOUBLEBUFFER 1)
    (SDL_GetDisplayMode 0 0 mode*)
    (let* ((screen-width (SDL_DisplayMode-w mode*))
           (screen-height (SDL_DisplayMode-h mode*))
           (window (SDL_CreateWindow
                    "SDL/GL"
                    SDL_WINDOWPOS_CENTERED
                    SDL_WINDOWPOS_CENTERED
                    screen-width
                    screen-height
                    (bitwise-ior SDL_WINDOW_OPENGL SDL_WINDOW_RESIZABLE))))
      (unless window
              (fusion:error "Unable to create render window" (SDL_GetError)))
      (let ((event (alloc-SDL_Event)) (ctx (SDL_GL_CreateContext window)))
        (SDL_Log (string-append
                  "SDL screen size: "
                  (object->string screen-width)
                  " x "
                  (object->string screen-height)))
        (SDL_Log (string-append
                  "OpenGL Version: "
                  (*->string (glGetString GL_VERSION))))
        (let recur ((iteration 0))
          (SDL_PollEvent event)
          (SDL_GL_SwapWindow window)
          (SDL_Log (number->string iteration))
          (glClearColor (random-real) (random-real) (random-real) 1.)
          (glClear (bitwise-ior
                    GL_COLOR_BUFFER_BIT
                    GL_DEPTH_BUFFER_BIT
                    GL_STENCIL_BUFFER_BIT))
          (SDL_GL_SwapWindow window)
          (thread-sleep! frame-delay)
          (unless (= SDL_QUIT (SDL_Event-type event))
                  (recur (++ iteration))))))))
