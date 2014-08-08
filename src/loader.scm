;; SchemeSpheres environment

(define eval-file
  (lambda (file)
    (for-each eval (with-input-from-file file read-all))))
(parameterize
 ((current-directory
   (cond-expand (android "sdcard") (else "spheres"))))
 (load "core/lib/syntax-case.o1")
 (eval-file "core/src/base-macros.scm")
 (eval-file "core/src/assert-macros.scm")
 (SDL_Log "Successfully loaded environment"))


;; Initialize globals
(define (init-globals)
  (if (zero? (shell-command "wget localhost:8000/globals.scm -O spheres/src/globals.scm"))
      (load "spheres/src/globals.scm")
      (println "globals.scm could not be retrieved"))
  (if (zero? (shell-command "wget localhost:8000/engine-types.scm -O spheres/src/engine-types.scm"))
      (load "spheres/src/engine-types.scm")
      (println "engine-types.scm could not be retrieved")))

;; Automatically load files
(init-globals)

;; Update the app Scheme source code
(define (go)
  (define project-sources "spheres/src/")
  (define (update-source source)
    (if (zero? (parameterize
                ((current-directory project-sources))
                (shell-command (string-append "wget -nv -N localhost:8000/" source))))
        (load (string-append project-sources source))
        (let ((message (string-append source " could not be retrieved")))
          (SDL_Log message)
          (println message))))
  (eval '(add-cond-expand-feature! ios))
  ;; gl-utils.scm
  (update-source "gl-utils.scm")
  ;; engine.scm
  (update-source "engine.scm")
  ;; app.scm
  (update-source "app.scm")
  'success)


;; Spawn a REPL
(if (remote-repl-setup! "localhost" port: 20000)
    (begin
      (remote-repl-run!)
      (SDL_Log "***** Successfully connected to Gambit Debug Server *****"))
    (SDL_Log "***** Unable to connect to Gambit Debug Server. Are you running 'sense'? *****"))


;; Wait until the thread receives a message to leave
(define *main-thread* (current-thread))
(thread-receive)
