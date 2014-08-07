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
  (define (update-source source)
    (if (zero? (shell-command (string-append "wget localhost:8000/" source " -O spheres/src/" source)))
        (load (string-append "spheres/src/" source))
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

;; Run the application
(go)

;; ;; Put the main thread to sleep, unless we want exit the main (like in iOS)
(cond-expand
 (ios #!void)
 (else (thread-sleep! +inf.0)))
