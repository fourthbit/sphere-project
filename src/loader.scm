(cond-expand
 ;; Android platform
 (android
  #!void)
 ;; iOS platform
 (ios
  (c-declare
#<<end-c-declare

#include "stdio.h"
#import "UIDevice+Hardware.h"

end-c-declare
  )
  (define ios-device (c-lambda () int
#<<end-c-lambda

UIDevice *h=[[UIDevice alloc] init];
int r = [h hardware];
[h release];
___result = r;

end-c-lambda
  ))
  (define ios-device-description (c-lambda () nonnull-char-string
#<<end-c-lambda

UIDevice *h=[[UIDevice alloc] init];
const char *c = [[h hardwareDescription] UTF8String];
[h release];
___result = (char *) c;

end-c-lambda
  ))
  (define printf (c-lambda (char-string) void "printf(\"%s\",___arg1);"))
  ;;(printf (string-append (number->string (ios-device)) "\n"))
  (printf (string-append "iOS Hardware: "(ios-device-description) "\n")))
 ;; Host platform
 (else
  #!void))



;; TODO: look for the right place
(define eval-file
  (lambda (file)
    (for-each eval (with-input-from-file file read-all))))

;; TODO: move to SDL for iOS
(define *current-sdl-events-filter*
  (lambda (userdata event)
    (SDL_Log "SDL Events Filter is not set")
    1))

(define (sdl-events-filter-set! proc)
  (set! *current-sdl-events-filter* proc))

(c-define (*sdl-events-filter-proxy* userdata event)
          (void* SDL_Event*) int "SDL_default_events_handler" ""
          (*current-sdl-events-filter* userdata event))


;; Sphere environment
(parameterize
 ((current-directory
   (cond-expand (android "sdcard") (else "spheres"))))
 (load "core/lib/syntax-case.o1")
 (eval-file "core/src/base-macros.scm")
 (eval-file "core/src/assert-macros.scm")
 (SDL_Log "Successfully loaded environment"))

;; Initialize globals
(define (init-globals)
  (if (zero? (shell-command "wget localhost:8000/globals.scm -O assets/src/globals.scm"))
      (load "assets/src/globals.scm")
      (println "globals.scm could not be retrieved")))

;; Automatically load files
(init-globals)

;; Update the app Scheme source code
(define (update-app)
  (define (update-source source)
    (if (zero? (shell-command (string-append "wget localhost:8000/" source " -O assets/src/" source)))
        (load (string-append "assets/src/" source))
        (for-each (lambda (f) (f (string-append source " could not be retrieved")))
                  '(SDL_Log println))))
  ;; gl-utils.scm
  (update-source "gl-utils.scm")
  ;; app.scm
  (update-source "app.scm"))

;; Install and run the remote REPL: IP address of the computer running the debug server
(if (remote-repl-setup! "localhost" port: 20000)
    (begin
      (remote-repl-run!)
      (SDL_Log "***** Successfully connected to Gambit Debug Server *****"))
    (SDL_Log "***** Unable to connect to Gambit Debug Server. Are you running 'sense'? *****"))

;; Put the main thread to sleep
(thread-sleep! +inf.0)
