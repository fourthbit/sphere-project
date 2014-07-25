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

;;(SDL_Log (current-directory))
;;(SDL_Log (object->string (directory-files)))

;; Load main module dynamically

(define eval-include
  (lambda (file)
    (for-each eval (with-input-from-file file read-all))))

(parameterize
 ((current-directory (cond-expand (android "sdcard") (else "spheres"))))
 (load "syntax-case.o1")
 (eval-include "base-macros.scm")
 (eval-include "assert-macros.scm")
 (SDL_Log "Successfully loaded environment"))

(define (update-app)
  (if (zero? (shell-command "wget -N localhost:8000/app.scm"))
      (load "app.scm")
      (println "App not updated")))





(define window #f)
(define screen-width)
(define screen-height)




;; Install and run the remote REPL: IP address of the computer running the debug server
(if (remote-repl-setup! "localhost" port: 20000)
    (begin
      (remote-repl-run!)
      (SDL_Log "***** Successfully connected to Gambit Debug Server *****"))
    (SDL_Log "***** Unable to connect to Gambit Debug Server. Are you running 'sense'? *****"))

(SDL_Log "***** Sleeping *****")

(thread-sleep! +inf.0)

