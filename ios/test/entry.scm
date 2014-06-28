(c-declare #<<end-c-declare

#include "stdio.h"
#import "UIDevice+Hardware.h"

end-c-declare
)

(define ios-device (c-lambda () int #<<end-c-lambda

UIDevice *h=[[UIDevice alloc] init];
int r = [h hardware];
[h release];
___result = r;
                
end-c-lambda
))

(define ios-device-description (c-lambda () nonnull-char-string #<<end-c-lambda

UIDevice *h=[[UIDevice alloc] init];
char *c = [[h hardwareDescription] UTF8String];
[h release];
___result = c;
                
end-c-lambda
))

(define printf (c-lambda (char-string) void "printf(\"%s\",___arg1);"))

(printf (string-append (object->string (system-type)) "\n"))

(printf (string-append (ios-device-description) "\n"))
(printf (string-append (number->string (ios-device)) "\n"))

(cond
 ((file-exists? "app.o1")
  (load "app.o1"))
 ((file-exists? "app.scm")
  (load "app.scm"))
 (else
  (printf (string-append "app.scm / app.o1 :: not found in " (current-directory) "\n"))))
