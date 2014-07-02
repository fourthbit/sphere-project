(cond-expand
 (android
  #!void)
 
 (ios
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
                                           const char *c = [[h hardwareDescription] UTF8String];
                                           [h release];
                                           ___result = (char *) c;
end-c-lambda
  ))
  (define printf (c-lambda (char-string) void "printf(\"%s\",___arg1);"))
  (printf "!@#$!@%@#$%@#$^#$^#$%!#@$!@$!@#$!@$!#$%#$^#$%^!%@#%$&$%^&#$^!#@$!@#$$^#%&#@#%!@#$!&!@%#@$^&#@^!@#$@#^#$^@#$%!@#%\n")
  (printf (string-append "iOS Hardware: "(ios-device-description) "\n"))
  (printf (string-append (number->string (ios-device)) "\n")))
 
 (else
  #!void))


