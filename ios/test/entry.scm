(define log
  (c-lambda (char-string)
            void
            "printf(\"%s\",___arg1);"))
            
(log "Hello Obj-C World!")
