(define-cond-expand-feature compile-to-c)
(define-cond-expand-feature debug)
(define-cond-expand-feature android)
(define-cond-expand-feature mobile)
(cond-expand
 (android (c-define (c-scheme-main) () void "scheme_main" "" (main)))
 (host (main))
 (else #!void))
