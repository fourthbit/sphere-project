(sphere: "my-app")
(dependencies:
 (loader
  (include
   (core: base-macros))
  (load
   (energy: remote/debuggee)
   (fusion: spheres-remote)
   (sdl2: sdl2)
   (sdl2: sdl2-image)
   (cond-expand
    ((or android ios)
     (opengl: gl-es2)
     (fusion: ios))
    (else
     (opengl: gl)))
   (math: matrix)
   (fabric: algorithm/list)))
 (app
  (include
   (core: base-macros)
   (core: assert-macros)
   (= engine-types))
  (load
   (= globals)
   (= gl-utils)
   (= engine))))
