(sphere: "my-app")
(dependencies:
 (loader
  (load
   (energy: remote/debuggee)
   (opengl: gl-es2)
   (sdl2: sdl2)
   (sdl2: sdl2-image)
   (math: matrix)
   (fusion: ios)
   (fabric: algorithm/list)))
 (app
  (include
   (core: base-macros)
   (core: assert-macros)
   (= engine-type))
  (load
   (= globals)
   (= gl-utils)
   (= engine)))
 (main-minimal
  (load
   (energy: remote/debuggee)))
 (main
  (include
   (core: base-macros)
   (core: assert-macros))
  (load
   (energy: remote/debuggee)
   (math: matrix))))
