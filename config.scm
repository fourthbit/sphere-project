(sphere: "my-app")
(dependencies:
 (loader
  (load
   (energy: remote/debuggee)
   (opengl: gl-es2)
   (math: matrix)
   (sdl2: sdl2)
   (sdl2: sdl2-image)
   (fusion: core)))
 (main-minimal
  (load
   (energy: remote/debuggee)))
 (main
  (include
   (core: base-macros)
   (core: assert-macros))
  (load
   (energy: remote/debuggee)
   (math: matrix)
   (fusion: core))))
