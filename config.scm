(sphere: "my-app")
(dependencies:
 (loader
  (include
   (core: ffi-macros))
  (prelude
   (core: ffi-prelude)
   (sdl2: sdl2-prelude))
  (load
   (energy: remote/debuggee)
   (opengl: gl-es2)
   (sdl2: sdl2)
   (sdl2: sdl2-image)
   (math: matrix)
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
