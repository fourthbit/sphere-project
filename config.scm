(sphere: "my-app")
(dependencies:
 (loader
  (load
   (sdl2: sdl2)))
 (main
  (include
   (core: base-macros)
   (core: assert-macros))
  (load
   (energy: remote/debuggee)
   (math: matrix)
   ;;(fusion: core)
   )))
