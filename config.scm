(sphere: "my-app")
(dependencies:
 (main2
  (include
   (core: base-macros)
   (core: assert-macros))
  (load
   (core: ffi)
   (energy: remote/debuggee)
   (math: matrix)
   (fusion: core))))
