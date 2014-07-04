;; Main module of the application
;;(println "Hello world!")


(SDL_Log "Hello world!")
(SDL_Log (object->string (directory-files)))
(SDL_Log "Works well, apparently")

(thread-sleep! 1000)
