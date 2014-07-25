;; Minimal debug server

(SDL_Log "***** Initializing Gambit Debug Server *****")
(if (remote-repl-setup! "192.168.1.36" port: 20000)
      (begin
       (remote-repl-run!)
       (SDL_Log "***** Successfully connected to Gambit Debug Server *****"))
      (SDL_Log "***** Unable to connect to Gambit Debug Server. Are you running 'sense'? *****"))

(thread-sleep! +inf.0)
