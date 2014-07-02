(define ios-directory
  (make-parameter "ios/"))

(define ios-src-directory-suffix
  (make-parameter "src/"))

(define ios-src-directory
  (make-parameter
   (string-append (ios-directory) (ios-src-directory-suffix))))

(define ios-build-directory-suffix
  (make-parameter "build/"))

(define ios-build-directory
  (make-parameter
   (string-append (ios-src-directory) (ios-build-directory-suffix))))

(define ios-assets-directory-suffix
  (make-parameter "assets/"))

(define ios-assets-directory
  (make-parameter (string-append (ios-directory) (ios-assets-directory-suffix))))

(define ios-link-file
  (make-parameter "linkfile_.c"))



;;------------------------------------------------------------------------------
;;!! Host programs

(define xcodebuild-path
  (make-parameter
   (if (zero? (shell-command "xcodebuild -usage &>/dev/null"))
       "xcodebuild"
       #f)))

(define ios-sim-path
  (make-parameter
   (if (zero? (shell-command "ios-sim --version &>/dev/null"))
       "ios-sim"
       #f)))






;;! Check whether the project seems to be prepared for Android
(define (fusion#ios-project-supported?)
  (unless (file-exists? (ios-directory))
          (err "iOS directory doesn't exist. Please run iOS setup task."))
  (when (null? (directory-files (ios-directory)))
        (err "iOS directory doesn't seem to have anything. Please run iOS setup task."))
  (unless (file-exists? (ios-src-directory))
          (err "iOS src/ directory doesn't exist. Please run iOS setup task."))
  (when (null? (fileset dir: (ios-directory) test: (extension=? "xcodeproj")))
        (err "iOS Xcode project doesn't exist. Please run iOS setup task.")))



(define (fusion#generate-link-file modules #!key (verbose #f) (version '()))
  (info/color 'blue "generating link file")
  (let* ((output-file (string-append (android-build-directory) (android-link-file)))
         (code
          `((link-incremental ',(map (lambda (m) (string-append (android-build-directory)
                                                           (%module-filename-c m version: version)))
                                     modules)
                              output: ,output-file))))
    (if verbose (pp code))
    (unless (= 0 (gambit-eval-here code))
            (err "error generating Gambit link file"))))




;;! Compile App
;; .parameter main-module main-module of the Android App
;; .parameter import-modules modules already generated to be linked as well
(define (fusion#ios-compile-app main-module
                                #!key
                                (cond-expand-features '())
                                (compiler-options '())
                                (version compiler-options)
                                (compiled-modules '())
                                (target 'debug)
                                (verbose #f))
  ;; Defines
  (##cond-expand-features (append '(mobile android) (##cond-expand-features)))
  ;; Checks
  (fusion#ios-project-supported?)
  ;; Compute dependencies
  (let* ((modules-to-compile (append (%module-deep-dependencies-to-load main-module) (list main-module)))
         (all-modules (append compiled-modules modules-to-compile)))
    ;; List files generated by compiling modules and the linkfile
    (let ((all-c-files
           (append (map (lambda (m) (string-append (ios-build-directory-suffix) (%module-filename-c m version: version))) all-modules)
                   (list (string-append (ios-build-directory-suffix) (android-link-file))))))
      ;; Create Android build directory if it doesn't exist
      (unless (file-exists? (ios-build-directory))
              (make-directory (ios-build-directory)))
      ;; Create Android assets directory if it doesn't exist
      (unless (file-exists? (ios-assets-directory))
              (make-directory (ios-assets-directory)))
      ;; Generate modules (generates C code)
      (let ((something-generated? #f))
        (for-each
         (lambda (m)
           (if ((newer-than? (string-append (ios-build-directory)
                                            (%module-filename-c m version: version)))
                (string-append (%module-path-src m) (%module-filename-scm m)))
               (begin
                 (set! something-generated? #t)
                 (sake#compile-to-c m
                                    cond-expand-features: (append cond-expand-features '(ios mobile))
                                    compiler-options: compiler-options
                                    verbose: verbose))))
         modules-to-compile)
        (if something-generated?
            (info/color 'blue "C files generated")
            (info/color 'blue "no Scheme files needed recompilation"))
        ;; Copy C files from both compiled and imported modules into build directory (if updated)
        (for-each
         (lambda (m) (let ((source (string-append (current-build-directory)
                                             (%module-filename-c m version: version)))
                      (destination (string-append (ios-build-directory)
                                                  (%module-filename-c m version: version))))
                  (if ((newer-than? destination) source)
                      (copy-file source destination))))
         all-modules)
        ;; Generate link file
        (if something-generated?
            (fusion#generate-link-file all-modules version: version))))
    
    (info/color 'blue "compiling C code into a static lib"))
    ;; TODO
  
    (info/color 'blue "compiling iOS app")
    (parameterize
     ((current-directory (ios-directory)))
     (shell-command (string-append (xcodebuild-path) " build -configuration Debug"))))














;;----------------------------------------------------------------------------------------
;;!! Android tasks

(define-task android:setup ()
  ;; Set up Android project files
  (fusion#android-project-set-target "android-15")
  ;; Create symlink to SDL library from SDL2 Sphere
  (let ((SDL-link (string-append (android-jni-generator-directory) "deps/SDL")))
    (unless (file-exists? SDL-link)
            (create-symbolic-link (string-append (%sphere-path 'sdl2) "deps/SDL2-2.0.3") SDL-link))))

(define-task android:compile ()
  (if #f ;; #t to compile as a single app executable
      ;; Compile all modules within the app executable
      (fusion#android-compile-app "main" 'main
                                  target: 'debug
                                  cond-expand-features: '(debug)
                                  compiler-options: '(debug))
      (begin
        ;; Compile the Android app with just the loader code
        (fusion#android-compile-app "my-app" 'loader
                                    target: 'debug
                                    cond-expand-features: '(ios debug)
                                    compiler-options: '(debug))
        ;; Compile the main module and its dependencies as a loadable object for the ARM
        ;; arch.  The (load) function takes care of loading code dinamically, both compiled
        ;; and source code. This can be used during Android development in the following ways:
        ;; - Uploading the code to the SD card
        ;; - Bundling the code within the APK
        ;; - Dynamically running with the Remote Debugger in Emacs or the terminal
        (fusion#compile-loadable-set "main_arm" 'main
                                     merge-modules: #f
                                     target: 'debug
                                     arch: 'android-arm
                                     cond-expand-features: '(debug)
                                     compiler-options: '(debug))
        (fusion#android-upload-file "main_arm.o1"))))

(define-task android:install ()
  (fusion#android-install 'debug))

(define-task android:run ()
  ;; Run the Activity
  (fusion#android-run "org.libsdl.app/org.libsdl.app.SDLActivity")
  ;; Log cat
  (shell-command (string-append (android-adb-path) " logcat *:S *:F SchemeSpheres SDL SDL/APP")))

(define-task android (android:compile android:install android:run)
  'android)

(define-task android:clean ()
  (fusion#android-clean))

;;----------------------------------------------------------------------------------------
;;!! iOS tasks

(define-task ios:setup ()
  ;; Create symlink to SDL include library from SDL2 Sphere
  (let ((SDL-link (string-append (ios-directory) "SDL/include")))
    (unless (file-exists? SDL-link)
            (create-symbolic-link (string-append (%sphere-path 'sdl2) "deps/SDL2-2.0.3/include")
                                  SDL-link))))

(define-task ios:compile ()
  (if #t ;; #t to compile as a single app executable
      ;; Compile all modules within the app executable
      (fusion#ios-compile-app 'main
                              target: 'debug
                              cond-expand-features: '(ios debug)
                              compiler-options: '(debug))
      (begin
        ;; Compile the iOS app with just the loader module
        ;; The loader will decide which object to load according to the runtime architecture
        (fusion#ios-compile-app 'loader
                                target: 'debug
                                cond-expand-features: '(ios debug)
                                compiler-options: '(debug))
        ;; Compile the main module and its dependencies as a loadable object, for all iOS
        ;; archs. The (load) function takes care of loading code dinamically, both compiled
        ;; and source code. This can be used during iOS development in the following ways:
        ;; - Uploading code to the Resources folder (part of the app bundle)
        ;; - Uploading code to the Documents folder (created at runtime, must be uploaded when the app is running)
        ;; - Dynamically running with the Remote Debugger in Emacs or the terminal
        (fusion#compile-loadable-set "main_i386" 'main
                                     merge-modules: #f
                                     target: 'debug
                                     arch: 'ios-simulator
                                     cond-expand-features: '(debug)
                                     compiler-options: '(debug))
        (fusion#compile-loadable-set "main_arm7" 'main
                                     merge-modules: #f
                                     target: 'debug
                                     arch: 'arm7
                                     cond-expand-features: '(debug)
                                     compiler-options: '(debug))
        (fusion#compile-loadable-set "main_arm7s" 'main
                                     merge-modules: #f
                                     target: 'debug
                                     arch: 'arm7s
                                     cond-expand-features: '(debug)
                                     compiler-options: '(debug)))))

(define-task ios:run ()
  (parameterize
   ((current-directory (ios-directory)))
   (shell-command
    (string-append
     (ios-sim-path) " launch build/Debug-iphoneos/SchemeSpheres.app --debug"))))

(define-task ios:xcode ()
  (shell-command "open -a Xcode ios/SchemeSpheres.xcodeproj"))

(define-task ios:clean ()
  'clean)

(define-task ios (ios:compile ios:run)
  'ios)

;;----------------------------------------------------------------------------------------
;;!! Host (Linux/OSX) tasks

(define-task host:run ()
  (fusion#host-run-interpreted 'main)) 

(define-task host:compile ()
  ;; Note (merge-modules): If #t this will include all dependencies in one big file before compiling to C
  ;; Note (compile-loadable-set): this must be linked flat
  (if #f ;; #t to compile the application as a single standalone
      ;; Bundle a single executable
      (fusion#host-compile-exe "my-application-standalone" 'main
                               merge-modules: #f)
      (begin 
        ;; Compile as a loader and a loadable library
        (fusion#host-compile-exe "my-application" 'loader
                                 target: 'debug
                                 cond-expand-features: '(host debug)
                                 compiler-options: '(debug))
        ;; Compile the main module and its dependencies as a loadable object. The (load)
        ;; function takes care of loading code dinamically, both compiled and source code.
        (fusion#compile-loadable-set "main" 'main
                                     merge-modules: #f
                                     target: 'debug
                                     arch: 'host
                                     cond-expand-features: '(debug)
                                     compiler-options: '(debug)))))

(define-task host:clean ()
  (sake#default-clean))

(define-task host (host:run)
  'host)

;;----------------------------------------------------------------------------------------
;;!! General

(define help #<<end-of-help
  
  Tasks (run with 'sake <task>')
  ------------------------------
  
  android:setup             Setup Android project before running other tasks
  android:compile           Compile the Android app
  android:install           Install App in current Android device (hardware or emulated)
  android:run               Run App in current Android device
  android:clean             Clean all Android generated files
  android                   Execute compile, install, run

  ios:setup                 Setup iOS project before running other tasks
  ios:compile               Compile the iOS app
  ios:run                   Launch the iOS Simulator and run the app
  ios:xcode                 Open the iOS project in Xcode
  ios:clean                 Clean all iOS generated files
  ios                       Execute compile and run
  
  host:compile              Compile the host program as standalone
  host:run                  Run the host OS (Linux/OSX) program interpreted
  host:clean                Clean the generated host program files
  host                      Defaults to host:run

end-of-help
)

(define-task all ()
  (println help))
