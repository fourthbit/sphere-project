(define (string-split sep)
        (lambda (str)
          (call-with-input-string
           str
           (lambda (p)
             (read-all p (lambda (p) (read-line p sep)))))))

(define (string-concatenate strings)
  (define (%string-copy! to tstart from fstart fend)
    (if (> fstart tstart)
        (do ((i fstart (+ i 1))
             (j tstart (+ j 1)))
            ((>= i fend))
          (string-set! to j (string-ref from i)))

        (do ((i (- fend 1)                    (- i 1))
             (j (+ -1 tstart (- fend fstart)) (- j 1)))
            ((< i fstart))
          (string-set! to j (string-ref from i)))))
  (let* ((total (do ((strings strings (cdr strings))
                     (i 0 (+ i (string-length (car strings)))))
                    ((not (pair? strings)) i)))
         (ans (make-string total)))
    (let lp ((i 0) (strings strings))
      (if (pair? strings)
          (let* ((s (car strings))
                 (slen (string-length s)))
            (%string-copy! ans i s 0 slen)
            (lp (+ i slen) (cdr strings)))))
    ans))

(define (string-join strings #!key (delim " ") (grammar 'infix))
        (let ((buildit (lambda (lis final)
                         (let recur ((lis lis))
                           (if (pair? lis)
                               (cons delim (cons (car lis) (recur (cdr lis))))
                               final)))))
          (cond ((pair? strings)
                 (string-concatenate
                  (case grammar
                    ((infix strict-infix)
                     (cons (car strings) (buildit (cdr strings) '())))
                    ((prefix) (buildit strings '()))
                    ((suffix)
                     (cons (car strings) (buildit (cdr strings) (list delim))))
                    (else (error "Illegal join grammar"
                                 grammar string-join)))))
                ((not (null? strings))
                 (error "STRINGS parameter not list." strings string-join))
                ((eq? grammar 'strict-infix)
                 (error "Empty list cannot be joined with STRICT-INFIX grammar."
                        string-join))
                (else ""))))







(define ios-directory
  (make-parameter "ios/"))

(define ios-source-directory-suffix
  (make-parameter "src/"))

(define ios-source-directory
  (make-parameter
   (string-append (ios-directory) (ios-source-directory-suffix))))

(define ios-build-directory-suffix
  (make-parameter "build/"))

(define ios-build-directory
  (make-parameter
   (string-append (ios-source-directory) (ios-build-directory-suffix))))

(define ios-assets-directory-suffix
  (make-parameter "assets/"))

(define ios-assets-directory
  (make-parameter (string-append (ios-directory) (ios-assets-directory-suffix))))

;; (define ios-link-file-incremental
;;   (make-parameter "linkfile_.c"))

;; (define ios-link-file-flat
;;   (make-parameter "linkfile_flat_.c"))

(define ios-device-sdk-directory
  (make-parameter
   (let* ((sdk-dir-process
           (open-process (list path: "tools/get_ios_sdk_dir" ;; XXX TODO: Move to Sphere Fusion
                               arguments: '("iPhoneOS"))))
          (result (read-line sdk-dir-process)))
     (unless (zero? (process-status sdk-dir-process))
             (err "fusion#compile-ios-app: error running script tools/get_ios_sdk_dir"))
     (close-input-port sdk-dir-process)
     result)))

(define ios-simulator-sdk-directory
  (make-parameter
   (let* ((sdk-dir-process
           (open-process (list path: "tools/get_ios_sdk_dir" ;; XXX TODO: Move to Sphere Fusion
                               arguments: '("iPhoneSimulator"))))
          (result (read-line sdk-dir-process)))
     (unless (zero? (process-status sdk-dir-process))
             (err "fusion#compile-ios-app: error running script tools/get_ios_sdk_dir"))
     (close-input-port sdk-dir-process)
     result)))


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
  (unless (file-exists? (ios-source-directory))
          (err "iOS source directory doesn't exist. Please run iOS setup task."))
  (when (null? (fileset dir: (ios-directory) test: (extension=? "xcodeproj")))
        (err "iOS Xcode project doesn't exist. Please run iOS setup task.")))

(define (fusion#ios-clean)
  (define (delete-if-exists dir)
    (when (file-exists? dir)
          (sake#delete-file dir recursive: #t force: #t)))
  (delete-if-exists (ios-assets-directory))
  (delete-if-exists (ios-build-directory))
  (delete-if-exists (string-append (ios-source-directory) "libspheres.a"))
  (delete-if-exists (string-append (ios-directory) "build/")))


(define (fusion#ios-link-incremental link-file
                                     modules
                                     #!key
                                     (verbose #f)
                                     (version '()))
  (info/color 'blue "generating incremental link file")
  (let* ((output-file (string-append (ios-build-directory) link-file))
         (code
          `((link-incremental
             ',(map (lambda (m) (string-append (ios-build-directory)
                                          (%module-filename-c m version: version)))
                    modules)
             output: ,output-file))))
    (if verbose (pp code))
    (unless (= 0 (gambit-eval-here code))
            (err "error generating Gambit incremental link file"))
    output-file))

(define (fusion#ios-link-flat link-file
                              modules
                              #!key
                              (verbose #f)
                              (version '()))
  (info/color 'blue (string-append "generating flat link file: " link-file))
  (let* ((output-file (string-append (ios-build-directory) link-file))
         (code
          `((link-flat
             ',(map (lambda (m) (string-append (ios-build-directory)
                                          (%module-filename-c m version: version)))
                    modules)
             output: ,output-file
             ;; with flat link files we remove warnings if non-verbose, since it includes used standard procedures too
             warnings?: ,verbose))))
    (if verbose (pp code))
    (unless (= 0 (gambit-eval-here code))
            (err "error generating Gambit flat link file"))
    output-file))


(define (fusion#ios-run-compiler #!key
                                 arch
                                 platform-type
                                 arguments
                                 (compiler 'gcc)
                                 (verbose #f))
  (let ((arch-str (symbol->string arch))
        (sdk-name (case platform-type ((device) "iphoneos") ((simulator) "iphonesimulator")))
        (ios-sdk-directory (case platform-type
                             ((device) (ios-device-sdk-directory))
                             ((simulator) (ios-simulator-sdk-directory)))))
    ;; Checks
    (unless (or (eq? platform-type 'simulator) (eq? platform-type 'device))
            (err "fusion#compile-ios-app: wrong platform-type"))
    (unless (or (eq? compiler 'gcc) (eq? compiler 'g++))
            (err "fusion#compile-ios-app: wrong compiler"))
    ;; Construct compiler strings
    (let* ((ios-cc-cli (string-append
                        "-sdk " sdk-name
                        " gcc"
                        " -isysroot " ios-sdk-directory
                        " -arch " arch-str
                        " -miphoneos-version-min=5.0"))
           (ios-cxx-cli (string-append
                         "-sdk " sdk-name
                         " g++"
                         " -isysroot " ios-sdk-directory
                         " -arch " arch-str
                         " -miphoneos-version-min=5.0"))
           (selected-compiler-cli (case compiler ((gcc) ios-cc-cli) ((g++) ios-cxx-cli))))
      (when verbose
            (info/color 'green "Compiler command:")
            (println selected-compiler-cli)
            (info/color 'green "Compiler args:")
            (println (string-join arguments)))
      (let ((compilation-process
             (open-process
              (list path: "xcrun"
                    arguments: (append ((string-split #\space) selected-compiler-cli)
                                       arguments)
                    environment:
                    (list (string-append "ARCH=" arch-str)
                          (string-append "CC=\"xcrun " ios-cc-cli "\"")
                          (string-append "CC=\"xcrun " ios-cxx-cli "\"")
                          (string-append "CFLAGS=\"-Wno-trigraphs -Wreturn-type -Wunused-variable\"")
                          "CXXFLAGS=\"-Wno-trigraphs -Wreturn-type -Wunused-variable\""
                          (string-append "LD=\"ld -arch " arch-str "\"")
                          "LDFLAGS=\"\"")))))
        (unless (zero? (process-status compilation-process))
                (err "fusion#ios-run-compiler: error running command"))))))

(define (fusion#ios-run-linker #!key
                                 arch
                                 platform-type
                                 arguments
                                 (verbose #f))
  (let ((arch-str (symbol->string arch))
        (sdk-name (case platform-type ((device) "iphoneos") ((simulator) "iphonesimulator")))
        (ios-sdk-directory (case platform-type
                             ((device) (ios-device-sdk-directory))
                             ((simulator) (ios-simulator-sdk-directory)))))
    ;; Checks
    (unless (or (eq? platform-type 'simulator) (eq? platform-type 'device))
            (err "fusion#compile-ios-app: wrong platform-type"))
    ;; Construct compiler strings
    (let ((ios-ld-cli (string-append
                       "-sdk " sdk-name
                       " ld"
                       " -syslibroot " ios-sdk-directory
                       " -arch " arch-str)))
      (when verbose
            (info/color 'green "Compiler command:")
            (println ios-ld-cli)
            (info/color 'green "Compiler args:")
            (println (string-join arguments)))
      (let ((compilation-process
             (open-process
              (list path: "xcrun"
                    arguments: (append ((string-split #\space) ios-ld-cli)
                                       arguments)
                    environment:
                    (list (string-append "ARCH=" arch-str)
                          (string-append "LD=\"ld -arch " arch-str "\"")
                          "LDFLAGS=\"\"")))))
        (unless (zero? (process-status compilation-process))
                (err "fusion#ios-run-linker: error running command"))))))


(define (fusion#ios-create-library-archive lib-name o-files #!key (verbose #f))
  (shell-command (string-append "ar r" (if verbose "cv " " ") lib-name " " (string-join o-files))))

;;! Compile App
;; .parameter main-module main-module of the Android App
;; .parameter import-modules modules already generated to be linked as well
(define (fusion#ios-compile-app main-module
                                #!key
                                arch
                                (cond-expand-features '())
                                (compiler-options '())
                                (version compiler-options)
                                (compiled-modules '())
                                (target 'debug)
                                (verbose #f))
  ;; Defines
  (##cond-expand-features (append '(mobile ios) (##cond-expand-features)))
  ;; Checks
  (fusion#ios-project-supported?)
  (unless arch (err "fusion#ios-compile-app: arch argument is mandatory"))
  (unless (or (eq? arch 'i386) (eq? arch 'armv7) (eq? arch 'armv7s))
          (err "fusion#ios-compile-app: wrong arch argument"))
  ;; Compute dependencies
  (let* ((modules-to-compile (append (%module-deep-dependencies-to-load main-module) (list main-module)))
         (all-modules (append compiled-modules modules-to-compile))
         (platform-type (case arch
                          ((i386) 'simulator)
                          ((armv7 armv7s) 'device)))
         (link-file "linkfile_.c"))
    ;; List files generated by compiling modules and the linkfile
    (let ((all-c-files
           (append (map (lambda (m) (string-append (ios-build-directory) (%module-filename-c m version: version))) all-modules)
                   (list (string-append (ios-build-directory) link-file)))))
      ;; Create iOS build directory if it doesn't exist
      (unless (file-exists? (ios-build-directory))
              (make-directory (ios-build-directory)))
      ;; Create iOS assets directory if it doesn't exist
      (unless (file-exists? (ios-assets-directory))
              (make-directory (ios-assets-directory)))
      ;; Generate modules (generates C code)
      (let ((something-generated? #f))
        (for-each
         (lambda (m)
           (let ((output-c-file (string-append (ios-build-directory) (%module-filename-c m version: version))))
             (if ((newer-than? output-c-file)
                  (string-append (%module-path-src m) (%module-filename-scm m)))
                 (begin
                   (set! something-generated? #t)
                   (sake#compile-to-c m
                                      cond-expand-features: (append cond-expand-features '(ios mobile))
                                      compiler-options: compiler-options
                                      verbose: verbose
                                      output: output-c-file)))))
         modules-to-compile)
        (when something-generated?
            (info/color 'blue "new C files generated")
            (fusion#ios-link-incremental link-file all-modules version: version))
        (set! something-generated? #f)
        (info/color 'green "compiling C/Scheme code into a static lib")
        (let ((o-files
               (map (lambda (f)
                      (let ((output-o-file (string-append (path-strip-extension f) ".o")))
                        (when ((newer-than? output-o-file) f)
                              (unless something-generated?
                                      (info/color 'blue "compiling updated C files:")
                                      (info/color 'purple (string-append " >>>  " f)))
                              (set! something-generated? #t)
                              (fusion#ios-run-compiler
                               arch: arch
                               platform-type: platform-type
                               arguments: `("-x"
                                            "objective-c"
                                            ,(string-append "-I" (ios-directory) "gambit/include")
                                            "-D___LIBRARY"
                                            ,(string-append "-I" (ios-source-directory))
                                            "-I/usr/local/Gambit-C/spheres/sdl2/deps/SDL2-2.0.3/include" ;;XXX TODO
                                            ;;,(string-append "-I" ios-sdk-directory "/System/Library/Frameworks/OpenGLES.framework/Headers") ;; XXX TODO
                                            ;; "-w" ;; XXX TODO
                                            ;; **************
                                            ;; **************
                                            ;; **************
                                            ;; **************
                                            "-c"
                                            ,f
                                            "-o"
                                            ,output-o-file)
                               compiler: 'gcc
                               verbose: verbose))
                        output-o-file))
                    all-c-files)))
          (fusion#ios-create-library-archive (string-append (ios-source-directory) "libspheres.a")
                                             o-files
                                             verbose: verbose))))
    (info/color 'blue "compiling iOS app")
    (parameterize
     ((current-directory (ios-directory)))
     (shell-command (string-append
                     (xcodebuild-path) " build -configuration Debug -sdk "
                     (case platform-type ((device) "iphoneos") ((simulator) "iphonesimulator"))
                     " -arch " (symbol->string arch))))))

(define (fusion#ios-compile-loadable-set output-file
                                         main-module
                                         #!key
                                         arch
                                         (cond-expand-features '())
                                         (compiler-options '())
                                         (version compiler-options)
                                         (compiled-modules '())
                                         (target 'debug)
                                         (merge-modules #f)
                                         (verbose #f))
  ;; Defines
  (##cond-expand-features (append '(mobile ios) (##cond-expand-features)))
  ;; Checks
  (fusion#ios-project-supported?)
  (unless arch (err "fusion#ios-compile-loadable-set: arch argument is mandatory"))
  (unless (or (eq? arch 'i386) (eq? arch 'armv7) (eq? arch 'armv7s))
          (err "fusion#ios-compile-loadable-set: wrong arch argument"))
  ;; Compute dependencies
  (let* ((modules-to-compile (append (%module-deep-dependencies-to-load main-module) (list main-module)))
         (all-modules (append compiled-modules modules-to-compile))
         (platform-type (case arch
                          ((i386) 'simulator)
                          ((armv7 armv7s) 'device)))
         (link-file (string-append output-file ".c")))
    ;; List files generated by compiling modules and the linkfile
    (let ((all-c-files
           (append (map (lambda (m) (string-append (ios-build-directory) (%module-filename-c m version: version))) all-modules)
                   (list (string-append (ios-build-directory) link-file)))))
      ;; Create iOS build directory if it doesn't exist
      (unless (file-exists? (ios-build-directory))
              (make-directory (ios-build-directory)))
      ;; Create iOS assets directory if it doesn't exist
      (unless (file-exists? (ios-assets-directory))
              (make-directory (ios-assets-directory)))
      ;; Generate modules (generates C code)
      (let ((something-generated? #f))
        (for-each
         (lambda (m)
           (let ((output-c-file (string-append (ios-build-directory) (%module-filename-c m version: version))))
             (if ((newer-than? output-c-file)
                  (string-append (%module-path-src m) (%module-filename-scm m)))
                 (begin
                   (set! something-generated? #t)
                   (sake#compile-to-c m
                                      cond-expand-features: (append cond-expand-features '(ios mobile))
                                      compiler-options: compiler-options
                                      verbose: verbose
                                      output: output-c-file)))))
         modules-to-compile)
        (when something-generated?
            (info/color 'blue "new C files generated")
            (fusion#ios-link-flat link-file all-modules version: version))
        ;; Compile objects
        (set! something-generated? #f)
        (let ((o-files
               (map (lambda (f)
                      (let* ((output-o-file (string-append (path-strip-extension f) ".o"))
                             (args `("-x"
                                     "objective-c"
                                     ,(string-append "-I" (ios-directory) "gambit/include")
                                     "-D___DYNAMIC"
                                     ,(string-append "-I" (ios-source-directory))
                                     ;;"-I/usr/local/Gambit-C/spheres/sdl2/deps/SDL2-2.0.3/include" ;;XXX TODO
                                     ;;,(string-append "-I" ios-sdk-directory "/System/Library/Frameworks/OpenGLES.framework/Headers") ;; XXX TODO
                                     ;; "-w" ;; XXX TODO
                                     ;; **************
                                     ;; **************
                                     ;; **************
                                     ;; **************
                                     "-c"
                                     ,f
                                     "-o"
                                     ,output-o-file)))
                        (when ((newer-than? output-o-file) f)
                              (unless something-generated?
                                      (info/color 'blue "compiling updated C files:")
                                      (info/color 'purple (string-append " >>>  " f)))
                              (set! something-generated? #t)
                              (fusion#ios-run-compiler
                               arch: arch
                               platform-type: platform-type
                               compiler: 'gcc
                               arguments: args
                               verbose: verbose))
                        output-o-file))
                    all-c-files)))
          ;; Make bundle
          (info/color 'green "compiling C/Scheme code into a loadable object")
          (fusion#ios-run-linker
           arch: arch
           platform-type: platform-type
           ;; Flags not included:
           ;; "-lcrt1.o" This one is for the executables
           arguments: `( ;; TODO XXX         MOVE ESSENTIAL TO PROCEDURE
                        "-bundle"
                        "-demangle"
                        "-dynamic"
                        "-ObjC"
                        "-all_load"
                        "-dead_strip"
                        "-ios_simulator_version_min" "5.0"
                        "-objc_abi_version" "2"
                        "-no_implicit_dylibs"
                        "-framework" "Foundation"
                        "-framework" "UIKit"
                        "-lobjc"
                        "-lSystem"
                        ;;"-Lios/gambit/lib"
                        ;;"-lgambc"
                        ;;"-demangle -dynamic -arch i386 -all_load -dead_strip -ios_simulator_version_min 7.1.0 -syslibroot /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator7.1.sdk -ObjC -o /Users/Alvaro/Library/Developer/Xcode/DerivedData/SchemeSpheres-cplbjdvmioedvvdtudhkdgprgyrw/Build/Products/Debug-iphonesimulator/SchemeSpheres.app/SchemeSpheres -lcrt1.o -L/Users/Alvaro/Library/Developer/Xcode/DerivedData/SchemeSpheres-cplbjdvmioedvvdtudhkdgprgyrw/Build/Products/Debug-iphonesimulator -L/Users/Alvaro/Dropbox/working/DaTest/ios/SDL/lib -L/Users/Alvaro/Dropbox/working/DaTest/ios/gambit -L/Users/Alvaro/Dropbox/working/DaTest/ios/gambit/lib -L/Users/Alvaro/Dropbox/working/DaTest/ios/src -filelist /Users/Alvaro/Library/Developer/Xcode/DerivedData/SchemeSpheres-cplbjdvmioedvvdtudhkdgprgyrw/Build/Intermediates/SchemeSpheres.build/Debug-iphonesimulator/SchemeSpheres.build/Objects-normal/i386/SchemeSpheres.LinkFileList -objc_abi_version 2 -no_implicit_dylibs -framework Foundation -framework UIKit -framework OpenGLES -framework QuartzCore -framework CoreAudio -framework AudioToolbox -framework CoreGraphics -lSDL2 -lgambc -dependency_info /Users/Alvaro/Library/Developer/Xcode/DerivedData/SchemeSpheres-cplbjdvmioedvvdtudhkdgprgyrw/Build/Intermediates/SchemeSpheres.build/Debug-iphonesimulator/SchemeSpheres.build/Objects-normal/i386/SchemeSpheres_dependency_info.dat -framework Foundation -lobjc -lSystem /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/5.1/lib/darwin/libclang_rt.ios.a -F/Users/Alvaro/Library/Developer/Xcode/DerivedData/SchemeSpheres-cplbjdvmioedvvdtudhkdgprgyrw/Build/Products/Debug-iphonesimulator"
                        ,@o-files
                        "-o"
                        ,(string-append (ios-directory) output-file))
           verbose: verbose))))))














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
        ;; Compile the main module and its dependencies as a loadable object for the ARM
        ;; arch.  The (load) function takes care of loading code dinamically, both compiled
        ;; and source code. This can be used during Android development in the following ways:
        ;; - Uploading the code to the SD card
        ;; - Bundling the code within the APK
        ;; - Dynamically running with the Remote Debugger in Emacs or the terminal
        (fusion#compile-loadable-set "main_arm.o1" 'main
                                     merge-modules: #f
                                     target: 'debug
                                     arch: 'android-arm
                                     cond-expand-features: '(debug)
                                     compiler-options: '(debug))
        ;; Compile the Android app with just the loader code
        (fusion#android-compile-app "my-app" 'loader
                                    target: 'debug
                                    cond-expand-features: '(ios debug)
                                    compiler-options: '(debug)))))

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
  (if #f ;; #t to compile as a single app executable
      ;; Compile all modules within the app executable
      (fusion#ios-compile-app 'main
                              arch: 'i386 ;; armv7 / armv7s
                              target: 'debug
                              cond-expand-features: '(ios debug)
                              compiler-options: '(debug)
                              verbose: #t)
      (begin
        ;; Compile the main module and its dependencies as a loadable object, for all iOS
        ;; archs. The (load) function takes care of loading code dinamically, both compiled
        ;; and source code. This can be used during iOS development in the following ways:
        ;; - Uploading code to the Resources folder (part of the app bundle)
        ;; - Uploading code to the Documents folder (created at runtime, must be uploaded when the app is running)
        ;; - Dynamically running with the Remote Debugger in Emacs or the terminal
        (fusion#ios-compile-loadable-set "main.o1" 'main
                                         merge-modules: #f
                                         target: 'debug
                                         arch: 'i386
                                         cond-expand-features: '(ios debug)
                                         compiler-options: '(debug)
                                         verbose: #t)
        #;
        (fusion#ios-compile-loadable-set "main_arm7" 'main
                                         merge-modules: #f
                                         target: 'debug
                                         arch: 'armv7
                                         cond-expand-features: '(debug)
                                         compiler-options: '(debug))
        #;
        (fusion#ios-compile-loadable-set "main_arm7s" 'main
                                         merge-modules: #f
                                         target: 'debug
                                         arch: 'armv7s
                                         cond-expand-features: '(debug)
                                         compiler-options: '(debug))
        #;
        (fusion#make-ios-fat-lib ...)
        ;; Compile the iOS app with just the loader module
        ;; The loader will decide which object to load according to the runtime architecture
        (fusion#ios-compile-app 'loader
                                arch: 'i386 ;; armv7 / armv7s
                                target: 'debug
                                cond-expand-features: '(ios debug)
                                compiler-options: '(debug)
                                verbose: #t))))

(define-task ios:run ()
  (parameterize
   ((current-directory (ios-directory)))
   (shell-command
    (string-append
     (ios-sim-path) " launch build/Debug-iphonesimulator/SchemeSpheres.app"))))

(define-task ios:xcode ()
  (shell-command "open -a Xcode ios/SchemeSpheres.xcodeproj"))

(define-task ios:clean ()
  (fusion#ios-clean))

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
                                     s                                     target: 'debug
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

    clean                     Clean all targets

end-of-help
)

(define-task clean (android:clean ios:clean host:clean)
  'clean)

(define-task all ()
  (println help))
