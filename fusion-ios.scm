;;; Copyright (c) 2014 by Álvaro Castro Castilla
;;; Extensions for Sake, to use with Fusion projects


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
           (open-process (list path: (string-append (%sphere-path 'fusion) "tools/get-ios-sdk-dir")
                               arguments: '("iPhoneOS"))))
          (result (read-line sdk-dir-process)))
     (unless (zero? (process-status sdk-dir-process))
             (err "fusion#compile-ios-app: error running script tools/get_ios_sdk_dir"))
     (close-input-port sdk-dir-process)
     result)))

(define ios-simulator-sdk-directory
  (make-parameter
   (let* ((sdk-dir-process
           (open-process (list path: (string-append (%sphere-path 'fusion) "tools/get-ios-sdk-dir")
                               arguments: '("iPhoneSimulator"))))
          (result (read-line sdk-dir-process)))
     (unless (zero? (process-status sdk-dir-process))
             (err "fusion#compile-ios-app: error running script tools/get_ios_sdk_dir"))
     (close-input-port sdk-dir-process)
     result)))


;;------------------------------------------------------------------------------
;;!! Toolchain

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


;;------------------------------------------------------------------------------
;;!! iOS support procedures

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

;;! Default clean for iOS
(define (fusion#ios-clean)
  (define (delete-if-exists dir)
    (when (file-exists? dir)
          (sake#delete-file dir recursive: #t force: #t)))
  (delete-if-exists (ios-assets-directory))
  (delete-if-exists (ios-build-directory))
  (delete-if-exists (string-append (ios-source-directory) "libspheres.a"))
  (delete-if-exists (string-append (ios-directory) "build/")))

;;! Generate incremental link file for iOS given a set of modules
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

;;! Generate flat link file for iOS given a set of modules
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

;;! Runs the compiler with the iOS environment
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

;;! Runs the linker with the iOS environment
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

;;! Create an archive (static library) for iOS given a set of object files
(define (fusion#ios-create-library-archive lib-name o-files #!key (verbose #f))
  (shell-command (string-append "ar r" (if verbose "cv " " ") lib-name " " (string-join o-files))))

;;! Compile App
;; .parameter main-module main-module of the Android App
;; .parameter arch: target architecture
(define (fusion#ios-compile-app main-module
                                #!key
                                arch
                                (cond-expand-features '())
                                (compiler-options '())
                                (version compiler-options)
                                (compiled-modules '())
                                (target 'debug)
                                (verbose #f))
  ;; Cond-expand features (relevant within the Sake environment)
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

;;! Generate a loadable object from a module and its dependencies for iOS
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
  ;; Cond-expand features (relevant within the Sake environment)
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
