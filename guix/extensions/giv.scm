(define-module (guix extensions giv)
  #:use-module (guix scripts download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix scripts)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (guix build utils)
  #:use-module (guix build-system copy)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)

  #:export (guix-giv))


;;;
;;; Library.
;;;

(define (guix-download-wrapper url)
  ;; TODO: uncomment this
  ;; (info (G_ "computing hash for ~a...~%") url)
  (let* ((port (mkstemp "/tmp/giv-XXXXXX"))
         (ret (begin
                (cadr (string-split
                       (with-output-to-string
                         (lambda _ (guix-download url)))
                       #\newline)))))

    ;; Janitoring
    (delete-file (port-filename port))
    (close-port port)
    ret))

(define (dependency-url->sha256 dependency-url)
  (guix-download-wrapper dependency-url))

(define (dependency->source-package dependency)
  (let-keywords dependency #f (name
                               url
                               type)
                (unless (eq? type 'tarball)
                  (error (format #f "Only tarballs are supported ATM! You provided: ~A" type)))
                (package
                  ;; Dondle metadata
                  (name (symbol-append name '-giv))
                  ;; TODO: truly deal with this
                  (version "0.0.0-giv") ;; at first, versions shouldn't matter
                  (synopsis (string-append "giv source: " (symbol->string name)))
                  (description "A giv source locked.")
                  (license license:non-copyleft) ;; TODO: what should I do here?
                  (home-page "")
                  ;; Real shit
                  ;; TODO: support something besides tarballs
                  (build-system copy-build-system)
                  (source (origin
                            (method url-fetch)
                            (uri url)
                            (sha256
                             (base32 (dependency-url->sha256 url))))))))

(define (source-origin->string origin)
  (format #f
          "(origin
(method ~a)
(uri \"~a\")
(sha256
 ~a))"
          (string->symbol (string-delete #\* (symbol->string (procedure-name (origin-method origin)))))
          (origin-uri origin)
          (content-hash-value (origin-hash origin))))

(define (source-package->package-string package)
  (format #f
          "(package
(name ~a)
(version \"~a\")
(synopsis \"~a\")
(description \"~a\")
(license ~a)
(home-page \"\")
(build-system ~a)
(source ~a))"
          (symbol->string (package-name package))
          (package-version package)
          (package-synopsis package)
          (package-description package)
          "license:gpl3" ;; TODO: oh well, licences causing trouble again
          "copy-build-system"
          (source-origin->string (package-source package))))


(define (dependency->locked-channel-package dependency)
  (symbol->string
   (cadr dependency)))

(define (dependency->locked-source-package dependency)
  (source-package->package-string
   (dependency->source-package dependency)))

(define (dependency->locked-dependency dependency)
  (if (eq? (car dependency) 'package)
      (dependency->locked-channel-package dependency)
      (dependency->locked-source-package dependency)))

;; ((:name foo) (package bar))
(define (lock-dependencies dependencies)
  (map dependency->locked-dependency dependencies))


;;;
;;; Helpers.
;;;

(define-syntax-rule (todo! ...)
  (leave (G_ "not implemented yet!~%")))


;;;
;;; Subcommandas.
;;;

;; TODO: Support arguments
(define (bootstrap-project)
  ;; Bootstrap current dir as a Guix project
  (mkdir (string-append (getcwd) "/giv"))
  (let ((port (open-output-file "giv/sources.scm")))
    ;; TODO: add giv as project dependency and choose guix channel commit
    (display "Sources(?)\n" port))
  (let ((port (open-output-file "giv/sources.lock")))
    ;; TODO: actually lock the sources.scm to real package definitions
    (display "WE'RE LOCKED (I guess)!\n" port)))


;;;
;;; Command-line options.
;;;

(define (show-subcommands)
  (display (G_ "Available commands:\n"))
  (display (G_ "
    init    bootstrap a Guix project")))

(define (show-flags)
  (display (G_ "
  -m, --manifest=FILE    create project with the manifest from FILE"))
  (display (G_ "
  -n, --dry-run          do not actually build the modules"))

  (newline)

  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit")))

(define (show-help)
  (display (G_ "Usage: guix giv COMMAND [OPTION]...
Easy dependency management for Guix projects.\n"))
  (newline)
  (show-subcommands)
  (newline)
  (show-flags)
  (newline))

(define-command (guix-giv . args)
  (category development)
  (synopsis "TODO")
  (match args
    (("init" args ...)
     (bootstrap-project))
    ((or ("-h") ("--help"))
     (show-help)
     (exit 0))
    ((command _ ...)
     (leave (G_ "~a: unknown sub-command~%") command))
    (()
     (leave (G_ "missing sub-command~%")))))
