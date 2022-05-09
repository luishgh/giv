(define-module (guix extensions giv)
  #:use-module (guix scripts)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (guix build utils)

  #:use-module (ice-9 match)

  #:export (guix-giv))


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
  (todo!))


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
