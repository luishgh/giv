(define-module (guix extensions giv)
  #:use-module (guix channels)
  #:use-module (guix describe)
  #:use-module (guix scripts download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix scripts)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (guix build utils)
  #:use-module (guix build-system copy)

  #:use-module (guix records)
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
                  (license license:non-copyleft) ;; FIXME: what should I do here?
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
(name \"~a\")
(version \"~a\")
(synopsis \"~a\")
(description \"~a\")
(license ~a)
(home-page \"\")
(build-system ~a)
(source ~a))"
          (package-name package)
          (package-version package)
          (package-synopsis package)
          (package-description package)
          "license:gpl3" ;; FIXME: oh well, licences causing trouble again
          "copy-build-system"
          (source-origin->string (package-source package))))


(define (dependency->locked-channel-package dependency)
  (symbol->string
   (cadr dependency)))

(define (dependency->locked-source-package dependency)
  (source-package->package-string
   (dependency->source-package dependency)))

(define (dependency->locked-dependency dependency)
  (if (eq? (car dependency) 'channel-package)
      (dependency->locked-channel-package dependency)
      (dependency->locked-source-package dependency)))

;; ((:name foo) (package bar))
(define (lock-dependencies dependencies)
  (map dependency->locked-dependency dependencies))

;; TODO: add build-system in order to support building the project through Guix
;; A Guix project
(define-record-type* <project>
  project make-project
  project?
  this-project
  (name project-name)                   ; string
  (channels project-channels (thunked)) ; string
  (dependencies project-dependencies
                (thunked)
                (sanitize lock-dependencies)))

;; TODO: add package inputs (aka: dependencies)
(define (project->package-string project project-path)
  (format #f
          "
(define %source-dir \"~a\")

(package
(name \"~a\")
(version \"~a\")
(synopsis \"~a\")
(description \"~a\")
(license ~a)
(home-page \"\")
(build-system ~a)
(source (local-file %source-dir #:recursive? #t)))\n"
          project-path
          (project-name project)
          "0.0.0-dev"
          ""
          ""
          "license:gpl3" ;; FIXME: oh well, licences causing trouble again
          "trivial-build-system"))


;;;
;;; Helpers.
;;;

(define-syntax-rule (todo! ...)
  (leave (G_ "not implemented yet!~%")))


;;;
;;; Subcommandas.
;;;

(define (get-guix-channel)
  (car (filter (lambda (channel)
                   (eq? (channel-name channel) 'guix))
                 (current-channels))))

;; TODO: add giv as project dependency in a way that works ;-;
(define (write-initial-project port)
  (let ((guix-channel
         (get-guix-channel)))
    (format port
  "(project
    (channels
     ('guix . \"~a\"))
    (dependencies
     ('channel-package 'giv)))\n" (channel-commit guix-channel))))

(define (write-initial-sources-lock port project project-path)
  (display (project->package-string project project-path) port))

(define* (bootstrap-project command-args)
  ;; Bootstrap current dir as a Guix project
  (let ((path (if (null? command-args)
                  (getcwd)
                  (canonicalize-path (car command-args))))
        (initial-project
         (project
          (name "giv-project")
          (channels
           ('guix . (channel-commit (get-guix-channel))))
          (dependencies
           ('channel-package 'giv)))))
    (mkdir (string-append path "/giv"))
    (let ((port (open-output-file (string-append path "/giv/sources.scm"))))
      (write-initial-project port))
    (let ((port (open-output-file (string-append path "/giv/locked-sources.scm"))))
      ;; TODO: actually lock the sources.scm to real package definitions
      (write-initial-sources-lock port initial-project path))))


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
  (synopsis "manage project dependencies")
  (match args
    (("init" args ...)
     (bootstrap-project args))
    ((or ("-h") ("--help"))
     (show-help)
     (exit 0))
    ((command _ ...)
     (leave (G_ "~a: unknown sub-command~%") command))
    (()
     (leave (G_ "missing sub-command~%")))))
