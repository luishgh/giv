(define-module (guix extensions giv)
  #:use-module (guix channels)
  #:use-module (guix describe)
  #:use-module (guix scripts download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix import utils) #:prefix import-utils:)
  #:use-module (guix scripts)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (guix build utils)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)

  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 eval-string)

  #:export (guix-giv project))


;;;
;;; Constants.
;;;

(define-once %giv-folder-path
  "/giv")

(define-once %project-prelude
  "(use-modules (guix extensions giv))")


;;;
;;; Helpers.
;;;

(define-syntax-rule (todo! ...)
  (let* ((current-location (current-source-location)))
    (leave (G_ "~a:~a:~a: not implemented yet!~%")
           (assq-ref current-location 'filename)
           (assq-ref current-location 'line)
           (assq-ref current-location 'column))))


;;;
;;; Library.
;;;

(define (guix-download-wrapper url)
  (info (G_ "computing hash for ~a...~%") url)
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
                               license
                               type)
                (unless (eq? type 'gnu)
                  (leave (G_ "Only gnu tarballs are supported ATM! You provided: ~a") type))
                (package
                  ;; Dondle metadata
                  ;; TODO: truly deal with this
                  (version "0.0.0-giv") ;; at first, versions shouldn't matter
                  (synopsis (string-append "giv source: " (symbol->string name)))
                  (description "A giv source locked.")
                  (home-page "")
                  ;; Real shit
                  ;; TODO: support something besides gnu tarballs
                  (name (symbol-append name '-giv))
                  (license (import-utils:spdx-string->license license))
                  (build-system gnu-build-system)
                  (source (origin
                            (method url-fetch)
                            (uri url)
                            (sha256
                             (base32 (dependency-url->sha256 url))))))))

(define (source-origin->list origin)
  `(origin
    (method ,(string->symbol (string-delete #\* (symbol->string (procedure-name (origin-method origin))))))
    (uri ,(origin-uri origin))
    (sha256
     ,(content-hash-value (origin-hash origin)))))

(define (source-package->list package)
  `(package
    (name ,(symbol->string (package-name package)))
    (version ,(package-version package))
    (synopsis ,(package-synopsis package))
    (description ,(package-description package))
    (license ,(package-license package))
    (home-page "")
    (build-system ,(symbol-append (build-system-name (package-build-system package)) '-build-system))
    (source ,(source-origin->list (package-source package)))))

(define (dependency->locked-channel-package dependency)
  `(specification->package ,(symbol->string (cadr dependency))))

;; NOTE: I first generate a valid <package> instance and then turn it
;; into staged code instead of just generating the code directly for easy validation
(define (dependency->locked-source-package dependency)
  (source-package->list
   (dependency->source-package dependency)))

(define (dependency->locked-dependency dependency)
  (match dependency
    (('channel-package package) (dependency->locked-channel-package dependency))
    (_ (dependency->locked-source-package dependency))))

(define (lock-dependencies dependencies)
  (map dependency->locked-dependency dependencies))

;; TODO: sanitize project fields where it is needed
;; A Guix project
(define-record-type* <project>
  project make-project
  project?
  this-project
  (name project-name)                   ; string
  (channels project-channels (thunked)) ; string
  (dependencies project-dependencies    ; list of dependencies
                (thunked)
                (default '()))
  (build-system project-build-system    ; symbol
                (default 'copy-build-system)))

;; NOTE: In regards to channel-packages. Using specification->package
;; for channel packages permits typos, so it would be better to latter
;; change to symbols and import the appropriate modules. However, for
;; the first impl, it's fine.
;; TODO: support useful build systems
;; TODO: support other VCS than git
(define (project->package-string project project-path)
  (format #f
          "~s\n\n~s\n\n~s\n"
          '(use-modules (guix packages)
                        (gnu packages)
                        ((guix licenses) #:prefix license:)
                        (guix build-system copy)
                        (guix build-system gnu)
                        (guix git-download)
                        (guix download)
                        (guix gexp))

          `(define %source-dir ,project-path)

          `(package
            (name ,(project-name project))
            (version "0.0.0-dev")
            (synopsis "")
            (description "")
            (license "license:gpl3") ;; FIXME: oh well, licences causing trouble again
            (home-page "")
            (build-system ,(project-build-system project))
            (source (local-file %source-dir
                                #:recursive? #t
                                #:select? (git-predicate %source-dir)))
            (inputs
             (list ,@(if (not (null? (project-dependencies project)))
                      (lock-dependencies
                       (project-dependencies project))
                      '(#f)))))))

;; TODO: make project-string look nicer (aka: correctly indented and all)
(define (project->project-string project)
  (format #f
          "(project
     (name \"~a\")
     (channels
      '(~a))
     (dependencies
      '~a))\n"
          (project-name project)
          (fold
           (lambda (str prev)
             (string-append prev "\n" str))
           ""
           (map
            (lambda (channel-spec)
              (match channel-spec
                ((#:name name
                  #:url url
                  #:commit commit
                  #:branch branch)
                 (format #f
                  "(#:name ~a
#:url \"~a\"
#:commit \"~a\"
#:branch \"~a\")"
                  name url commit branch))
                ((#:name name
                  #:url url
                  #:commit commit)
                 (format #f
                         "(#:name ~a
#:url \"~a\"
#:commit \"~a\")"
                         name url commit))))
            (project-channels project)))
          (if (null? (project-dependencies project))
              "()"
              (format #f "~s" (project-dependencies project)))))

;; TODO: support channel authentication
(define (lock-project-channels project)
  (format #f
          "~s\n"
          `(list ,@(map
                    (lambda (channel-spec)
                      (let-keywords channel-spec #f (name url commit (branch "master"))
                                    `(channel
                                      (name ',name)
                                      (url ,url)
                                      (branch ,branch)
                                      (commit ,commit))))
                    (project-channels project)))))


;;;
;;; Subcommandas.
;;;

(define (get-guix-channel)
  (car (filter (lambda (channel)
                   (eq? (channel-name channel) 'guix))
                 (current-channels))))

(define (write-project-sources port project)
  (display (project->project-string project) port))

(define (write-sources-lock port project project-path)
  (display (project->package-string project project-path) port))

(define (write-channels-lock port project)
  (display (lock-project-channels project) port))

(define (generate-sources path project)
  (let ((port (open-output-file (string-append path %giv-folder-path "/sources.scm"))))
    (write-project-sources port project)
    (close-port port)))

;; TODO: maybe turn this into a subcommand, niv doesn't have it but seems useful
(define* (lock-project #:optional path project)
  (let* ((path (or path (getcwd)))
         (project (or project (get-project))))
    (let ((port (open-output-file (string-append path %giv-folder-path "/locked-sources.scm"))))
      (write-sources-lock port project path)
      (close-port port))
    (let ((port (open-output-file (string-append path %giv-folder-path "/locked-channels.scm"))))
      (write-channels-lock port project)
      (close-port port))))

(define (update-project-files path current-project)
  (generate-sources path current-project)
  (lock-project path current-project))

;; TODO: add giv as project dependency in a way that works ;-; i have
;; to check how a guix extension can be packaged. First we could do it
;; with a custom channel and later propose the addition to guix proper
;; FIXME: at first, I'll just pretend like everyone has giv :P
(define (bootstrap-project command-args)
  ;; Bootstrap current dir as a Guix project
  (let* ((path (getcwd))
         (initial-project
          (project
           (name (basename path))
           (channels
            `((#:name guix
               #:url "https://git.savannah.gnu.org/git/guix.git"
               #:commit ,(channel-commit (get-guix-channel))))))))

    ;; Check if giv folder is already present
    (if (file-exists? (string-append path %giv-folder-path))
        (leave (G_ "giv folder already present at ~a~%") path)
        (begin
          (mkdir (string-append path %giv-folder-path))
          (update-project-files path initial-project)))))

(define (get-project)
  (let* ((project-path
          (if (file-exists? (string-append (getcwd) %giv-folder-path))
              (getcwd)
              (leave (G_ "Not inside a giv project!~%"))))
         (project-string (call-with-input-file (string-append project-path %giv-folder-path "/sources.scm") get-string-all)))
    (eval-string (string-append %project-prelude project-string))))

(define (add-channel-dependency current-project name)
  (project
   (inherit current-project)
   (dependencies (append (project-dependencies current-project) `((channel-package ,(string->symbol name)))))))

;; TODO: support templates for automatic updating
(define (add-source-dependency current-project name url type license)
  (project
   (inherit current-project)
   (dependencies (append (project-dependencies current-project)
                         `((#:name ,(string->symbol name)
                            #:url ,url
                            #:type ,(string->symbol type)
                            #:license ,license))))))

(define (add-dependency args)
  (let* ((path (getcwd))
         (current-project (get-project))
         (new-project
          (match args
            ((name) (add-channel-dependency current-project name))
            ((name url type license) (add-source-dependency current-project name url type license))
            (_ (leave (G_ "wrong number of arguments for add~%"))))))
    (update-project-files path new-project)))

(define (remove-dependency args)
  (let* ((dependency-name (car args))
         (path (getcwd))
         (current-project (get-project))
         (new-project
          (project
           (inherit current-project)
           (dependencies
            (remove
             (lambda (dependency)
               (match dependency
                 (`(channel-package ,dependency-name) #t)
                 ((#:name dependency-name ...) #t)
                 (_ #f)))
             (project-dependencies current-project))))))
    (info (G_ "removing ~a...~%") dependency-name)
    (update-project-files path new-project)))


;;;
;;; Command-line interface.
;;;

(define (show-subcommands)
  (display (G_ "Available commands:\n"))
  (display (G_ "
    init [PATH]           bootstrap a Guix project"))
  (display (G_ "
    add NAME [URL] [TYPE] add a new dependency")))

(define (show-flags)
  ;; (display (G_ "
  ;; -m, --manifest=FILE    create project with the manifest from FILE"))
  ;; (display (G_ "
  ;; -n, --dry-run          do not actually build the modules"))

  ;; (newline)

  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -v, --version          display version information and exit")))

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
    (("add" args ...)
     (add-dependency args))
    (("remove" args ...)
     (remove-dependency args))
    ((or ("-h") ("--help"))
     (show-help)
     (exit 0))
    ((command _ ...)
     (leave (G_ "~a: unknown sub-command~%") command))
    (()
     (leave (G_ "missing sub-command~%")))))
