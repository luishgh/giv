(project
 (name "sample-project")
 (channels
  (guix . "654f878f0b9d2136affa3e3d32da1639e6942a54"))
 (dependencies
  ((#:name hello-tarball
    #:url "https://ftp.gnu.org/gnu/hello/hello-2.12.tar.gz"
    ;; TODO: add support for automatic updates through templates as the one below:
    ;; #:url-template "https://ftp.gnu.org/gnu/hello/hello-<version>.tar.gz"
    #:type tarball)
   (package hello) ;; This should get the `hello' package from the `guix' channel.
   )))

