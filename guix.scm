(use-modules
  (gnu packages)
  (gnu packages autotools)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo)
  (guix build-system gnu)
  (guix download)
  (guix gexp)
  ((guix licenses) #:prefix license:)
  (guix packages)
  (srfi srfi-1))

(package
  (name "guile-smt")
  (version "0.1")
  (source
    (local-file
      (dirname (current-filename))
      #:recursive?
      #t
      #:select?
      (lambda (file stat)
        (not (any (lambda (my-string)
                    (string-contains file my-string))
                  (list ".git" ".dir-locals.el" "guix.scm"))))))
  (build-system gnu-build-system)
  (arguments `())
  (native-inputs
    (list autoconf automake pkg-config texinfo))
  (inputs (list guile-3.0 guile-gdbm-ffi))
  (propagated-inputs (list))
  (synopsis "")
  (description "")
  (home-page "")
  (license license:gpl3+))

