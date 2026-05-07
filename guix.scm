(use-modules (gnu packages)
             (guix build-system asdf)
             (guix git-download)
             (guix packages))

(define sbcl-mcclim-1.0.0
  (package
    (inherit (specification->package "sbcl-mcclim"))
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/McCLIM/McCLIM")
             (commit (string-append version "-koliada"))))
       (file-name (git-file-name "cl-mcclim" version))
       (sha256
        (base32 "1rh321ikganff515jnm51jk71dwyij970jvic1jrh0cami7s9ifz"))))))

(define cl-mcclim-1.0.0
  (sbcl-package->cl-source-package sbcl-mcclim-1.0.0))

(append
 (specifications->packages
  '("sbcl"
    "openssl"
    "python"
    "python-pillow"
    "gcc-toolchain"
    "nss-certs"
    "bash"
    "coreutils"
    "findutils"
    "grep"
    "git"
    "font-dejavu"
    "curl"
    "xorg-server"
    "xdotool"
    "imagemagick"))
 (list cl-mcclim-1.0.0))
