(use-modules (gnu packages))

(specifications->packages
  '(
    "sbcl"
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
    "imagemagick"
    "cl-mcclim"))
