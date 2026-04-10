(use-modules (gnu packages))

(specifications->packages
  '(
    "sbcl"
    "openssl"
    "ncurses"
    "python"
    "python-pillow"
    "rust"
    "rust:cargo"
    "gcc-toolchain"
    "nss-certs"
    "bash"
    "coreutils"
    "findutils"
    "grep"
    "git"
    "font-dejavu"
    "curl"
    "cl-mcclim"))
