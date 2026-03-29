(use-modules (gnu packages))

(specifications->packages
  '(
    "sbcl"
    "openssl"
    "ncurses"
    "python"
    "python-pillow"
    "nss-certs"
    "bash"
    "coreutils"
    "findutils"
    "grep"
    "font-dejavu"
    "curl"
    "cl-mcclim"))
