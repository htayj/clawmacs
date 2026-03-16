(use-modules (gnu packages))

(specifications->packages
  '(
    "sbcl"
    "openssl"
    "python"
    "python-pillow"
    "nss-certs"
    "bash"
    "coreutils"
    "findutils"
    "grep"
    "font-dejavu"
    "curl"))
