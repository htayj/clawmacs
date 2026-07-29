{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = with pkgs; [
    sbcl
    openssl
    python3
    python3Packages.pillow
    cacert
    bash
    coreutils
    util-linux
    findutils
    gnugrep
    dejavu_fonts
    curl
  ];

  shellHook = ''
    export RPLACA_QUICKLISP_SETUP="$PWD/.cache/home/quicklisp/setup.lisp"
    export XDG_CACHE_HOME="$PWD/.cache"
    mkdir -p "$XDG_CACHE_HOME/home/quicklisp"
  '';
}
