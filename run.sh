#!/bin/sh

# Find OpenSSL library for drakma/cl+ssl on Guix systems
if [ -z "$LD_LIBRARY_PATH" ]; then
  SSL_LIB=$(find /gnu/store -maxdepth 3 -name "libcrypto.so.3" 2>/dev/null | head -1)
  if [ -n "$SSL_LIB" ]; then
    export LD_LIBRARY_PATH="$(dirname "$SSL_LIB")"
  fi
fi

exec sbcl --noinform \
  --load "$HOME/quicklisp/setup.lisp" \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(asdf:load-system :clawmacs)' \
  --eval '(clawmacs:clawmacs-main)'
