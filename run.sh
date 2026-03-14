#!/bin/sh
exec sbcl --noinform \
  --eval '(require :asdf)' \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(asdf:load-system :clawmacs)' \
  --eval '(clawmacs:clawmacs-main)'
