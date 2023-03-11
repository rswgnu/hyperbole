#!/bin/bash

echo *** Install test ***

set -e

install_method="$1"

app=/tmp/hypb-$$

mkdir -p $app
cp -a * $app
export HOME=$app/$install_method

[ -e $app/$install_method/install-local.sh ] && cd $app/$install_method && ./install-local.sh

cd $app/$install_method

## Initial install with ert tests
emacs --batch -l $app/$install_method/.emacs \
      --eval '(load (expand-file-name "test/hy-test-dependencies.el" hyperb:dir))' \
      -l hypb-ert \
      --eval "(hypb-ert-require-libraries)" \
      -f ert-run-tests-batch-and-exit

## Startup again interactive - check hyperbole is found
emacs -nw --eval "(if (boundp 'hyperb:version) (kill-emacs 0) (kill-emacs 1))"
