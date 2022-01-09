#!/bin/bash

echo *** Install test ***

install_method="$1"

app=/tmp/hypb-$$

mkdir -p $app
cp -a * $app
export HOME=$app/$install_method

[ -e $app/$install_method/install-local.sh ] && cd $app/$install_method && ./install-local.sh

cd $app/$install_method
emacs --batch -l $app/$install_method/.emacs \
      --eval '(load (expand-file-name "test/hy-test-dependencies.el" hyperb:dir))' \
      -l hypb-ert \
      --eval "(hypb-ert-require-libraries)" \
      -f ert-run-tests-batch-and-exit
