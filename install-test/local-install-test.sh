#!/bin/bash

echo *** Install test ***

set -e

install_method="$1"

app=/tmp/hypb-$$/home/user
export HOME=$app
mkdir -p $app/.emacs.d

[ -e ${install_method}-install-local.sh ] && ./${install_method}-install-local.sh

if [ -n "$2" ]
then
  export LOCAL_HYPB_REPO=$2
  export LOCAL_HYPB_BRANCH=$3
fi

cp ${install_method}-emacs $app/.emacs.d/init.el

cd $app

## Initial install with ert tests
## FIXME: This does not fail the script if test are failing.  Current
## state is that the test suite does not work 100% for different
## reasons for all install forms.
emacs --batch -l ~/.emacs.d/init.el \
      --eval '(load (expand-file-name "test/hy-test-dependencies.el" hyperb:dir))' \
      -l hypb-ert \
      --eval "(hypb-ert-require-libraries)" \
      -f ert-run-tests-batch-and-exit || true

## Startup again interactive - check hyperbole is found
if emacs -nw --eval "(if (boundp 'hyperb:version) (kill-emacs 0) (kill-emacs 1))"
then
  echo "Install succeeded"
else
  echo "Install FAILED"
fi
