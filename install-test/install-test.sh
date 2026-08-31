#!/bin/bash

echo *** Install test ***

set -e

im="$1"

mkdir -p ${HOME}/.emacs.d

[ -e ${im}-install-local.sh ] && ./${im}-install-local.sh

cp ${im}-emacs ${HOME}/.emacs.d/init.el
[ -e ${im}-early-init ] && cp ${im}-early-init ${HOME}/.emacs.d/early-init.el

cd $HOME

# Initial startup will install Hyperbole
emacs -nw --init-directory="${HOME}/.emacs.d" --eval "(kill-emacs 0)"

# Verify installation
if emacs -nw --init-directory="${HOME}/.emacs.d" \
   --eval "(kill-emacs
             (if (and (featurep (quote hyperbole))
                      hyperbole-mode)
                 0
               1))"
then
  echo -e "\n\n*** Hyperbole ${im} installed OK ***\n\n"
else
  echo -e "\n\n*** ERROR: Hyperbole ${im} was not installed properly ***\n\n"
  # Inspect the installation on error
  bash
  exit 1
fi

## Uncomment for inspecting the installation
# bash
