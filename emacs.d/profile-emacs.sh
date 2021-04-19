#!/bin/bash
## https://oremacs.com/2015/02/24/emacs-speed-test/
## also read
## https://stackoverflow.com/questions/778716/how-can-i-make-emacs-start-up-faster

# Usage
# Start emacs as follows:
# $ emacs -Q -l PATH/profile-dotemacs.el -f profile-dotemacs
# with PATH being the path to where this file resides.
# The specific file to be profiled (i.e. ~/.emacs) is hardcoded within profile-dotemacs.el as follows:
#   (defvar profile-dotemacs-file "~/.emacs" "File to be profiled.") 

#emacs -Q -l elisp/profile-dotemacs.el \
#    --eval "(setq profile-dotemacs-file \
#        (setq load-file-name $(abspath init.el)))" \
#    -f profile-dotemacs

emacs -Q -l elisp/profile-dotemacs.el \
  -f profile-dotemacs
