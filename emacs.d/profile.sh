#!/bin/bash
## https://oremacs.com/2015/02/24/emacs-speed-test/
## also read
## https://stackoverflow.com/questions/778716/how-can-i-make-emacs-start-up-faster

#emacs -Q -l elisp/profile-dotemacs.el \
#    --eval "(setq profile-dotemacs-file \
#        (setq load-file-name $(abspath init.el)))" \
#    -f profile-dotemacs

emacs -Q -l elisp/profile-dotemacs.el \
  -f profile-dotemacs
