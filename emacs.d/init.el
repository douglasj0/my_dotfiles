; -*- mode: emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Doug's GNU Emacs startup file                                           ;;;
;;; hpotter@hogworts.edu                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable included org mode to provide org-babel
(require 'org)
(when (file-exists-p "~/.emacs.d/emacs-init.org")
(org-babel-load-file (expand-file-name "emacs-init.org"
                     user-emacs-directory))
(byte-compile-file (concat user-emacs-directory "emacs-init.el")))
