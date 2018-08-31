; -*- mode: emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Doug's GNU Emacs startup file                                           ;;;
;;; doug@jacksonspub.com                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; whoami?
(setq
 user-full-name "Douglas Jackson"
 user-mail-address "dbjackson@gmail.com")

;; To refresh package list, run:  M-x package-refresh-contents
;; To manually update installed packages:  M-x package-list-packages U x
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(
        ("elpy"  . "http://jorgenschaefer.github.io/packages/")
        ("gnu"   . "http://elpa.gnu.org/packages/")
        ("org"   . "http://orgmode.org/elpa/") ; provides org-plus-contrib
        ("melpa" . "http://melpa.org/packages/")
))
(package-initialize)

;; use-package - https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Test out a package without installing it
;(use-package try
;  :ensure t)

;; Load org-plus-contrib for org-babel
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
)

;; org-babel
(org-babel-load-file (expand-file-name "emacs-init.org"
                     user-emacs-directory))
