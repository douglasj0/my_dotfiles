; -*- mode: emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Doug's GNU Emacs startup file                                           ;;;
;;; hpotter@hogworts.edu                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; whoami?
(setq
 user-full-name "Douglas Jackson"
 user-mail-address "hpotter@hogworts.edu")

;; To refresh package list, run:  M-x package-refresh-contents
;; To manually update installed packages:  M-x package-list-packages U x
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(
        ;("elpy"  . "http://jorgenschaefer.github.io/packages/") ; elpy package archive
        ;("gnu"   . "https://elpa.gnu.org/packages/") ; default package archive
        ("org"   . "https://orgmode.org/elpa/") ; provides org-plus-contrib
        ("melpa" . "https://melpa.org/packages/") ; milkypostman's pkg archive
))
(package-initialize)

;; use-package - https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;(eval-when-compile
;  (require 'use-package)
;  (setq use-package-verbose t)
;  ;(require 'diminish)                ;; if you use :diminish
;  ;(require 'bind-key)                ;; if you use any :bind variant
;)

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

;; Disable ad-redefinition-action messages on startup
;; Caused by third party functions redefining defadvice
;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)
