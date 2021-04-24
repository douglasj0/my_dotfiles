;;; init.el --- -*- lexical-binding: t -*-
;;; lexical scope: https://nullprogram.com/blog/2016/12/22/


;; install use-package early, needed for org-plus-contrib
;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents) ;; maybe only do before installing pkgs?
  (package-install 'use-package))

;; install no-littering as early as possible
; Help keeping ~/.emacs.d clean
; https://github.com/emacscollective/no-littering/
(use-package no-littering               ; Keep .emacs.d clean
  :ensure t
)

;; Install and load the latest org-mode version via org-plus-contrib
(use-package org
  :ensure org-plus-contrib
  :hook
  ;(org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  ;(org-mode . org-num-mode)
  :custom
  (org-src-tab-acts-natively t))

;; Simplify tangling, on emacs startup check
(when (file-exists-p "~/.emacs.d/emacs-init.org")
  (org-babel-load-file (expand-file-name "emacs-init.org"
                       user-emacs-directory))
)
