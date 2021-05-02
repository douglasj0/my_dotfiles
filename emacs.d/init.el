;;; init.el --- -*- lexical-binding: t -*-
;;; lexical scope: https://nullprogram.com/blog/2016/12/22/

;; NOTE: Moved back to init.el due to topgrade issues with early-init.el
;; set elpa directory name based on emacs major version
;(setq package-user-dir (format "~/.emacs.d/elpa-%d" emacs-major-version)) ;ex. elpa-27
(setq package-user-dir (concat "~/.emacs.d/elpa-" emacs-version)) ;ex. elpa-27.1

;; To refresh package list, run:  M-x package-refresh-contents
;; To manually update installed packages:  M-x package-list-packages U x
(require 'package)
;(setq package-enable-at-startup nil) ; moved to early-init
(setq package-archives
  '(
    ("melpa" . "https://melpa.org/packages/")     ; milkypostman's pkg archive
    ("org"   . "https://orgmode.org/elpa/")       ; provides org-plus-contrib
    ("elpa"  . "https://elpa.gnu.org/packages/"))); default package archive

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


;; Initialize use-package on non-linux platforms
;; install use-package early, needed for org-plus-contrib
;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; install no-littering as early as possible
; Help keeping ~/.emacs.d clean
; https://github.com/emacscollective/no-littering/
(use-package no-littering               ; Keep .emacs.d clean
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
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
