;;; init.el --- -*- lexical-binding: t -*-
;;; https://nullprogram.com/blog/2016/12/22/

;; set elpa directory name based on emacs major version
;(setq package-user-dir (format "~/.emacs.d/elpa-%d" emacs-major-version)) ;ex. elpa-27
(setq package-user-dir (concat "~/.emacs.d/elpa-" emacs-version)) ;ex. elpa-27.1

;; To refresh package list, run:  M-x package-refresh-contents
;; To manually update installed packages:  M-x package-list-packages U x
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(
        ;("elpy"  . "http://jorgenschaefer.github.io/packages/") ; elpy package archive
        ("gnu"   . "https://elpa.gnu.org/packages/") ; default package archive
        ("org"   . "https://orgmode.org/elpa/") ; provides org-plus-contrib
        ("melpa" . "https://melpa.org/packages/") ; milkypostman's pkg archive
))
(package-initialize)

;; install use-package early, needed for org-plus-contrib
;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Disable ad-redefinition-action messages on startup
;; Caused by third party functions redefining defadvice
;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)

;;; Ignore byte-compile warning
;; ex. for Emacs 27: Warning: cl package required at runtime
(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  redefine
                                  obsolete
                                  noruntime
                                  cl-functions
                                  interactive-only
                                  ))

;; Install and load the latest org-mode version via org-plus-contrib
(use-package org
  ;:defer t
  ;; to be sure we have the latest Org version
  :ensure org-plus-contrib
  :hook
  ;(org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  ;(org-mode . org-num-mode)
  :custom
  (org-src-tab-acts-natively t))

;; install no-littering as early as possible
; Help keeping ~/.emacs.d clean
; https://github.com/emacscollective/no-littering/
(use-package no-littering               ; Keep .emacs.d clean
  :ensure t
)

;; Simplify tangling, on emacs startup check
(when (file-exists-p "~/.emacs.d/emacs-init.org")
  (org-babel-load-file (expand-file-name "emacs-init.org"
                       user-emacs-directory))
)
