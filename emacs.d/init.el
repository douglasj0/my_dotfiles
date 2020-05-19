; -*- mode: emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Doug's GNU Emacs startup file                                           ;;;
;;; hpotter@hogworts.edu                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; whoami?
(setq
 user-full-name "Douglas Jackson"
 user-mail-address "hpotter@hogworts.edu")

;; set elpa directory name based on emacs major version
;(setq package-user-dir (concat "~/.emacs.d/elpa-" emacs-version))
(setq package-user-dir (format "~/.emacs.d/elpa-%d" emacs-major-version))

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

;; install use-package - https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; keep customize settings in their own file
;(setq custom-file "~/.emacs.d/custom.el")
;(when (file-exists-p custom-file)
;  (load custom-file))

;; enable included org mode to provide org-babel
(require 'org)
(when (file-exists-p "~/.emacs.d/emacs-init.org")
(org-babel-load-file (expand-file-name "emacs-init.org"
                     user-emacs-directory))
(byte-compile-file (concat user-emacs-directory "emacs-init.el")))

;; Disable ad-redefinition-action messages on startup
;; Caused by third party functions redefining defadvice
;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)
