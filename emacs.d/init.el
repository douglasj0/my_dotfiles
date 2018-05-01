; -*- mode: emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Doug's GNU Emacs startup file                                           ;;;
;;; doug@jacksonspub.com                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keep track of loading time
(defconst emacs-start-time (current-time))

;;; Load Customizations if they exist
(setq custom-file "~/Dropbox/Home/elisp/custom.el")
(load custom-file 'noerror)

;;; Add 'info' and 'elisp' to load-path (C-h v load-path RET)
(add-to-list 'Info-default-directory-list "~/.emacs.d/info")
(add-to-list 'load-path "~/.emacs.d/elisp/") ;; elisp packages not in pkg manager

;; To manually update installed packages:  M-x package-list-packages U x
(require 'package)
(setq package-enable-at-startup nil)
;(add-to-list 'package-archives
;             '("melpa" . "http://melpa.org/packages/"))
(setq package-archives
      '(
        ("elpy"  . "https://jorgenschaefer.github.io/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Test out a package without installing it
(use-package try
  :ensure t)

;; Load org-plus-contrib
(load "~/.emacs.d/org-plus-contrib.el")

;; Load emacs-init org file
(require 'org)
(org-babel-load-file
  (expand-file-name "emacs-init.org"
                     user-emacs-directory))

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                           emacs-start-time))))
(message "Loading settings...done (%.3fs)" elapsed))
