; -*- mode: emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Doug's GNU Emacs startup file                                           ;;;
;;; doug@jacksonspub.com                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2014-12-30 - Added electric-pair-mode
;;; 2015-01-31 - Commented out poweline (init errors) and cobol-mode (magit)
;;; 2016-01-30 - Added C-Tab to bury-buffer, easy buffer switching?
;;; 2017-01-11 - Installed better-defaults, commented out or removed dups
;;; 2017-09-01 - Converted from help to ivy/counsel
;;; 2017-10-03 - Converted from neotree to treemacs
;;; 2018-04-12 - Converted back to neotree
;;; 2018-04-14 - Declared Emacs bancruptcy, started convering to org for init

;; Keep track of loading time
(defconst emacs-start-time (current-time))

;;; Load Customizations if they exist
(setq custom-file "~/Dropbox/Home/elisp/custom.el")
(load custom-file 'noerror)

;;; Add 'info' and 'elisp' to load-path (C-h v load-path RET)
(add-to-list 'Info-default-directory-list "~/.emacs.d/info")
(add-to-list 'load-path "~/.emacs.d/elisp/") ;; elisp packages not in pkg manager

;;; Install Packages - http://crn.io/2011/12/managing-packages-in-emacs-24/
;;; Update installed packages with: M-x package-list-packages U x
(setq package-archives
      '(
        ("elpy"  . "https://jorgenschaefer.github.io/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")
))

(defvar my-packages)
(setq my-packages '(
  color-moccur  ;; search a regexp in all buffers
  deft          ;; quickly browse, filter, & edit dirs of plain text notes
  dumb-jump     ;; an Emacs "jump to definition" package
  elpy          ;; python IDE for Emacs
  ivy           ;; generic completion mechanism like ido and helm
  counsel       ;; ivy's counsel
  exec-path-from-shell
  flycheck
  git-gutter    ;; show icon in gutter area indicating if ins, mod or del
  json-mode     ;; https://github.com/joshwnj/json-mode
  magit         ;; An emacs mode for git
  markdown-mode ;; Included with Emacs for OS X Modified
  multi-term    ;;
  neotree       ;; tree plugin like NerdTree for Vim
  org-plus-contrib ;; all org files plus all contribs files
  projectile    ;; project interaction library for Emacs
    counsel-projectile ;; projectile ivy integration
  py-autopep8   ;; automaticly apply pep8 fixes on file save
  pydoc-info    ;;
  restclient    ;; REST client for emacs
  s             ;; The long lost Emacs string manipulation library
  shell-pop     ;; Pop up a quick shell
  switch-window ;; Window switching, the visual way
  yasnippet     ;; a template system for Emacs, req by elpy
  zenburn-theme ;;
))

(load "package")
(package-initialize)

;; If we do not already have the downloaded package list, download it now.
;; To manually update installed packages:  M-x package-list-packages U x
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Load emacs-init org file
(require 'org)
(org-babel-load-file
  (expand-file-name "emacs-init.org"
                     user-emacs-directory))

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                           emacs-start-time))))
(message "Loading settings...done (%.3fs)" elapsed))
