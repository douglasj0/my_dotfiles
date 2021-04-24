;;; early-init.el --- -*- lexical-binding: t -*-
;;
;; Filename: early-init.el

;; ideasman_42's suggestion
;; https://www.reddit.com/r/emacs/comments/msll0j/do_any_of_you_have_some_tips_on_speeding_up_emacs/

;; default of gc-cons-threshold is 800k
(defvar default-gc-cons-threshold 16777216 ; 16mb
  "my default desired value of `gc-cons-threshold'
   during normal emacs operations.")

;; make garbage collector less invasive
(setq
  gc-cons-threshold
  most-positive-fixnum
  gc-cons-percentage 0.6)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda (&rest _)
    (setq
      gc-cons-threshold
      default-gc-cons-threshold
      gc-cons-percentage 0.1
      file-name-handler-alist default-file-name-handler-alist)

    ;; delete no longer necessary startup variable
    (makunbound 'default-file-name-handler-alist)))

;; Package initialize occurs automatically, before `user-init-file' is loaded, but after
;; `early-init-file'. We handle package initialization, so we must prevent Emacs from doing it
;; early!
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;;; and site-run-file
(setq site-run-file nil)

;; Avoid the pitfall of “loading old bytecode instead of newer source”
(setq load-prefer-newer t)

;; Profile emacs startup
;; https://raw.githubusercontent.com/daviwil/dotfiles/master/Emacs.org
(add-hook 'emacs-startup-hook
  (lambda ()
    (message "*** Emacs loaded in %s with %d garbage collections."
      (format "%.2f seconds"
         (float-time
            (time-subtract after-init-time before-init-time)))
      gcs-done)))



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

;; Also suppress extraneous package-initialize errors
(setq warning-suppress-log-types '((package reinitialization)))

;; set elpa directory name based on emacs major version
;(setq package-user-dir (format "~/.emacs.d/elpa-%d" emacs-major-version)) ;ex. elpa-27
(setq package-user-dir (concat "~/.emacs.d/elpa-" emacs-version)) ;ex. elpa-27.1

;; To refresh package list, run:  M-x package-refresh-contents
;; To manually update installed packages:  M-x package-list-packages U x
(require 'package)
;(setq package-enable-at-startup nil) ; moved to early-init
(setq package-archives
      '(
        ;("elpy"  . "http://jorgenschaefer.github.io/packages/") ; elpy package archive
        ("gnu"   . "https://elpa.gnu.org/packages/") ; default package archive
        ("org"   . "https://orgmode.org/elpa/") ; provides org-plus-contrib
        ("melpa" . "https://melpa.org/packages/") ; milkypostman's pkg archive
))

(package-initialize)

(provide 'early-init)
