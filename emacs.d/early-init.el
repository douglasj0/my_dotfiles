;;; early-init.el --- -*- lexical-binding: t -*-

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

(setq
  default-file-name-handler-alist
  file-name-handler-alist
  file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda (&rest _)
    (setq
      gc-cons-threshold
      default-gc-cons-threshold
      gc-cons-percentage 0.1
      file-name-handler-alist default-file-name-handler-alist)

    ;; delete no longer necessary startup variable
    (makunbound 'default-file-name-handler-alist)))

;;; Disable package-enabel-at-startup sinc we're handling it
(setq package-enable-at-startup nil)
;;; and site-run-file
(setq site-run-file nil)


;; Profile emacs startup
;; https://raw.githubusercontent.com/daviwil/dotfiles/master/Emacs.org
(add-hook 'emacs-startup-hook
  (lambda ()
    (message "*** Emacs loaded in %s with %d garbage collections."
      (format "%.2f seconds"
         (float-time
            (time-subtract after-init-time before-init-time)))
      gcs-done)))


;;; moved from init.el to simplify it
