;;; early-init.el --- -*- lexical-binding: t -*-
;
; disabling those in your init file, but elements were loaded before your init file, so you HAVE to call scroll-bar-mode and tool-bar-mode, which can take quite a lot of time (more than 100ms on my laptop)

;(push '(tool-bar-lines . 0) default-frame-alist)
;(push '(menu-bar-lines . 0) default-frame-alist)
;(push '(vertical-scroll-bars) default-frame-alist)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))


;;Garbage Collection
;;Make startup faster by reducing the frequency of garbage collection. Set gc-cons-threshold (the default is 800 kilobytes) to maximum value available, to prevent any garbage collection from happening during load time.
(setq gc-cons-threshold most-positive-fixnum) ;; was 100000000
;; Restore it to reasonable value after init.
;; was after-init-hook and 800000, but emacs-startup-hook runs later, catching more commands
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 20971520)))  ; 20mb


;; Show startup time
;; https://www.reddit.com/r/emacs/comments/m8d55l/what_is_your_startup_time
(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)
