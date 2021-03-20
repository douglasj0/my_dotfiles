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

;; Startup time
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
