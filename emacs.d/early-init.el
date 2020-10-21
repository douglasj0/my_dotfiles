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
