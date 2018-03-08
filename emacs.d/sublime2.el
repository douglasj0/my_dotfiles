;;;
;;; Emacs additions to mimic useful sublime text 2 features
;;;

;;; Enable line numbers (Similar to Sublime Text 2)
;; (global-linum-mode 1)
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
;(setq linum-format "%d ") ;put space between linenumber and text
(global-set-key (kbd "C-<f6>") 'linum-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- iedit --- Edit multiple regions with the same content simultaneously
;;; Like Sublime Text 2
;;; Select string with C-;   C-' will hide non-matching strings
;;; http://www.masteringemacs.org/articles/2012/10/02/iedit-interactive-multi-occurrence-editing-in-your-buffer/

;; (add-to-list 'load-path "~/.emacs.d/elisp/iedit/")
;; (require 'iedit)
;; (define-key global-map (kbd "C-;") 'iedit-mode)
;; ;(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- Mark Multiple
;;; Like Sublime Text 2
;;; https://github.com/magnars/mark-multiple.el

;; (add-to-list 'load-path "~/.emacs.d/elisp/mm")
;; (require 'inline-string-rectangle)
;; (global-set-key (kbd "C-x r t") 'inline-string-rectangle)

;; (require 'mark-more-like-this)
;; (global-set-key (kbd "C-c C-r") 'mark-previous-like-this) ; was C-<
;; (global-set-key (kbd "C-c C-n") 'mark-next-like-this)     ; was C->
;; (global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
;; (global-set-key (kbd "C-*") 'mark-all-like-this)

;; (add-hook 'sgml-mode-hook
;;           (lambda ()
;;             (require 'rename-sgml-tag)
;;             (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- sublimity - mini map of current buffer (better then minimap?)
;;; https://github.com/zk-phi/sublimity

;(require 'sublimity)
;; (require 'sublimity-scroll)
;; (require 'sublimity-map)
;; (require 'sublimity-attractive)

;(setq sublimity-scroll-weight 11
;      sublimity-scroll-drift-length 5)

;(sublimity-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- minimap - display of the current buffer alongside the main editing window
;;; Like Sublime Text 2
;;; http://www.emacswiki.org/emacs/MiniMap

;;(require 'minimap)
;;;Use ‘M-x minimap-create’ in a buffer you’re currently editing.
;;;Use ‘M-x minimap-kill’ to kill the minimap.
;;;Use ‘M-x customize-group RET minimap RET’ to adapt minimap to your needs.

;;; Toggle
;; (defun minimap-toggle ()
;;   "Toggle minimap for current buffer."
;;   (interactive)
;;   (if (not (boundp 'minimap-bufname))
;;       (setf minimap-bufname nil))
;;   (if (null minimap-bufname)
;;       (progn (minimap-create)
;; 	     (set-frame-width (selected-frame) 100))
;;     (progn (minimap-kill)
;; 	   (set-frame-width (selected-frame) 80))))
;; (global-set-key (kbd "C-<f7>") 'minimap-toggle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- multiple-cursors / similar to, but more customizable then, iedit
;;; Like Sublime Text 2
;;; https://github.com/magnars/multiple-cursors.el

;; (add-to-list 'load-path "~/.emacs.d/elisp/mc")
;; (require 'multiple-cursors)
;; ; From active region to multiple cursors:
;; (global-set-key (kbd "C-c C-l") 'mc/edit-lines)
;; (global-set-key (kbd "C-c C-e") 'mc/edit-ends-of-lines)
;; (global-set-key (kbd "C-c C-a") 'mc/edit-beginnings-of-lines)

;; ; Rectangular region mode
;; (global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; ; Mark more like this
;; (global-set-key (kbd "C-;") 'mc/mark-all-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-:") 'mc/mark-more-like-this-extended)
;; (global-set-key (kbd "C-c C-.") 'mc/mark-all-in-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- projectile
;;; Like Sublime Text 2
;;; Projectile is a project interaction library for Emacs.  If you want to mark
;;; a folder manually as a project just create an empty .projectile file in it
;;; https://github.com/bbatsov/projectile
;;; projectile file file (C-c p f)
;;;(add-to-list 'load-path "~/emacs.d/vendor")
;(require 'projectile)
;(require 'helm-projectile)
;(projectile-global-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- yasnippet
;;; Like Sublime Text 2
;;; https://github.com/capitaomorte/yasnippet

;; (add-to-list 'load-path "~/.emacs.d/elisp/yasnippet")
;; (require 'yasnippet)
;; (yas-global-mode 1)
