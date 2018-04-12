; -*- mode: emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Doug's GNU Emacs startup file                                           ;;;
;;; doug@jacksonspub.com                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NOTE:
; upcoming Emacs 24.4 includes dired-hide-details-mode, and enables uniquify by
; default.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2014-12-30 - Added electric-pair-mode
;;; 2015-01-31 - Commented out poweline (init errors) and cobol-mode (magit)
;;; 2016-01-30 - Added C-Tab to bury-buffer, easy buffer switching?
;;; 2017-01-11 - Installed better-defaults, commented out or removed dups
;;; 2017-09-01 - Converted from help to ivy/counsel
;;; 2017-10-03 - Converted from neotree to treemacs

;(setq debug-on-error t)

;;; Load Customizations if they exist
(setq custom-file "~/Dropbox/Home/elisp/custom.el")
(load custom-file 'noerror)

;;; Add 'info' and 'elisp' to load-path (C-h v load-path RET)
(add-to-list 'Info-default-directory-list "~/.emacs.d/info")
(add-to-list 'load-path "~/.emacs.d/elisp/") ;; elisp packages not in pkg manager

;;; Custom theme path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;;; Install Packages - http://crn.io/2011/12/managing-packages-in-emacs-24/
;;; Update installed packages with: M-x package-list-packages U x
(setq package-archives
      '(
        ("elpy"  . "http://jorgenschaefer.github.io/packages/")
        ("gnu"   . "http://elpa.gnu.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
))

(defvar my-packages)
(setq my-packages '(
  ;auctex        ;; package for writing and formatting TeX files, keeps failing install
  color-moccur  ;; search a regexp in all buffers
  ;company       ;; text completion framework for Emacs
  ;confluence    ;; mode for editing pages in a Confluence wiki
  deft          ;; quickly browse, filter, & edit dirs of plain text notes
  ;dired+        ;; extends  'dired.el', 'dired-aux.el', and 'dired-x.el'
  ;diredx        ;; provides extra functionality for DiredMode
  ;dirtree       ;; Directory tree views for Emacs
  ;dsvn          ;; interface for managing Subversion working copies
  ;escreen       ;; window multiplexer for Emacs
  elpy          ;; python IDE for Emacs
  ivy           ;; generic completion mechanism like ido and helm
  counsel       ;; ivy's counsel
  exec-path-from-shell
  flycheck
  git-gutter    ;; show icon in gutter area indicating if ins, mod or del
  gnuplot-mode  ;; offers syntax hilighting, basic indent, and cmd to plot file
  graphviz-dot-mode ;; emacs mode for the language used by graphviz
  ;helm           ;; incremental completion and selection narrowing framework
  ;helm-projectile ;;
  iedit         ;; Edit multiple regions in the same way simultaneously
  json-mode     ;; https://github.com/joshwnj/json-mode
  ;key-chord     ;; map pairs of simultaneously pressed keys to commands
  magit         ;; An emacs mode for git
  ;mark-multiple ;; sorta lets you mark several regions at once
  markdown-mode ;; Included with Emacs for OS X Modified
  ;maxframe      ;; maximize the emacs frame and stay within the display res
  ;monokai-theme ;; replacement theme for zenburn, didn't like fonts
  multi-term    ;;
  ;multiple-cursors ;; Multiple cursors for emacs.
  neotree       ;; tree plugin like NerdTree for Vim
  nov           ;; Major mode for reading epubs
  org-plus-contrib ;; all org files plus all contribs files
  ;org-journal   ;; set of functions to maintain a simple personal diary/journal
  ;perspective   ;; provides multiple workspaces for each Emacs frame
  ;powerline     ;; Emacs version of the Vim powerline.
  projectile    ;; project interaction library for Emacs
  ;psvn          ;; interface for the revision control tool subversion
  puppet-mode   ;;
  py-autopep8   ;; automaticly apply pep8 fixes on file save
  pydoc-info    ;;
  restclient    ;; REST client for emacs
  s             ;; The long lost Emacs string manipulation library
  shell-pop     ;; Pop up a quick shell
  ;smartscan     ;; Quickly jumps between other symbols found at point
  ;smex          ;; M-x enhancement for Emacs. Built on top of IDO
  ;sr-speedbar   ;; mode make SpeedBar show in Current Frame
  ;tabbar        ;; Web browser like tabs
  ;treemacs      ;; Directory pane
  sublimity     ;; smooth-scrolling, minimap and distraction-free mode
  switch-window ;; Window switching, the visual way
  ;twittering-mode ;; Twitter client for Emacs
  undo-tree     ;; undo system to recover any past state of a buffer
  ;w3m           ;; an Emacs interface to the w3m text browser
  ;workgroups    ;; Store an unlimited number of window configs
  yasnippet     ;; a template system for Emacs
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

;;; Only use Themes in GUI. Set color theme (load-theme) for >= Emacs 24
(if (fboundp 'load-theme)
    (if (display-graphic-p)
        (load-theme 'zenburn :no-confirm)  ; Emacs in own window (zenburn)
      (load-theme 'wheatgrass :no-confirm)  ; Emacs in tty
))

;;; Reload .emacs file by typing: M-x reload-config
(defun reload-config () "Reloads .emacs interactively."
  (interactive)
  (load "~/.emacs.d/init.el"))

;;; Only start server if it is not currently running
;(require 'server) ;; Note: (start-server) is now mostly deprecated
(load "server")
(setq server-socket-dir "~/.emacs.d/tmp")
(unless (server-running-p) (server-start))

;;; Define booleans based on system type
(defconst *is-a-mac*
  (eq system-type 'darwin)
  "Is this running on OS X?")
(defconst *is-linux*
  (eq system-type 'gnu/linux)
  "Is this running on Linux?")

;;; Run multiple async commands at once without the output buffers colliding:
;; Allow running multiple async commands simultaneously
(defadvice shell-command (after shell-in-new-buffer (command &optional output-buffer error-buffer))
  (when (get-buffer "*Async Shell Command*")
    (with-current-buffer "*Async Shell Command*"
      (rename-uniquely))))
(ad-activate 'shell-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- enable functions that are disabled by default

;; Upcase and downcase regions
(put 'upcase-region 'disabled nil)  ;C-x C-u
(put 'downcase-region 'disabled nil)  ;C-x C-l

;; Sets the current horizontal position for C-n and C-p
(put 'set-goal-column 'disabled nil)

;; Restrict buffer editing to a region
;; Text Narrowing commands:
;;   Region: C-x n n, Page: C-x n p
;;   Funct: C-x n p, Widen: C-x n w
;;   Subtree in Org-Mode:   C-x n s
(put 'narrow-to-region 'disabled nil)

;; Dired functions (find-alternate 'a' reuses dired buffer)
(put 'dired-find-alternate-file 'disabled nil)

;; Goal Column, enter C-x C-n, at point to set column that C-n should go to
;; to clear enter C-u C-x C-n
(put 'set-goal-column 'disabled nil)

;; Enable mini-buffer history save feature
(setq savehist-additional-variables    ;; also save...
  '(search-ring regexp-search-ring)    ;; ... my search entries
  savehist-file "~/.emacs.d/cache/history") ;; keep my .emacs.d clean
(savehist-mode t)                      ;; do customization before activate

;; Enable linnum mode for major programming modes in Emacs 24 (C, puppet, etc)
;(add-hook 'prog-mode-hook 'linum-mode)

;; And disable electric-indent if active
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- global key bindings

;;; https://stackoverflow.com/questions/93058/emacs-switching-to-another-frame-mac-os-x
; Re-enable swap frames with M-`
(global-set-key "\M-`" 'other-frame)

;; Open an Emacs eshell, shell, or term
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c s") 'shell)
;(global-set-key (kbd "C-c t") 'term)
;(global-set-key (kbd "C-c t") 'ansi-term) ; re-assigned to shell-pop

;; Bind rgrep to C-c r / C-c C-r
(global-set-key (kbd "C-c r") 'rgrep)
(global-set-key (kbd "C-c C-r") 'rgrep)

;; remap these
(global-set-key [C-f12]      'list-matching-lines)
(global-set-key [C-f10]      'query-replace)
(global-set-key [S-f10]      'ispell-buffer)
(global-set-key [C-S-f10]    'flyspell-buffer)

;; Show a summery of all registers with content
(global-set-key (kbd "C-x r v") 'list-registers)

;; Zap-back-to-char C-M-z (deletes back to the and including character)
;; a counterpoint to zap-to-char M-z
(global-set-key "\C-\M-z" #'(lambda (arg char)
  (interactive "p\ncZap to char: ")
  (zap-to-char (- arg) char)))

;;; Disable line wrap
;;(setq default-truncate-lines t) ; deprecated in 23.2, use truncate-lines
(setq truncate-lines t)
;;; Make side by side buffers function the same as the main window
(setq truncate-partial-width-windows nil)
;;; Add F12 to toggle line wrap
(global-set-key [f12] 'toggle-truncate-lines)

;;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Move through windows in reverse order of (other-window), C-x o
(global-set-key (kbd "C-x O") 'previous-multiframe-window)

;; Invoke M-x without the Alt key (from Steve Yegge's blog)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; global settings - Generic Emacs settings I cannot live without

;;; Turn on automatic bracket insertion by pairs. New in emacs 24
;;; http://ergoemacs.org/emacs/emacs_insert_brackets_by_pair.html
;;; Works great with new text, editing existing text is very annoying
;(electric-pair-mode 1)

;;; Set the mode based on the buffer name.  Thanks to `__jim__'.
;;; http://www.reddit.com/r/emacs/comments/d2t4q/scratch_buffers_for_emacs/c0x7a68
(setq-default major-mode
              (lambda ()
                (let ((buffer-file-name (or buffer-file-name (buffer-name))))
                  (set-auto-mode))))

;;; Kill line backwards
;;; http://emacsredux.com/blog/2013/04/08/kill-line-backward/
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

;;; Ping settings (from net-util.el)
;;; http://www.masteringemacs.org/articles/2011/03/02/network-utilities-emacs/
(defvar ping-program-options)
(setq ping-program-options '("-c" "4"))

;;; Enable whitespace-mode for diff buffers
;;; http://stackoverflow.com/questions/11805584/automatically-enable-whitespace-mode-in-diff-mode
(add-hook 'diff-mode-hook
          '(lambda ()
            (whitespace-mode 1)))

;;; Mouse wheel and keyboard scroll settings
;;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-follow-mouse 't)     ;scroll window under mouse
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))) ;1 line
(setq mouse-wheel-progressive-speed t) ;'nil disables accelerated scrolling
(setq scroll-step 1                    ;keyboard scroll one line at a time
      scroll-preserve-screen-position t
      scroll-conservatively 10000)     ;smooth scrolling

;;; Enable holidays in Calendar
(setq mark-holidays-in-calendar t)

;;; Address the issue of Emacs's lack of a vi-like "O" command
;;; http://stackoverflow.com/questions/2173324/emacs-equivalents-of-vims-dd-o-o
;;; Open line above: C-o  Open line below: C-n C-o  Kill whole line: C-S-Bcksp
(defadvice open-line (around vi-style-open-line activate)
  "Make open-line behave more like vi."
  (beginning-of-line)
  ad-do-it
  (indent-according-to-mode))

;; w/o-man mode (elisp man page formater for systems without 'man')
(defvar woman-show-log)
(defvar woman-cache-filename)
(setq woman-show-log nil)
(autoload 'woman "woman"
  "Decode and browse a Unix man page." t)
(setq woman-cache-filename "~/.emacs.d/cache/wmcache.el")

;;; Make tooltips appear in the echo area (checks if function exists)
(if (fboundp 'tooltip-mode)
 (tooltip-mode -1)
 (setq tooltip-use-echo-area t))

;; Use CUA mode for rectangles (C-RET to select, normal emacs keys to copy)
;;; http://emacs-fu.blogspot.com/2010/01/rectangles-and-cua.html
(setq cua-enable-cua-keys nil)  ;; only for rectangles, keeps (C-c, C-v, C-x).
(cua-mode t)
;; Emacs 24.4 introduces rectangle-mark-mode, C-x SPC, but not quite as useful

;;; Don't show the startup screen
(setq inhibit-startup-message t)

;;; Disable startup message in scratch buffer
(setq-default initial-scratch-message nil)
;;; And set scratch buffer initial mode to text instead of lisp eval
;(setq-default initial-major-mode 'text-mode)
;;; Don't create new lines when pressing 'arrow-down key' at end of the buffer
(setq next-line-add-newlines nil)

;;; Fix delete key working as backspace and not forward deleting
;;; (This only worked in window mode, not terminal. C-d works in both)
(when window-system (normal-erase-is-backspace-mode 1))

;;; Alias to change apropos to ap
(defalias 'ap 'apropos)

;;; hl-line: highlight the current line
(when (fboundp 'global-hl-line-mode)
  (global-hl-line-mode t)) ;; turn it on for all modes by default

;;; Make text mode default major mode with auto-fill enabled
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode) ;replaces longlines in 23

;;; Auto-scroll in *Compilation* buffer
(setq compilation-scroll-output t)

;;; make Emacs always indent using SPC characters and never TABs
;;; i.e. use spaces instead of tabs
;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Just-Spaces.html
(setq-default indent-tabs-mode nil)

;;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;;; Highlight regions and add special behaviors to regions.
;;; "C-h d transient" for more info.  transient-mark-mode is a toggle.
;;; also in Emacs 22 and greater, C-SPC twice to temp enable transient mark
;(setq transient-mark-mode nil)
(setq transient-mark-mode t)

;;; Display line and column numbers in the mode line
(setq line-number-mode    t
      column-number-mode  t)

;;; Stop blinking cursor
(blink-cursor-mode 0)

;;; Explicitly show the end of a buffer (indicated on left fringe of window)
(set-default 'indicate-empty-lines t)

;;; Line-wrapping
(set-default 'fill-column 78)

;;; backups - commented out for better-defaults
;(setq make-backup-files t ;; do make backups
;  backup-by-copying t     ;; and copy them here
;  backup-directory-alist '(("." . "~/.emacs.d/cache/backups"))
;  version-control t
;  kept-new-versions 2
;  kept-old-versions 5
;  delete-old-versions t)

;; Create the autosave dir if necessary, since emacs won't.
;(make-directory "~/.emacs.d/cache/autosaves/" t)
;; Put all auto-save files in a single directory
;(defvar autosave-dir (expand-file-name "~/.emacs.d/cache/autosaves/"))
;(setq auto-save-list-file-prefix
;  "~/.emacs.d/cache/autosaves/saves-")

;; Don't truncate lines
(setq truncate-lines t
      truncate-partial-width-windows nil)

;; Create new scratch buffer if needed
(run-with-idle-timer 1 t
    '(lambda () (get-buffer-create "*scratch*")))

;; allow scroll-down/up-command to move point to buffer end/beginning
(setq scroll-error-top-bottom 'true)

;; New json-mode
(setq auto-mode-alist (cons '("\\.json\\'" . js-mode) auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bookmarks
;;    ‘C-x r m’ – set a bookmark at the current location (e.g. in a file)
;;    ‘C-x r b’ – jump to a bookmark
;;    ‘C-x r l’ – list your bookmarks
;;    ‘M-x bookmark-delete’ – delete a bookmark by name
(setq
  bookmark-default-file "~/org/bookmarks" ;; .emacs.d/bookmarks by default
  bookmark-save-flag 1)                   ;; autosave each change)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- system specific settings

;;; -=Linux specific settings
(if *is-linux*
   (progn

;;; http://stackoverflow.com/questions/15277172/how-to-make-emacs-open-all-buffers-in-one-window-debian-linux-gnome
;(setq pop-up-frames 'graphic-only)
(setq pop-up-frames nil)

;;; http://stackoverflow.com/questions/4506249/how-to-make-emacs-org-mode-open-links-to-sites-in-google-chrome
;;; Open up URLs in browser using gnome-open
(setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "gnome-open")

;;; Problems with minibuffer font size display in KDE/Crunchbang/Unity(?), explictily set font
;;; List fonts with M-x descript-font
;(set-default-font "Monospace-10")
)) ;end Linux specific settings=-

;;; -=Apple OSX specific settings
(if *is-a-mac*
   (progn

;; Don't open up new frames for files dropped on icon, use active frame
(setq ns-pop-up-frames nil)

;;; Drag and drop on the emacs window opens the file in a new buffer instead of appending it to the current buffer
;;; http://stackoverflow.com/questions/3805658/how-to-configure-emacs-drag-and-drop-to-open-instead-of-append-on-osx
(if (fboundp 'ns-find-file)
    (global-set-key [ns-drag-file] 'ns-find-file))

;;; Move deleted files to the System's trash can
;;; set trash-directory otherwise uses freedesktop.org-style
(setq trash-directory "~/.Trash")
(setq delete-by-moving-to-trash t)

;;; Change default font (was Monaco)
;;(set-default-font "Menlo-12")

;; For Macbook Pro, which has no insert key.
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2006-07/msg00220.html
(global-set-key (kbd "C-c I") (function overwrite-mode))

;;; Bring emacs.app to the foreground when running emacsclient
;;; http://stackoverflow.com/questions/945709/emacs-23-os-x-multi-tty-and-emacsclient
;; (add-hook 'server-visit-hook 'call-raise-frame)
;; (defun call-raise-frame ()
;;   (raise-frame))

;;; Set default font face and size in GUI
;(when (display-graphic-p) (set-face-attribute 'default nil :font "Monaco-12"))

;;; For w3m on MacOS (installed with brew), add /usr/local/bin to exec-path
;(add-to-list 'exec-path "/usr/local/bin")

;;; Open up URLs in mac browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
; (setq browse-url-browser-function 'browse-url-default-windows-browser)

;;; Hard set option/alt key to meta, command to nil (allow os to set, was hyper)
;;; http://www.emacswiki.org/emacs/EmacsForMacOS
;(when (eq system-type 'darwin) ;; mac specific settings
  ;(setq mac-option-modifier 'meta)
  ;(setq mac-command-modifier 'nil)
  ;(global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to right-delete
;)

;;; Copy and paste into Emacs Terminal
;;; stack overflow, pasting text into emacs on Macintosh
;;; Copy - C-x M-w
;;; Paste - C-x C-y
(defun pt-pbpaste ()
  "Paste data from pasteboard."
  (interactive)
  (shell-command-on-region
    (point)
    (if mark-active (mark) (point))
    "pbpaste" nil t))

(defun pt-pbcopy ()
  "Copy region to pasteboard."
  (interactive)
  (print (mark))
  (when mark-active
    (shell-command-on-region
      (point) (mark) "pbcopy")
    (kill-buffer "*Shell Command Output*")))

(global-set-key [?\C-x ?\C-y] 'pt-pbpaste)
(global-set-key [?\C-x ?\M-w] 'pt-pbcopy)


)) ;end Apple OSX specific settings=-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- function definitions

;;; ---------------------------------------------------------------------------
;;; Toggles term between line mode and char mode
;;; http://emacs.stackexchange.com/questions/5585/how-to-copy-command-output-in-ansi-term-mode
;;; https://joelmccracken.github.io/entries/switching-between-term-mode-and-line-mode-in-emacs-term/
(require 'term)
(defun jnm/term-toggle-mode ()
  "Toggles term between line mode and char mode"
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))
;; active when the buffer is in line mode
(define-key term-mode-map (kbd "C-c C-j") 'jnm/term-toggle-mode)
(define-key term-mode-map (kbd "C-c C-k") 'jnm/term-toggle-mode)
;; active when the buffer is in character mode
(define-key term-raw-map (kbd "C-c C-j") 'jnm/term-toggle-mode)
(define-key term-raw-map (kbd "C-c C-k") 'jnm/term-toggle-mode)

;;; ---------------------------------------------------------------------------
;;; Create parent directory when visiting file in non-existent directory
;;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p parent-directory))
                   (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
          (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

;;; ---------------------------------------------------------------------------
;;; https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
;;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim ()
  "If the buffer is narrowed, it widens. Otherwise, it narrows to region, or Org subtree."
  (interactive)
  (cond ((buffer-narrowed-p) (widen))
	((region-active-p) (narrow-to-region (region-beginning) (region-end)))
	((equal major-mode 'org-mode) (org-narrow-to-subtree))
	(t (error "Please select a region to narrow to"))))
(global-set-key (kbd "C-c n") 'narrow-or-widen-dwim)

;; I bind this key to C-c n, using the bind-key function that comes with use-package.
;(bind-key "C-c n" 'narrow-or-widen-dwim)

;; I also bind it to C-x t n, using Artur Malabarba's toggle map idea:
;; http:://www.endlessparentheses.com/the-toggle-map-and-wizardry.html

;;; ---------------------------------------------------------------------------
;;; http://www.reddit.com/r/emacs/comments/1zkj2d/advanced_usage_of_eshell
(defun ha-eshell-here ()
  "Opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (split-window-vertically)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(defun af-eshell-here ()
  "Go to eshell and set current directory to the buffer's directory"
  (interactive)
  (let ((dir (file-name-directory (or (buffer-file-name)
                                      default-directory))))
    (eshell)
    (eshell/pushd ".")
    (cd dir)
    (goto-char (point-max))
    (eshell-kill-input)
    (eshell-send-input)))

;;; ---------------------------------------------------------------------------
;;; Locked mode - lock a window in place
(define-minor-mode locked-buffer-mode
  "Make the current window always display this buffer."
  nil " locked" nil
  (set-window-dedicated-p (selected-window) locked-buffer-mode))
(global-set-key (kbd "C-c C-l") 'locked-buffer-mode)

;;; --------------------------------------------------------------------------
;;; Narrow-to-region-indirect (Allow narrowing of different regions in windows)
;;; http://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (when (boundp 'evil-mode) ; There's probably a nicer way to do this
    (evil-exit-visual-state))
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
      (switch-to-buffer buf)))

;;; ---------------------------------------------------------------------------
;;; Kill all other open files, other then the current one.  NO Notification!
;;; http://emacsredux.com/blog/2013/03/30/kill-other-buffers/
(defun kill-other-buffers ()
  "Kill all buffers but the current one.
   Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))
;(global-set-key (kbd "C-c k") 'kill-other-buffers)

;;; ---------------------------------------------------------------------------
;;; change a marked region of text to all low-cased words concatenated by underscores
;;; A fox caught a bird => a_fox_caught_a_bird
(defun lower-and-concat (b e)
  (interactive "r")
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (downcase-region b e)
    (while (re-search-forward "[ \t]+" nil t)
      (replace-match "_"))))

;;; ---------------------------------------------------------------------------
;;; Move lines up or down (can't easily use C-S on MacOS)
;;; http://whattheemacsd.com//editing-defuns.el-02.html
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

;(global-set-key (kbd "<C-S-down>") 'move-line-down)
;(global-set-key (kbd "<C-S-up>") 'move-line-up)
(global-set-key (kbd "<M-S-down>") 'move-line-down)
(global-set-key (kbd "<M-S-up>") 'move-line-up)

;;; ---------------------------------------------------------------------------
;;; Open a full screen eshell, save current layout
;;; http://irreal.org/blog/?p=1742
(global-set-key (kbd "C-c E")
                (lambda ()
                  "Bring up a full-screen eshell or restore previous config."
                  (interactive)
                  (if (string= "eshell-mode" major-mode)
                      (jump-to-register :eshell-fullscreen)
                    (progn
                      (window-configuration-to-register :eshell-fullscreen)
                      (eshell)
                      (delete-other-windows)))))

;;; ---------------------------------------------------------------------------
;;; Re-open a file with sudo access
;;; http://www.reddit.com/r/emacs/comments/192n52/how_do_i_save_ediffmerged_changes_for/
(defun sudo-file()
  (interactive)
  (let ((p (point)))
    (find-alternate-file
     (concat "/sudo::" (buffer-file-name)))
    (goto-char p)))

;;; ---------------------------------------------------------------------------
;;; Jump to next character - Similar to Vim's semicolon command
;;; http://www.reddit.com/r/emacs/comments/nfj0e/emacs_wizards_how_do_you_move_about_in_source_code/
;(defun jump-to-next-char (c &optional count)
;  "Jump forward or backward to a specific character.  With a
;count, move that many copies of the character."
;  (interactive "cchar: \np")
;  (when (string= (string c) (buffer-substring (point) (+ 1 (point))))
;    (setq count (+ 1 count)))
;  (and
;   (search-forward (string c) nil t count)
;   (> count 0)
;   (backward-char)))
;(global-set-key (kbd "C-;") 'jump-to-next-char)

;;; ---------------------------------------------------------------------------
;;; Remove duplicate lines in a region
;;; http://stackoverflow.com/questions/13046791/how-to-delete-the-repeat-lines-in-emacs
(defun uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))

;;; ---------------------------------------------------------------------------
;;; Match Paren / based on the vim command using %
;;; http://grok2.tripod.com/
(defun match-paren (arg)
      "Go to the matching paren if on a paren; otherwise insert %."
      (interactive "p")
      (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
            ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
            (t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

;;; ---------------------------------------------------------------------------
;;; Unfill paragraph / Unfill region
;;; http://stackoverflow.com/questions/6707758/inverse-of-m-q-an-unfill-paragraph-function
(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the reverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the reverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

;;; ---------------------------------------------------------------------------
;;; Remove all space indentation
;;; http://stackoverflow.com/questions/10854776/emacs-function-which-eliminates-all-indentation
(defun my-delete-indentation (start end)
  "Delete all leading whitespace within the current region."
  (interactive "*r")
  (replace-regexp "^[[:space:]]+" "" nil start end))

;;; ---------------------------------------------------------------------------
;;; Remote ssh connection from within Emacs
;;; http://stackoverflow.com/questions/10495432/remote-ssh-connection-from-within-emacs
(defun my-ssh (args)
  "Connect to a remote host by SSH."
  (interactive "sssh ")
  (let ((switches (split-string-and-unquote args)))
    (set-buffer (apply 'make-term "ssh" "ssh" nil switches))
    (term-mode)
    (term-char-mode)
    (switch-to-buffer "*ssh*")))

;;; ---------------------------------------------------------------------------
;;; Save list of open files to kill-ring (by Trey Jackson)
;;; http://stackoverflow.com/questions/10537265/emacs-save-current-buffer-list-to-a-text-file
(defun copy-open-files ()
  "Add paths to all open files to kill ring"
  (interactive)
  (kill-new (mapconcat 'identity
                       (delq nil (mapcar 'buffer-file-name (buffer-list)))
                       "\n"))
  (message "List of files copied to kill ring"))

;;; ---------------------------------------------------------------------------
;;; Use a bar cursor when mark is active and a region exists.
;;; http://www.reddit.com/r/emacs/comments/stkb1/im_really_liking_the_bar_cursor/
(defun th-activate-mark-init ()
  (setq cursor-type 'bar))
(add-hook 'activate-mark-hook 'th-activate-mark-init)

(defun th-deactivate-mark-init ()
  (setq cursor-type 'box))
(add-hook 'deactivate-mark-hook 'th-deactivate-mark-init)

;; Use a red cursor in overwrite-mode
(defvar th--default-cursor-color "black")
(defadvice overwrite-mode (after th-overwrite-mode-change-cursor activate)
  "Change cursor color in override-mode."
  (if overwrite-mode
      (progn
        (setq th--default-cursor-color
              (let ((f (face-attribute 'cursor :background)))
                (if (stringp f)
                    f
                  th--default-cursor-color)))
        (set-cursor-color "red"))
    (set-cursor-color th--default-cursor-color)))

;;; ---------------------------------------------------------------------------
;;; I want a key to open the current buffer all over the screen.
;;; http://stackoverflow.com/questions/970292/emacs-multiple-columns-one-buffer
(defun all-over-the-screen ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (follow-mode t))

;;; ---------------------------------------------------------------------------
;;; Clear shell (m-x shell) screen like bash's clear command
(defun clear-shell ()
   (interactive)
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)))
(global-set-key "\C-xc" 'clear-shell)

;;; ---------------------------------------------------------------------------
;;; Save rectangle instead of killing it
;;; http://emacsblog.org/2007/03/17/quick-tip-set-goal-column
(defun kill-save-rectangle (start end &optional fill)
  "Save the rectangle as if killed, but don't kill it.  See
`kill-rectangle' for more information."
  (interactive "r\nP")
  (kill-rectangle start end fill)
  (goto-char start)
  (yank-rectangle))
(global-set-key (kbd "C-x r M-k") 'kill-save-rectangle)

;;; ---------------------------------------------------------------------------
;;; Steve Yegge's function rename a file that you're editing along
;;; with its corresponding buffer
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
 (filename (buffer-file-name)))
    (if (not filename)
 (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
   (message "A buffer named '%s' already exists!" new-name)
 (progn
   (rename-file name new-name 1)
   (rename-buffer new-name)
   (set-visited-file-name new-name)
   (set-buffer-modified-p nil))))))

;;; ---------------------------------------------------------------------------
;;; http://www.reddit.com/r/emacs/comments/gjqki/is_there_any_way_to_tell_emacs_to_not/c1o26uk
(defun toggle-sticky-buffer-window ()
 "Toggle whether this window is dedicated to this buffer."
 (interactive)
 (set-window-dedicated-p
  (selected-window)
  (not (window-dedicated-p (selected-window))))
 (if (window-dedicated-p (selected-window))
     (message "Window is now dedicated.")
   (message "Window is no longer dedicated.")))

(global-set-key [(super d)] 'toggle-sticky-buffer-window) ;; cmd-d

;;; ---------------------------------------------------------------------------
(defun intelligent-close ()
  "quit a frame the same way no matter what kind of frame you are on.

This method, when bound to C-x C-c, allows you to close an emacs frame the
same way, whether it's the sole window you have open, or whether it's
a \"child\" frame of a \"parent\" frame.  If you're like me, and use emacs in
a windowing environment, you probably have lots of frames open at any given
time.  Well, it's a pain to remember to do Ctrl-x 5 0 to dispose of a child
frame, and to remember to do C-x C-x to close the main frame (and if you're
not careful, doing so will take all the child frames away with it).  This
is my solution to that: an intelligent close-frame operation that works in
all cases (even in an emacs -nw session).

Stolen from http://www.dotemacs.de/dotfiles/BenjaminRutt.emacs.html."
  (interactive)
  (if (eq (car (visible-frame-list)) (selected-frame))
      ;;for parent/master frame...
      (if (> (length (visible-frame-list)) 1)
          ;;close a parent with children present
          (delete-frame (selected-frame))
        ;;close a parent with no children present
        (save-buffers-kill-emacs))
    ;;close a child frame
    (delete-frame (selected-frame))))
(global-set-key "\C-x\C-c" 'intelligent-close) ;forward reference


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- ansi-term    (C-c C-j - activate term-line-mode, C-c C-k - deactivate it)
;;; ansi-term tricks from:
;;; http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/

;; (require 'term nil 'noerror)
;; (defun visit-ansi-term ()
;;   "If the current buffer is:
;;      1) a running ansi-term named *ansi-term*, rename it.
;;      2) a stopped ansi-term, kill it and create a new one.
;;      3) a non ansi-term, go to an already running ansi-term
;;         or start a new one while killing a defunt one"
;;   (interactive)
;;   (let ((is-term (string= "term-mode" major-mode))
;;         (is-running (term-check-proc (buffer-name)))
;;         (term-cmd "/bin/bash")
;;         (anon-term (get-buffer "*ansi-term*")))
;;     (if is-term
;;         (if is-running
;;             (if (string= "*ansi-term*" (buffer-name))
;;                 (call-nteractively 'rename-buffer)
;;               (if anon-term
;;                   (switch-to-buffer "*ansi-term*")
;;                 (ansi-term term-cmd)))
;;           (kill-buffer (buffer-name))
;;           (ansi-term term-cmd))
;;       (if anon-term
;;           (if (term-check-proc "*ansi-term*")
;;               (switch-to-buffer "*ansi-term*")
;;             (kill-buffer "*ansi-term*")
;;             (ansi-term term-cmd))
;;        (ansi-term term-cmd)))))
;(global-set-key (kbd "<f5>") 'visit-ansi-term)
;(global-set-key (kbd "C-c t") 'visit-ansi-term)

;;; http://stackoverflow.com/questions/3295091/emacs-ansi-term-background-color
;;; Terminal Colors - Remove term's default white background color
;(setq term-default-bg-color "#211E1E")
;(setq term-default-fg-color "#AAAAAA")
(setq term-default-bg-color nil) ;; remove background color


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- better-defaults  (moved into init.el to avoid confusion)
;;; https://github.com/technomancy/better-defaults
;(require 'better-defaults)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(require 'uniquify)
 (setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
  (setq-default save-place t)

 (global-set-key (kbd "M-/") 'hippie-expand)
;(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(show-paren-mode 1)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- cisco router mode
;(autoload 'cisco-router-mode "cisco-router-mode" "edit cisco .cfg files" t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- company mode - a text completion framework
;;; https://company-mode.github.io/
;(add-hook 'after-init-hook 'global-company-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- daily log -

(defun daily-log ()
  "Automatically opens my daily log file and positions cursor at end of
last sentence."
  (interactive)
  ;(diary)
  (find-file "~/org/DailyLogs/+current") ;symlink to current log
  (goto-char (point-max))  ;go to the maximum accessible value of point
  (search-backward "* Notes") ;search to Notes section first to bypass notes
  (if (re-search-backward "[.!?]") ;search for punctuation from end of file
      (forward-char 1))
  )
(global-set-key [f9] 'daily-log)

;;; ---------------------------------------------------------------------------
;(diary)

;; Email 1
;; I have been using a simple system for writing notes day by day.  Kind of
;; like a diary.  It's really very unsophisticated but helpful.  It will allow
;; you to make notes into a template file.  Weeks, Months (etc...) later, you
;; can refer to them.
;;
;; For those who have never seen it
;; http://aonws01/unix-admin/Daily_Logs/Jerry_Sievers/
;;
;; Many of you new guys' questions to me have been answered from these notes
;; (eg, license keys info, who's who and so forth).
;;
;; John Sconiers asked about this and I set him up with it.  Whole procedure
;; takes only a few minutes to install and probably about fifteen minutes per
;; day to keep up to date.  An investment in time that pays off later.  Other
;; admins who have left Aon used this and liked it too.
;;
;; It also comes with a CGI program which, if your home directory is
;; accessible to aonws01, can allow others to browse your diary (I hear
;; cheering and booing...)
;;
;; Please let me know.  It would be nice to have everyone using this thing at
;; least minimally.

;; Email 2
;; Chris, I have installed the package in your home directory.  Files are in
;; Aon/DailyLogs.  The current log has a symbolic link named +Current.  You
;; also have an alias 'diary' which you can type at the shell.  Doing so will
;; invoke vi on the +Current file and position the cursor on the very last '.'
;; character in the file.  I have added the $HOME/bin directory to your path
;; and created one cron job to stamp the 'monday' file weekly.
;;
;; You should run the command 'new-daily-log' once per week to start a new
;; file.  Optionally, the previous file can be emailed to the destination of
;; your choice.  See the Aon/DailyLogs/.config file for details.
;;
;; Please call if you have any questions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- deft - an Emacs mode for quickly browsing, filtering, and editing
;;; directories of plain text notes.  http://jblevins.org/projects/deft/
;;; http://jblevins.org/git/deft.git
(autoload 'deft "deft" "mode for working with text notes" t)

(setq
  deft-extension "txt"
  deft-directory "~/org/notes"
  deft-text-mode 'org-mode
  deft-use-filename-as-title t
  deft-recursive t
  deft-ignore-file-regexp "archive\.*")
;(global-set-key (kbd "<f8>") 'deft))
(global-set-key (kbd "C-c C-g") 'deft-find-file)

;; Notational Velocity provides a show-hide function key, letting you pop
;; in-and-out of the interface quickly. I recreated a crude version of this
;; in Deft, bound to f6.
(define-minor-mode deft-note-mode "Deft notes" nil " Deft-Notes" nil)
(setq deft-text-mode 'deft-note-mode)
(defun kill-all-deft-notes ()
  (interactive)
  (save-excursion
    (let((count 0))
      (dolist(buffer (buffer-list))
        (set-buffer buffer)
        (when (not (eq nil deft-note-mode))
          (setq count (1+ count))
          (kill-buffer buffer)))
      )))
(defun deft-or-close () (interactive) (if (or (eq major-mode 'deft-mode) (not (eq nil deft-note-mode)))
                                          (progn (kill-all-deft-notes) (kill-buffer "*Deft*"))
                                        (deft)
                                        ))
(global-set-key [f8] 'deft-or-close)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- dired - directory listing buffer
;;; http://www.emacswiki.org/emacs/DiredPlus

;;; Toggle Unix hidden file display (M-o)
;(require 'dired+ nil 'noerror) ;; uber enhanced dired, very large?
(require 'dired-x nil 'noerror) ;included with emacs
(define-key ctl-x-map   "d" 'diredp-dired-files)
(define-key ctl-x-4-map "d" 'diredp-dired-files-other-window)

;; hide hidden files by default, use (M-o) to show
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;;; New dired+ option is to hide file details, can be togged with '('
;(setq diredp-hide-details-initially-flag -1) ;doesn't seem to work
;(setq global-dired-hide-details-mode -1)
;(setq dired-recursive-deletes 'top)
;; disable line wrapping in dired mode
(add-hook 'dired-mode-hook (lambda () (setq truncate-lines t)))

;;; http://stackoverflow.com/questions/1824696/function-to-call-same-shell-command-in-dired
;;; http://blog.nguyenvq.com/2009/12/01/file-management-emacs-dired-to-replace-finder-in-mac-os-x-and-other-os/
;; (defun dired-open ()
;;   (interactive)
;;   (save-window-excursion
;;     (dired-do-async-shell-command
;;      "open" current-prefix-arg
;;      (dired-get-marked-files t current-prefix-arg))))
;;   (define-key dired-mode-map (kbd "C-o") 'dired-open)

;;; Open the current directory in desktop
;;; http://xahlee.blogspot.com/2012/01/emacs-dired-opening-files-in-external.html
;; (defun open-in-desktop ()
;;   "Open the current file in desktop.
;; Works in Microsoft Windows, Mac OS X, Linux."
;;   (interactive)
;;   (cond
;;    ((string-equal system-type "windows-nt")
;;     (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
;;    ((string-equal system-type "darwin") (shell-command "open ."))
;;    ((string-equal system-type "gnu/linux") (shell-command "xdg-open ."))
;;    ) )
;; (define-key dired-mode-map (kbd "C-;") 'open-in-desktop)

;; Make sizes human-readable by default, sort version numbers
;; correctly, and put dotfiles and capital-letters first.
;(setq-default dired-listing-switches "-alhvi")

;;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Handle zip compression
(defvar dired-compress-file-suffixes)
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

;; Configure direx jump
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(define-key global-map "\C-x\C-j" 'dired-jump)
(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)

;; enable side-by-side dired buffer targets
;; Split your window, split-window-vertically & go to another dired directory.
;; When you will press C to copy, the other dir in the split pane will be
;; default destination.
(setq dired-dwim-target t) ;; suggest copying/moving to other dired buffer in split view


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- ehelp (Electric help)
;; Provides a pre-packaged 'Electric Help Mode' for on-line help screens.
;; Provides: SPC to scroll, DEL to scroll back, q to exit, r to retain
;; Instead of: Type C-x 1 to delete the help window, C-M-v to scroll help
(require 'ehelp)
(define-key global-map "\C-h" 'ehelp-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- elpy - Emacs Python IDE
;; https://realpython.com/blog/python/emacs-the-best-python-editor/
;; To use elpy, just add the following to your .emacs:
(elpy-enable)
;; If you find the (Python Elpy yas AC ElDoc Fill) mode line annoying, also add:
;(elpy-clean-modeline)

;; replace flymake with flycheck for on-the-fly checking
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Emable pep8 auto correct erros on save
;(require 'py-autopep8)
;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- eshell - shell written in Emacs Lisp
;;;
;;; define eshell aliases
(setq eshell-directory-name "~/.emacs.d/eshell")
(defalias 'ff 'find-file)
(defalias 'ffo 'find-file-other-window)

;;; protect the prompt from being overwritten
(setq comint-prompt-read-only t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- exec-path-from-shell
;;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;;; Disable flycheck running checkdoc against init.el
;;; http://stackoverflow.com/questions/15552349/flycheck-how-to-disable-warnning-while-edit-emacs-lisp-scripts
;(eval-after-load 'flycheck (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))  ;; ver 0.16+
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- git-gutter
;;; https://github.com/syohex/emacs-git-gutter
(global-git-gutter-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- gnus - Mail and News reader

;;; Define how Gnus is to fetch news
;(setq gnus-select-method '(nntp "nntp.aioe.org"))
(setq gnus-select-method '(nntp "news.eternal-september.org"))
;(setq nntp-authinfo-file "~/.authinfo.gpg") ;use gpg encrypted authfile
(setq nntp-authinfo-function 'nntp-send-authinfo)

;; setup summary buffer
(setq gnus-summary-line-format "%U%R%z%(%[%4L: %-20,20f%]%)%B %s\n"
      gnus-summary-same-subject "")

(setq gnus-sum-thread-tree-root " >"
      gnus-sum-thread-tree-single-indent "  "
      gnus-sum-thread-tree-vertical "|"
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "+-> "
      gnus-sum-thread-tree-single-leaf "`-> ")

;; show even if there are no articles
(setq gnus-permanently-visible-groups ".*")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- ibuffer - *Nice* buffer switching
;;
;; ibuffer filtering
;;
;; Search all marked buffers
;;   ‘M-s a C-s’ - Do incremental search in the marked buffers.
;;   ‘M-s a C-M-s’ - Isearch for regexp in the marked buffers.
;;   ‘U’ - Replace by regexp in each of the marked buffers.
;;   ‘Q’ - Query replace in each of the marked buffers.
;;   ‘I’ - As above, with a regular expression.

(autoload 'ibuffer "ibuffer" "List buffers." t)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Don't show empty buffer groups
(setq ibuffer-show-empty-filter-groups nil)

;; work groups for ibuffer
(setq ibuffer-saved-filter-groups
      '(("default"
         ("version control" (or (mode . svn-status-mode)
                   (mode . svn-log-edit-mode)
                   (name . "^\\*svn-")
                   (name . "^\\*vc\\*$")
                   (name . "^\\*Annotate")
                   (name . "^\\*vc-")
                   (name . "^\\*git-")
                   (name . "^\\*magit")))
         ("emacs" (or (name . "^\\*scratch\\*$")
                      (name . "^\\*Messages\\*$")
                      (name . "^TAGS\\(<[0-9]+>\\)?$")
                      (name . "^\\*info\\*$")
                      (name . "^\\*Occur\\*$")
                      (name . "^\\*grep\\*$")
                      (name . "^\\*Compile-Log\\*$")
                      (name . "^\\*Backtrace\\*$")
                      (name . "^\\*Process List\\*$")
                      (name . "^\\*gud\\*$")
                      (name . "^\\*Man")
                      (name . "^\\*WoMan")
                      (name . "^\\*Kill Ring\\*$")
                      (name . "^\\*Completions\\*$")
                      (name . "^\\*tramp")
                      (name . "^\\*shell\\*$")
                      (name . "^\\*compilation\\*$")))
         ("Helm" (or (name . "\*helm\*")))
         ("Help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*")))
         ("emacs-source" (or (mode . emacs-lisp-mode)
                             (filename . "/Applications/Emacs.app")
                             (filename . "/bin/emacs")))
         ("emacs-config" (or (filename . ".emacs.d")
                             (filename . "emacs-config")))
         ("org" (or (name . "^\\*org-")
                    (name . "^\\*Org")
                    (mode . org-mode)
                    (mode . muse-mode)
                    (name . "^\\*Calendar\\*$")
                    (name . "^+current$")
                    (name . "^diary$")
                    (name . "^\\*Agenda")))
         ("latex" (or (mode . latex-mode)
                      (mode . LaTeX-mode)
                      (mode . bibtex-mode)
                      (mode . reftex-mode)))
         ("dired" (or (mode . dired-mode)))
         ("perl" (mode . cperl-mode))
         ("erc" (mode . erc-mode))
         ("shell" (or (mode . shell-mode)
                        (name . "^\\*terminal\\*$")
                        (name . "^\\*ansi-term\\*$")
                        (name . "^\\*shell\\*$")
                        (name . "^\\*eshell\\*$")))
         ("gnus" (or (name . "^\\*gnus trace\\*$")
                        (mode . message-mode)
                        (mode . bbdb-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (name . "^\\.bbdb$")
                        (name . "^\\.newsrc-dribble"))))))

;; Order the groups so the order is : [Default], [agenda], [emacs]
(defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups ()
                                                 activate)
  (setq ad-return-value (nreverse ad-return-value)))

;; Hide the following buffers
;;(setq ibuffer-never-show-predicates
;;      (list "\\*Completions\\*"
;;            "\\*vc\\*"))

;; Enable ibuffer expert mode, don't prompt on buffer deletes
(setq ibuffer-expert t)

;; Load the 'work' group, can set to load groups by location
;; ibuffer-auto-mode is a minor mode that automatically keeps the buffer
;; list up to date. I turn it on in my ibuffer-mode-hook:
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "default")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- ispell - interactive spell
;;; Set ispell checks to use aspell
;;; on mac:  `brew install aspell --lang=en` (instead of ispell)
(eval-after-load "ispell"
  '(progn
     (setq ispell-program-name "aspell")
     (setq ispell-list-command "list")
     ;; sug-mode=fast is more accurate, slower then ultra
     (setq ispell-extra-args '("--sug-mode=fast"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- ivy-mode - a generic completion mechanism for Emacs
;;; swiper - an alternative to isearch uses ivy to show overview of all matches
;;; https://github.com/abo-abo/swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-virtual-abbreviate 'full)
(setq enable-recursive-minibuffers t)

;; Ivy-based interface to standard commands
(global-set-key (kbd "C-s") 'swiper)
;(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;(global-set-key (kbd "<f1> f") 'counsel-describe-function)
;(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;(global-set-key (kbd "<f1> l") 'counsel-find-library)
;(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;; Ivy-based interface to shell and system tools
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; disable M-x filter "^"
(setq ivy-initial-inputs-alist nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- magit-mode - emacs mode for interacting with the Git vcs
(global-set-key (kbd "C-c g") 'magit-status)  ;;
(setq magit-completing-read-function 'ivy-completing-read)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- sql
;;; sql-mysql configuration

;;; highlight mysql keywords
(add-hook 'sql-mode-hook
          (lambda ()
            (sql-highlight-mysql-keywords)))

;;; hilight postgres keywords
(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)

;; Make mysql not buffer sending stuff to the emacs-subprocess-pipes
;; -n unbuffered -B batch(tab separated) -f force(go on after error)
;; -i ignore spaces -q no caching -t table format
(setq-default sql-mysql-options (quote ("-n" "-B" "-f" "-i" "-q" "-t")))

;; set prompt back to default
(setq sql-mysql-options '("--prompt=mysql> "))
;(sql-set-product-feature 'mysql :prompt-regexp "^[mM]y[sS][qQ][lL][^>]*> ")

;; save history between sessions
  (defun my-sql-save-history-hook ()
    (let ((lval 'sql-input-ring-file-name)
          (rval 'sql-product))
      (if (symbol-value rval)
          (let ((filename
                 (concat "~/.emacs.d/cache/"
                         (symbol-name (symbol-value rval))
                         "-history.sql")))
            (set (make-local-variable lval) filename))
        (error
         (format "SQL history will not be saved because %s is nil"
                 (symbol-name rval))))))

  (add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- neo-tree
;;; Emacs version of Vim's nerdtree
(global-set-key [f5] 'neotree-toggle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- nov.el - Major mode for reading epubs
;;; https://github.com/wasamasa/nov.el
;;; Open the EPUB file with C-x C-f ~/novels/novel.epub, scroll with SPC and
;;; switch chapters with n and p. More keybinds can be looked up with F1 m.
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- org-mode - for keeping notes, maintaining TODO lists, doing project
;;; planning, and authoring with a fast and effective plain-text system
;;; Moved to init-org-mode.el for ease of customization
(load "~/.emacs.d/org.conf")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- projectile - project managent (works with helm)
;;; Project navigation and management library for Emacs
;;; https://github.com/bbatsov/projectile
;;; http://tuhdo.github.io/helm-projectile.html

;; put bookmark in cache directory
(setq projectile-known-projects-file "~/.emacs.d/cache/projectile-bookmarks.eld")

;; use helm projectile C-c p h
(projectile-global-mode)
;(setq projectile-completion-system 'helm)
;;(helm-projectile-on)

;; force use external commands for indexing
(setq projectile-indexing-method 'alien)

;; enable caching unconditionally use this snippet of code:
(setq projectile-enable-caching t)

;;(setq projectile-switch-project-action 'helm-projectile-find-file)
;(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-completion-system 'ivy)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- puppet-mode
;;; Setup puppet-mode for autoloading
;;; http://www.emacswiki.org/emacs/PuppetProgramming
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-hook 'puppet-mode-hook 'linum-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- recentf - a minor mode that builds a list of recently opened files
;;; http://www.masteringemacs.org/articles/2011/01/27/find-files-faster-recent-files-package/
(autoload 'recentf "recentf" "List recent files" t)
(setq
  recentf-save-file "~/.emacs.d/cache/recentf"
  recentf-max-saved-items 200      ;; max save file cache
  recentf-max-menu-items 15)      ;; max 15 in menu
(recentf-mode t)                  ;; turn it on
;(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;(global-set-key (kbd "C-x C-r") 'helm-recentf)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- shell-pop
;;; https://github.com/kyagi/shell-pop-el
(require 'shell-pop)
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/bash")
  ;(setq shell-pop-universal-key "C-t")
  (global-set-key (kbd "C-c t") 'shell-pop)
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- switch-window - a C-x o replacment for multiple window movement
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- tramp- edit files on remote servers
(autoload 'tramp "tramp" "Tramp mode" t)

;(custom-set-variables '(tramp-verbose 10)) ;; debugging info
;(setq tramp-debug-buffer t)

;; we need a bit more funky pattern, as tramp will start $SHELL
;; (sudo -s), ie., zsh for root user
(setq shell-prompt-pattern "^[^a-zA-Z].*[#$%>] *")
;(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
(setq
  tramp-default-method "sshx"  ;; inline method w/diff interactive shell
  ;tramp-default-method "scpx" ;; external method w/diff interactive shell
  tramp-persistency-file-name "~/.emacs.d/cache/tramp"
  tramp-auto-save-directory "~/.emacs.d/cache/autosaves"
  tramp-terminal-type "dumb")  ;; fix remote shell hanging, change .bashrc to
                               ;; for TERM=dumb and set PS1='$ '

;; fix "ls does not support --dired; see `dired-use-ls-dired' for more details."
;; it seems that only GNU ls supports --dired
(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

;; Speed up tramp by disabling version control on remote files
;; http://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; Set remote shell to be /bin/bash
;; http://superuser.com/questions/454288/how-to-specify-for-emacs-tramp-which-remote-shell-to-open
(setq explicit-shell-file-name "/bin/bash")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- treemacs -
;(setq treemacs-show-hidden-files nil)
;(global-set-key [f5] 'treemacs-toggle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- undo-tree
(require 'undo-tree)
;; If you want to replace the standard Emacs' undo system with the
;; 'undo-tree-mode' system in all buffers, you can enable it globally by
(global-undo-tree-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

;;; M-g M-g - goto-line
