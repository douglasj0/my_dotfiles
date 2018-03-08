;;; NOTE, eww should be used instead now - dbj
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- w3m-mode - Emacs interface to the w3m text browser

;(message "*** Loaded w3m.el")

(setq w3m-use-cookies t)

;; lnum mode - press 'f' then the # next to the link, similar to ace-jump-mode - slow load
;(w3m-lnum-mode 1)

;; enable tab mode
;; C-c C-t - create, C-c C-w - close, C-c C-[np] - navigate, C-c C-s - navigate
(setq w3m-use-tab t)

;; for better navigation (use arrow keys to navigate)
;(setq w3m-key-binding 'info)

;; enable images (T to toggle)
;(setq w3m-default-display-inline-images t)
;(setq w3m-display-inline-images t)

;; From wiki, fix encoding issues
(setq w3m-coding-system 'utf-8
          w3m-file-coding-system 'utf-8
          w3m-file-name-coding-system 'utf-8
          w3m-input-coding-system 'utf-8
          w3m-output-coding-system 'utf-8
          w3m-terminal-coding-system 'utf-8)

;;; --- config from beatofthegeek.com ---
;; http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html

;;change default browser for 'browse-url' to w3m
;(setq browse-url-browser-function 'w3m-goto-url-new-session)

;;change w3m user-agent to android to get mobile versions of sites
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")

;;quick access hacker news (M-x hn)
(defun hn ()
  (interactive)
  (browse-url "http://news.ycombinator.com"))

;;quick access reddit (M-x reddit)
(defun reddit (reddit)
  "Opens the REDDIT in w3m-new-session"
  (interactive (list
               (read-string "Enter the reddit (default: psycology): " nil nil "psychology" nil)))
  (browse-url (format "http://m.reddit.com/r/%s" reddit))
)

;;quick access to wikipedia search (search for word under cursor) (M-x wikipedia)
(defun wikipedia-search (search-term)
  "Search for SEARCH-TERM on wikipedia"
  (interactive
   (let ((term (if mark-active
                   (buffer-substring (region-beginning) (region-end))
               (word-at-point))))
     (list
      (read-string
       (format "Wikipedia (%s):" term) nil nil term)))
  )
 (browse-url
  (concat
   "http://en.m.wikipedia.org/w/index.php?search="
   search-term
  ))
)

;;when I want to enter the web address all by hand, (M-x open-site)  NOTE: load w3m first
(defun w3m-open-site (site)
  "Opens site in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address(default: w3m-home):" nil nil w3m-home-page nil )))
  (w3m-goto-url-new-session
   (concat "http://" site)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))) t)
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))) t))

;----------------------------------------------------------------------

;;; org-mode function to check checkbox and move to next in list
;;; http://superuser.com/questions/568482/org-mode-function-to-check-checkbox-and-move-to-next-in-list
(defun zin/org-checkbox-next ()
  (interactive)
  (when (org-at-item-checkbox-p)
    (org-toggle-checkbox))
  (org-next-item))

;----------------------------------------------------------------------

;; Capture templates - C-c c t
(setq org-capture-templates
 '(("t" "Todo" entry (file+headline (concat org-directory "/gtd.org") "Tasks")
        "* TODO %?\n %i\n %a")
   ("n" "Note" entry (file+datetree (concat org-directory "/notes.org") "Notes")
        "* %?\nEntered on %U\n  %i\n  %a")
   ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org") "Journel")
        "* %?\nEntered on %U\n  %i\n  %a")
   ("l" "Link" plain (file (concat org-directory "/links.org"))
         "- %?\n %x\n"))) ; save contents of clipboard to links.org (URLs)

;----------------------------------------------------------------------

;(require 'direx)
;(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)

;(defun notes ()
;  "Switch to my work dir."
;   (interactive)
;   (find-file "~/org/deft")
;   )

;;; Smartscan minor mode (M-x smartscan-mode)
;; https://github.com/mickeynp/smart-scan
;; M-n and M-p move between symbols and type M-' to replace all symbols
;;  in the buffer matching the one under point, and C-u M-' to replace
;;  symbols in your current defun only (as used by narrow-to-defun.)
;(smartscan-mode 1)

;;; Guru Mode
;;; Guru mode disables some common keybindings and suggests the use of the
;;; established Emacs alternatives instead.
;;; https://github.com/bbatsov/guru-mode
;;;(add-to-list 'load-path "~/emacs.d/vendor")
;(require 'guru-mode)
;(guru-global-mode +1)

;;; Cobol Mode
;;(require 'cobol-mode)      ; Unconditional load
;; commented out, interfering with magit!
;(autoload 'cobol-mode "cobol-mode" "Major mode for Tandem COBOL files." t nil)
;(setq auto-mode-alist
;   (append
;     '(("\\.cob\\'" . cobol-mode)         ;extension of .cob means cobol-mode
;       ("\\([\\/]\\|^\\)[^.]+$" . cobol-mode)) ;so does no extension at all.
;    auto-mode-alist))

;;; occur-dwim (instead of plain M-s o)
;;; http://oremacs.com/2015/01/26/occur-dwim/
;;; It will offer as the default candidate:
;;;    the current region, if it's active
;;;    the current symbol, otherwise
;(defun occur-dwim ()
;  "Call `occur' with a sane default."
;  (interactive)
;  (push (if (region-active-p)
;            (buffer-substring-no-properties
;             (region-beginning)
;             (region-end))
;          (thing-at-point 'symbol))
;        regexp-history)
;  (call-interactively 'occur))

;----------------------------------------------------------------------

;; Don't iconify frame when running under a window system
;(if (display-graphic-p) (progn (global-set-key "\C-Z" nil)))
;; Instead ruUse double C-z for suspend frame
;(global-unset-key (kbd "C-z"))
;(global-set-key (kbd "C-z C-z") 'my-suspend-frame)
;(defun my-suspend-frame ()
;  "In a GUI environment, do nothing, otherwise 'suspend-frame'."
;  (interactive)
;  (if (display-graphic-p)
;      (message "suspend-frame disabled for graphical displays.")
;    (suspend-frame)))

;----------------------------------------------------------------------

; packages:  flx-ido, flx, ido-ubiquitous, ido-vertical-mode

;;; -- ido - Interactive do
;;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
;;; http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode

;|--------+------------------------------------------------------------------|
;| C-b    | Reverts to the old switch-buffer completion engine               |
;| C-f    | Reverts to the old find-file completion engine                   |
;| C-d    | Opens a dired buffer in the current directory                    |
;| C-a    | Toggles showing ignored files (see ido-ignore-files)             |
;| C-c    | Toggles if searching of buffer and file names should ignore case |
;| TAB    | Attempt to complete the input                                    |
;| C-p    | Toggles prefix matching                                          |
;| C-s/C-r| Moves to the next and previous match, respectively               |
;| C-t    | Toggles matching by Emacs regular expression.                    |
;| BKSP   | Deletes characters as usual or goes up one directory             |
;| C-SPC  | Restricts completion list to anything that matches your input    |
;| //     | ignore the preceding path, and go back to the top-most directory |
;| ~/     | Jumps to the home directory.                                     |
;| M-d    | Searches for the input in all sub-directories of current         |
;| C-k    | Kills the currently focused buffer or deletes the file           |
;| M-m    | Creates a new sub-directory to the directory you're in           |
;|--------+------------------------------------------------------------------|

;;; Enable ido mode to work with C-x C-f (find files)
;;; C-x C-f C-f will revert to old style completion engine
;;; or try C-x C-f C-e to enter path EDIT mode.  Useful for tramp.

;;; Enable flx mode (flx.el and flx-ido.el) - https://github.com/lewang/flx
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1) ; ido-ubiquitous, disable to use lusty-explorer
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(setq gc-cons-threshold 20000000)

(add-to-list 'ido-ignore-files "\\.DS_Store")

;; If Recentf is enabled, you can use [C-x b] (or M-x ido-switch-buffer) to
;; visit recently closed files by enabling virtual buffers
(setq ido-use-virtual-buffers t)

;; Create new buffer if nothing found (always, prompt, never)
(setq ido-create-new-buffer 'always)
(setq confirm-nonexistent-file-or-buffer nil)

;;; Disable the merging (the "looking in other directories" in ido vulgo)
;;; But can also undo the merge with C-z in ido
(setq ido-auto-merge-work-directories-length -1)  ;; breaks searching in dirs

;;; when using ido, the confirmation is rather annoying...
;;; but, when this is active ido does a double confirmation
;;; ignoring the first y and requiring a second y, disabling!
;;;
;;; testing it again 3/2/14
(setq confirm-nonexistent-file-or-buffer nil)

(setq ido-file-extensions-order '(".org" ".txt" ".py" ".pp" ".md"
                                  ".xml" ".el" ".ini" ".cfg" ".cnf"))

;; Disable ido's directory cache, acts up over flaky network links
(setq ido-save-directory-list-file nil)

(setq
  ;ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
  ido-save-directory-list-file nil
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
    "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
  ;ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
  ido-case-fold  t                 ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ;ido-enable-last-directory-history nil ;forget latest selected dir name
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)
  ;ido-enable-flex-matching nil     ; don't try to be too smart (fuzzy matching)
  ido-enable-flex-matching t       ; enable fuzzy matching
  ido-max-prospects 8              ; don't spam my minibuffer
  ido-confirm-unique-completion t  ; wait for RET, even with unique completion
)

;--------------------------------------------------------------

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
;(global-set-key "\C-x\ \C-r" 'helm-recentf)

;;; If using Lusty, remove this to limit ido-mode usage
;;; strips path, only shows filename
(defun recentf-ido-find-file ()
  "Find a recent file using Ido,"
 (interactive)
  (let* ((file-assoc-list
     (mapcar (lambda (x)
       (cons (file-name-nondirectory x)
             x))
         recentf-list))
    (filename-list
     (remove-duplicates (mapcar #'car file-assoc-list)
                :test #'string=))
    (filename (ido-completing-read "Choose recent file: "
           filename-list
           nil
           t)))
    (when filename
      (find-file (cdr (assoc filename
                file-assoc-list))))))
;;; get rid of `find-file-read-only' and replace it with something more useful.
(global-set-key "\C-x\ \C-r" 'recentf-ido-find-file)

;;; http://emacs.stackexchange.com/questions/3063/recently-opened-files-in-ido-mode
;;; shows full path
;(defun ido-recentf-open ()
;  "Use `ido-completing-read' to find a recent file."
;  (interactive)
;  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;      (message "Opening file...")
;    (message "Aborting")))
;; bind to C-x C-r
;(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(defun ido-choose-from-recentf ()
  "Use ido to select a recently visited file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))

;; bind it to "C-c f"
(global-set-key (kbd "C-c f") 'ido-choose-from-recentf)

;-----------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- smex - a M-x enhancement for Emacs based on ido.
;;; https://github.com/nonsequitur/smex/

(require 'smex nil 'noerror)
;; (smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command

;-----------------------------------------------------------------

;;; Zippy the pinhead yow
;;; http://ergoemacs.org/emacs/emacs_zippy.html
;;; Now, in emacs, call yow, and a zippism will show. To insert (C-u M-x yow)
(defvar yow-file)
(setq yow-file "~/.emacs.d/elisp/yow_file_zippy_pinhead_quotes.txt.gz" )

;-----------------------------------------------------------------

;; Electric indent mode - added in 24.1 (on by default in 24.4
;(electric-indent-mode +1)


;-----------------------------------------------------------------
; system specific

;;; http://stackoverflow.com/questions/9248996/how-to-toggle-fullscreen-with-emacs-as-default
;(defun switch-fullscreen nil
;  (interactive)
;  (let* ((modes '(nil fullboth fullwidth fullheight))
;         (cm (cdr (assoc 'fullscreen (frame-parameters) ) ) )
;         (next (cadr (member cm modes) ) ) )
;    (modify-frame-parameters
;     (selected-frame)
;     (list (cons 'fullscreen next)))))
;(global-set-key (kbd "C-M-<return>") 'switch-fullscreen)

;;; Non-native fullscreen for OSX on emacs 24.4
;;; http://crypt.codemancers.com/posts/2013-07-05-non-native-fullscreen-for-osx-on-emacs-24-dot-3/
;(setq ns-use-native-fullscreen nil)
;(global-set-key (kbd "C-M-<return>") 'toggle-frame-fullscreen)

;;; Mac specific from https://github.com/rawsyntax/emacs.d/blob/master/Blondie.local.el
;(setq browse-url-browser-function 'browse-url-default-macosx-browser)
;;(setq browse-url-browser-function 'browse-url-default-windows-browser)
;(setq locate-make-command-line (lambda (s) `("mdfind" "-name" ,s)))

;---------------------------------------------------------------


;;; -----


;;;
; removed from init.el but may want again some day
;;;

;; Comments are too dark:
;; Molokai is: #465457 , Candidates:
;; #586568 #6B7679 #7E8789 #90989A
;(set-face-foreground 'font-lock-comment-face "#6B7679")
;(set-face-foreground 'font-lock-comment-delimiter-face "#6B7679")

;; don't let the cursor go into minibuffer prompt
;(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 
;;; http://makble.com/how-to-switching-buffer-effectively-in-emacs
;(defalias 'go 'switch-to-buffer)
;(defalias 'go 'helm-mini)
 
;; Increase / Decrease emacs font size with Meta-Scroll wheel
;; Can do via keyboard with C-x C-+ / C-x C--. Then successive C-+ / C--


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- cisco router mode
(autoload 'cisco-router-mode "cisco-router-mode" "edit cisco .cfg files" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- ediff
;;; 1. ignore space, 2. split side by side, 3. prevent popup):
; ??? - had custom-set-variables in it - ???


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- helm - an incremental completion and selection narrowing framework
;;; https://github.com/emacs-helm/helm
;;; http://tuhdo.github.io/helm-intro.html

(require 'helm)
(require 'helm-config)

;;; Allow TAB to run command instead of selecting action
;;(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;;(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;;(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;;(when (executable-find "curl")
;;  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t  ;open helm buffer inside current window
      helm-ff-file-name-history-use-recentf t)

;; SMEX replacement
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; activate helm mini (replaces switch-to-buffer)
(global-set-key (kbd "C-x b") 'helm-mini)

;; To enable fuzzy matching, add the following settings:
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; Override helm-command-find-files
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; jump to any man entry using Helm interface (C-x c m)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; Bind helm-occur
(global-set-key (kbd "C-c h o") 'helm-occur)

(helm-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- ldap-mode - colorization for editing ldifs, etc.
;(autoload 'ldap-mode "ldap-mode" nil t)
(require 'ldap-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- moccur - lists all matching lines of multiple buffers -- slow load
;;; color and edit moccur
;;; http://emacswiki.org/emacs/OccurMode
(require 'color-moccur)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- CPerl mode
;;;

;; Use cperl-mode instead of the default perl-mode
(defalias 'perl-mode 'cperl-mode)

(setq cperl-electric-keywords t) ;; expands for keywords such as foreach, etc
(setq cperl-hairy t) ;; Turns on most of the CPerlMode options

;;; use C-h f for perldoc in cperl mode
(add-hook 'cperl-mode-hook
          (lambda ()
            (local-set-key (kbd "C-h f") 'cperl-perldoc)))

;; http://stackoverflow.com/questions/12408031/emacs-perl-mode-send-script-buffer-to-perl-interpreter
(defun perl-on-buffer ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "perl" "*Perl Output*")
  (display-buffer "*Perl Output*"))

(eval-after-load 'cperl-mode
  '(define-key cperl-mode-map (kbd "<f5>") 'perl-on-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- powerline
;;; Emacs version of the Vim powerline status-bar
;;; https://github.com/milkypostman/powerline
;(require 'powerline)
;; Built-in Themes
;(powerline-default-theme)
;;(powerline-center-theme)
;;(powerline-vim-theme)
;;(powerline-nano-theme)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- sos - Search StackOverflow - https://github.com/omouse/emacs-sos
(autoload 'sos "sos" "Search Stack Overflow" t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- sql
;;; sql-mysql configuration
;;; Problem is mysql isn't on Mac's path

;(add-to-list 'exec-path "/Applications/XAMPP/xamppfiles/bin")
;(defalias 'sql-get-login 'ignore) ; don't prompt for login info
;;; -or-
;;(setq sql-mysql-program "/Applications/XAMPP/xamppfiles/bin/mysql")
;(setq sql-user "admin")
;;(setq sql-password "")
;(setq sql-server "localhost")
;;(setq sql-database "test")
;;(setq sql-mysql-options "optional command line options")

;;; hilight mysql keywords
;(add-hook 'sql-mode-hook 'sql-highlight-mysql-keywords)
(add-hook 'sql-mode-hook
          (lambda ()
            (sql-highlight-mysql-keywords)))


;;; hilight postgres keywords
;(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)

;; Make mysql not buffer sending stuff to the emacs-subprocess-pipes
;; -n unbuffered -B batch(tab separated) -f force(go on after error)
;; -i ignore spaces -q no caching -t table format
(setq-default sql-mysql-options (quote ("-n" "-B" "-f" "-i" "-q" "-t")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --sublime2 - Emacs additions to mimic sublime text 2 functionality
;(load "~/.emacs.d/sublime2")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- subversion  (call with M-x svn-status) -- slow load
;;; psvn
;(require 'psvn)
;(autoload 'svn-status "psvn.el" "SVN interface" t) ;; doesn't show indicator


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prefer eww for browing links
;(setq browse-url-browser-function 'eww-browse-url)

;;; Dockerfile mode
;; https://github.com/spotify/dockerfile-mode
;;You can specify the image name in the file itself by adding a line like this at the top of your Dockerfile.
;;  ## -*- docker-image-name: "your-image-name-here" -*-
;;If you don't, you'll be prompted for an image name each time you build.

;;(add-to-list 'load-path "/your/path/to/dockerfile-mode/")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))


;;; tramp dired sudo mode
;;; http://oremacs.com/2015/02/15/sudo-stuffs/
(defun sudired ()
  (interactive)
  (require 'tramp)
  (let ((dir (expand-file-name default-directory)))
    (if (string-match "^/sudo:" dir)
        (user-error "Already in sudo")
      (dired (concat "/sudo::" dir)))))
(define-key dired-mode-map "!" 'sudired)


;;; Do Ediff as I Mean
;;; http://kaushalmodi.github.io/2015/03/09/do-ediff-as-i-mean/
(defun modi/ediff-dwim ()
        "Do ediff as I mean.

If a region is active when this command is called, call `ediff-regions-wordwise'.

Else if the current frame has 2 windows,
- Do `ediff-files' if the buffers are associated to files and the buffers
  have not been modified.
- Do `ediff-buffers' otherwise.

Otherwise call `ediff-buffers' interactively."
        (interactive)
        (if (region-active-p)
            (call-interactively 'ediff-regions-wordwise)
          (if (= 2 (safe-length (window-list)))
              (let (bufa bufb filea fileb)
                (setq bufa  (get-buffer (buffer-name)))
                (setq filea (buffer-file-name bufa))
                (save-excursion
                  (other-window 1)
                  (setq bufb (get-buffer (buffer-name))))
                (setq fileb (buffer-file-name bufb))
                (if (or
                     ;; if either of the buffers is not associated to a file
                     (null filea) (null fileb)
                     ;; if either of the buffers is modified
                     (buffer-modified-p bufa) (buffer-modified-p bufb))
                    (progn
                      (message "Running (ediff-buffers \"%s\" \"%s\") .." bufa bufb)
                      (ediff-buffers bufa bufb))
                  (progn
                    (message "Running (ediff-files \"%s\" \"%s\") .." filea fileb)
                    (ediff-files filea fileb))))
            (call-interactively 'ediff-buffers))))


;;; -------------------
;;; --- twittering mode
;;; http://www.emacswiki.org/emacs/TwitteringMode
(require 'twittering-mode)

;; (Optional) To avoid having to authorize twittering-mode everytime you run it, add this to your `.emacs`:
;; This requires GnuPG. And also, either EasyPG (included as part of emacs v. 23) or alpaca.el (0.13) is necessary.
(setq twittering-use-master-password t)

;; (Optional) You may need to point twittering-mode to your system’s CA certificate bundle. On CentOS 6 I added this to my `.emacs`:
;(setq twittering-cert-file "/etc/ssl/certs/ca-bundle.crt")

;;; http://stackoverflow.com/questions/5710334/how-can-i-get-mouse-selection-to-work-in-emacs-and-iterm2-on-mac
;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- ido - Interactive do
;;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
;;; http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode

;|--------+------------------------------------------------------------------|
;| C-b    | Reverts to the old switch-buffer completion engine               |
;| C-f    | Reverts to the old find-file completion engine                   |
;| C-d    | Opens a dired buffer in the current directory                    |
;| C-a    | Toggles showing ignored files (see ido-ignore-files)             |
;| C-c    | Toggles if searching of buffer and file names should ignore case |
;| TAB    | Attempt to complete the input                                    |
;| C-p    | Toggles prefix matching                                          |
;| C-s/C-r| Moves to the next and previous match, respectively               |
;| C-t    | Toggles matching by Emacs regular expression.                    |
;| BKSP   | Deletes characters as usual or goes up one directory             |
;| C-SPC  | Restricts completion list to anything that matches your input    |
;| //     | ignore the preceding path, and go back to the top-most directory |
;| ~/     | Jumps to the home directory.                                     |
;| M-d    | Searches for the input in all sub-directories of current         |
;| C-k    | Kills the currently focused buffer or deletes the file           |
;| M-m    | Creates a new sub-directory to the directory you're in           |
;|--------+------------------------------------------------------------------|
; require packages: flx-ido, ido-ubiquitous, ido-vertical-mode

;;; Enable ido mode to work with C-x C-f (find files)
;;; C-x C-f C-f will revert to old style completion engine
;;; or try C-x C-f C-e to enter path EDIT mode.  Useful for tramp.

;;; Enable flx mode (flx.el and flx-ido.el) - https://github.com/lewang/flx
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1) ; ido-ubiquitous, disable to use lusty-explorer
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(setq gc-cons-threshold 20000000)

(add-to-list 'ido-ignore-files "\\.DS_Store")

;; If Recentf is enabled, you can use [C-x b] (or M-x ido-switch-buffer) to
;; visit recently closed files by enabling virtual buffers
(setq ido-use-virtual-buffers t)

;; Create new buffer if nothing found (always, prompt, never)
(setq ido-create-new-buffer 'always)
(setq confirm-nonexistent-file-or-buffer nil)

;;; Disable the merging (the "looking in other directories" in ido vulgo)
;;; But can also undo the merge with C-z in ido
(setq ido-auto-merge-work-directories-length -1)  ;; breaks searching in dirs

;;; when using ido, the confirmation is rather annoying...
;;; but, when this is active ido does a double confirmation
;;; ignoring the first y and requiring a second y, disabling!
;;;
;;; testing it again 3/2/14
(setq confirm-nonexistent-file-or-buffer nil)

(setq ido-file-extensions-order '(".org" ".txt" ".py" ".pp" ".md"
                                  ".xml" ".el" ".ini" ".cfg" ".cnf"))

;; Disable ido's directory cache, acts up over flaky network links
(setq ido-save-directory-list-file nil)

(setq
  ;ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
  ido-save-directory-list-file nil
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
    "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
  ;ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
  ido-case-fold  t                 ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ;ido-enable-last-directory-history nil ;forget latest selected dir name
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)
  ;ido-enable-flex-matching nil     ; don't try to be too smart (fuzzy matching)
  ido-enable-flex-matching t       ; enable fuzzy matching
  ido-max-prospects 8              ; don't spam my minibuffer
  ido-confirm-unique-completion t  ; wait for RET, even with unique completion
)


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
;;; -- recentf - a minor mode that builds a list of recently opened files
;;; http://www.masteringemacs.org/articles/2011/01/27/find-files-faster-recent-files-package/
(autoload 'recentf "recentf" "List recent files" t)
(setq
  recentf-save-file "~/.emacs.d/cache/recentf"
  recentf-max-saved-items 200      ;; max save file cache
  recentf-max-menu-items 15)      ;; max 15 in menu
(recentf-mode t)                  ;; turn it on
;(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;(global-set-key "\C-x\ \C-r" 'helm-recentf)

;;; If using Lusty, remove this to limit ido-mode usage
;;; strips path, only shows filename
(defun recentf-ido-find-file ()
  "Find a recent file using Ido,"
 (interactive)
  (let* ((file-assoc-list
     (mapcar (lambda (x)
       (cons (file-name-nondirectory x)
             x))
         recentf-list))
    (filename-list
     (remove-duplicates (mapcar #'car file-assoc-list)
                :test #'string=))
    (filename (ido-completing-read "Choose recent file: "
           filename-list
           nil
           t)))
    (when filename
      (find-file (cdr (assoc filename
                file-assoc-list))))))
;;; get rid of `find-file-read-only' and replace it with something more useful.
(global-set-key "\C-x\ \C-r" 'recentf-ido-find-file)

;;; http://emacs.stackexchange.com/questions/3063/recently-opened-files-in-ido-mode
;;; shows full path
;(defun ido-recentf-open ()
;  "Use `ido-completing-read' to find a recent file."
;  (interactive)
;  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;      (message "Opening file...")
;    (message "Aborting")))
;; bind to C-x C-r
;(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(defun ido-choose-from-recentf ()
  "Use ido to select a recently visited file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))

;; bind it to "C-c f"
(global-set-key (kbd "C-c f") 'ido-choose-from-recentf)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- Uniquify
;;; If you use ido-mode, you may also want to check out uniquify, which allows
;;; for buffer names that are easier to distinguish in the ido-mode echo area
;;; Fixed buffer unique names, stop adding <1>, <2> to buffers
;;; http://stackoverflow.com/questions/5607794/emacs-switching-between-buffers-with-the-same-name-but-in-different-directories
;(require 'uniquify)
(autoload 'uniquify "uniquify" "unique buffers" t)
(setq uniquify-buffer-name-style 'post-forward)
;(setq uniquify-buffer-name-style 'forward)
(setq uniquify-strip-common-suffix t)


;;; for org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda

;; Agenda window setup
;; Possible values for this option are:
;;  current-window  -Show agenda in the current window, keeping other windows.
;;  other-frame     -Use `switch-to-buffer-other-frame' to display agenda.
;;  other-window    -Use `switch-to-buffer-other-window' to display agenda.
;;  reorganize-frame-Show only 2 windows, the current window and the agenda.
;; See also the variable `org-agenda-restore-windows-after-quit'."
;(setq org-agenda-window-setup 'other-frame) ;; open agenda in new frame
(setq org-agenda-window-setup 'current-window) ;; don't kill my window setup

(setq calendar-week-start-day 1)
(setq case-fold-search t)
(setq current-language-environment "Latin-1")
(setq default-input-method "latin-1-prefix")
(setq make-backup-files nil)
(setq normal-erase-is-backspace t)
(setq org-agenda-ndays 7)
(setq org-agenda-repeating-timestamp-show-all nil)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-show-all-dates t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-sorting-strategy (quote ((agenda time-up priority-down tag-up) (todo tag-up))))
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-todo-ignore-deadlines t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-with-date t)
(setq org-agenda-window-setup (quote other-window))
(setq org-deadline-warning-days 7)
(setq org-export-html-style "<link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\">")
(setq org-fast-tag-selection-single-key nil)
(setq org-log-done (quote (done)))
(setq org-refile-targets (quote (("gtd.org" :maxlevel . 1) ("someday.org" :level . 2))))
(setq org-reverse-note-order nil)
(setq org-tags-column -78)
(setq org-tags-match-list-sublevels nil)
(setq org-time-stamp-rounding-minutes (quote (1 15)))
(setq org-use-fast-todo-selection t)
(setq org-use-tag-inheritance nil)
(setq org-log-done nil)
(setq org-agenda-include-diary nil)
(setq org-timeline-show-empty-dates t)
(setq org-insert-mode-line-in-empty-file t)

;; 2006-05-26  - added following line
;(require 'org-install)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other
(setq org-agenda-exporter-settings
      '((ps-number-of-columns 1)
        (ps-landscape-mode t)
        (htmlize-output-type 'css)))

(defun gtd ()
    (interactive)
    (find-file "~/org/gtd/gtd.org")
)
(global-set-key (kbd "C-c g") 'gtd)


(add-hook 'org-agenda-mode-hook 'hl-line-mode)

; org mode start - added 20 Feb 2006
;; The following lines are always needed. Choose your own keys.

;(global-font-lock-mode t)

;(global-set-key "\C-x\C-r" 'prefix-region)
(global-set-key "\C-x\C-l" 'goto-line)
(global-set-key "\C-x\C-y" 'copy-region-as-kill)

