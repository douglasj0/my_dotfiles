;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- org-mode - for keeping notes, maintaining TODO lists, doing project
;;; planning, and authoring with a fast and effective plain-text system
;;; Moved to init-org-mode.el for ease of customization
;;; NOTES:
;;; To include org files in other org files:
;;; #+setupfile: /path/to/config.org
;;; Info:
;;;   Call up the info with C-h i.
;;;   Then call g (Info-goto-node).
;;;   Enter (org) at the prompt.

;(message "*** Loaded org-mode-init.el")
(require 'org)
(require 'org-contacts) ;takes 0.59s To load
(require 'ox-confluence)

;; Add markdown and odt export
;;http://stackoverflow.com/questions/22988092/emacs-org-mode-export-markdown
;; odt errors in messages, no styles?
(eval-after-load "org"
  '(require 'ox-md nil t))
;(eval-after-load "org"
;  '(require 'ox-odt nil t))

;; Bind C-h o to org-info
;(with-eval-after-load "org"
 (define-key global-map (kbd "C-h o") 'org-info)
;)

; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

; add opml export - didn't work, test it
; https://github.com/edavis/org-opml
;(load-library "ox-opml")

;; Set to the location of your Org files on your local system
(setq org-directory "~/org")

;; Open all txt files in org-mode
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))

;; Use ido for prompt completions
;(setq org-completion-use-ido t)

;; Including all org files from a directory into the agenda.
;;   Or manage by hand, visit file and type:  C-c [ to add, C-c ] to remove.
;(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))


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

;; Include emacs diary, not needed if using org-anniversary
;(setq org-agenda-include-diary t)

;; Custom agenda commands
;; http://members.optusnet.com.au/~charles57/GTD/mydotemacs.txt
(setq org-agenda-custom-commands
'(
("P" "Projects"
              ((tags "PROJECT")))

("H" "Office and Home Lists"
     ((agenda)
          (tags-todo "OFFICE")
          (tags-todo "HOME")
          (tags-todo "COMPUTER")
          (tags-todo "DVD")
          (tags-todo "READING")))

("D" "Daily Action List"
     ((agenda "" ((org-agenda-ndays 1)
                     (org-agenda-sorting-strategy
                        (quote ((agenda time-up priority-down tag-up))))
                     (org-deadline-warning-days 0)
                     ))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Capture

;; Setup default target for notes and a global hotkey for new ones
;; NOTE:  Need org-mode version 6.3.6 or later for this to work
;; http://stackoverflow.com/questions/3622603/org-mode-setup-problem-when-trying-to-use-capture
(setq org-default-notes-file (expand-file-name "~/org/notes.org"))

;; Capture templates - C-c c t
;(setq org-capture-templates
; '(("t" "Todo" entry (file+headline (concat org-directory "/gtd.org") "Tasks")
;        "* TODO %?\n %i\n %a")
;   ("n" "Note" entry (file+datetree (concat org-directory "/notes.org") "Notes")
;        "* %?\nEntered on %U\n  %i\n  %a")
;   ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org") "Journel")
;        "* %?\nEntered on %U\n  %i\n  %a")
;   ("l" "Link" plain (file (concat org-directory "/links.org"))
;         "- %?\n %x\n"))) ; save contents of clipboard to links.org (URLs)

;; Based on Sacha Chua's org-capture-tempaltes
;; http://pages.sachachua.com/.emacs.d/Sacha.html
(defvar dbj/org-basic-task-template "* TODO %^{Task}
SCHEDULED: %^t
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>
%?
" "Basic task data")

(defvar dbj/org-basic-someday-template "* %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>
%?

%i
" "Basic task data")

(setq org-capture-templates
      `(("t" "Tasks" entry 
         (file+headline "~/org/gtd/newgtd.org" "Tasks")
         ,dbj/org-basic-task-template)
        ("s" "Someday task" entry
         (file+headline "~/org/gtd/someday.org" "Someday")
         ,dbj/org-basic-someday-template)
        ("j" "Journal entry" plain
         (file+datetree+prompt "~/org/gtd/journal.org")
         "%i\n%?\n")
        ("c" "Contact, email" entry (file "~/org/gtd/contacts.org")
          "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")
        ("C" "Contact, full" entry (file "~/org/gtd/contacts.org")
          "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:PHONE:
:ALIAS:
:NICKNAME:
:IGNORE:
:ICON:
:NOTE:
:ADDRESS:
:BIRTHDAY:
:END:")
        ("n" "Notes" entry
          (file+datetree "~/org/gtd/notes.org")
          "* %?\n\n%i\n")
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other

;; When adding new heading below the current heading, the new heading is
;; placed after the body instead of before it.  C-<RET>
(setq org-insert-heading-respect-content t)

;; Set Todo keywords, same as:
;; #+TODO: TODO(t) STARTED(s) WAITING(w) | DONE(d) CANCELED(c)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w@/!)")
        (sequence "|" "DONE(d!)" "CANCELED(c@)")))

;; Set Tags, same as:
;; #+TAGS: home(h) work(w) @computer(c) @phone(p) errants(e)
(setq org-tag-alist '(("@office" . ?o) ("@home" . ?h) ("computer" . ?c)
                      ("phone" . ?p) ("reading" . ?r)))

;; Prevent C-k from killing whole subtrees and losing work
(setq org-special-ctrl-k t)

;; Fontify code buffers in org, instead of grey text
;; This is especially nice when you open an editing buffer with [Ctrl+c ']
;; to insert code into the #+begin_src ... #+end_src area.
(setq org-src-fontify-natively t)

;; org-refile (C-c C-w) settings from:
;; http://www.mail-archive.com/emacs-orgmode@gnu.org/msg34415.html
;(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-outline-path-complete-in-steps t)
;(setq org-completion-use-ido nil)
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '((org-agenda-files :maxlevel . 2)
                           (nil :maxlevel . 3)))

;; MobileOrg config
;; M-x org-mobile-push - copy org files to ~/Dropbox/MobileOrg
;; M-x org-mobile-pull - integrate remove changes into local org files
;;
;; Set to the location of your Org files on your local system
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Home/MobileOrg")

;;; Display a list of closed todo's from this month and last month
;;; http://jcardente.blogspot.com/2010/06/org-mode-hack-tasks-done-last-month.html
;(defun jtc-org-tasks-closed-in-month (&optional month year match-string)
;  "Produces an org agenda tags view list of the tasks completed
;in the specified month and year. Month parameter expects a number
;from 1 to 12. Year parameter expects a four digit number. Defaults
;to the current month when arguments are not provided. Additional search
;criteria can be provided via the optional match-string argument "
;  (interactive)
;  (let* ((today (calendar-current-date))
;         (for-month (or month (calendar-extract-month today)))
;         (for-year  (or year  (calendar-extract-year today))))
;    (org-tags-view nil
;          (concat
;           match-string
;           (format "-CATEGORY={Home}")
;           (format "+CLOSED>=\"[%d-%02d-01]\""
;                   for-year for-month)
;           (format "+CLOSED<=\"[%d-%02d-%02d]\""
;                   for-year for-month
;                   (calendar-last-day-of-month for-month for-year))))))
;
;(defun jtc-org-tasks-last-month ()
;  "Produces an org agenda tags view list of all the tasks completed
;last month with the Category Foo."
;  (interactive)
;  (let* ((today (calendar-current-date))
;         (for-month (calendar-extract-month today))
;         (for-year  (calendar-extract-year today)))
;       (calendar-increment-month for-month for-year -1)
;       (jtc-org-tasks-closed-in-month
;        ;for-month for-year "CATEGORY=\"Foo\"+TODO=\"DONE\"")))
;        for-month for-year "-CATEGORY={Home}+TODO=\"DONE\"")))

;;; Bullet point report
;;; http://www.ooblick.com/weblog/2010/06/03/monthly-reports-with-org-mode/
;;; TODO - Home/home is showing up in the bullet list
; Make it easier to generate bullets for $BOSS
;(defvar bullet-entry-types
;  '(:closed)
;  "Org-mode agenda types that we want to see in the monthly bullet report
;See `org-agenda-entry-types'."
;  )
;
;(defun bullets ()
;  "Show a list of achievements for the past month, for monthly reports.
;Uses `org-agenda'.
;"
;  (interactive)
;  (require 'org-agenda)
;  ; All we're doing here, really, is calling `org-agenda' with
;  ; arguments giving a start date and a number of days. But to do
;  ; that, we need to figure out
;  ; - the date of the first of last month
;  ; - the number of days in last month
;  (let* ((now (current-time))
;	 ; Figure out when last month was. Assuming that I run this
;	 ; close to the beginning of a month, then `now' minus two
;	 ; weeks was some time in the previous month. We can use that
;	 ; to extract the year and month that we're interested in.
;	 (2weeks-ago
;	  (time-subtract now
;			 (days-to-time 14)))
;	 ; We'll also need to know when the first of this month was,
;	 ; to find out how long last month was. If today is the 12th
;	 ; of the month, then the first of the month was `now' minus
;	 ; 11 days.
;	 (1st-of-this-month
;	  (time-subtract now
;			 (days-to-time
;			  (- (nth 3 (decode-time now))
;			     1))))
;	 ; Ditto to find the first of last month.
;	 (1st-of-last-month
;	  (time-subtract 2weeks-ago
;			 (days-to-time
;			  (- (nth 3 (decode-time 2weeks-ago))
;			     1))))
;	 ; The length of last month is the difference (in days)
;	 ; between the first of last month, and the first of this
;	 ; month.
;	 (len-last-month
;	  (time-to-number-of-days
;	   (time-subtract 1st-of-this-month
;			  1st-of-last-month)))
;	 (start-date (decode-time 1st-of-last-month))
;	 (start-year (nth 5 start-date))	; Year number
;	 (start-mon (nth 4 start-date))		; Month number
;	 ; Restrict the agenda to only those types of entries we're
;	 ; interested in. I think this takes advantage of dynamic
;	 ; scoping, which is normally an abomination unto the lord,
;	 ; but is useful here.
;	 (org-agenda-entry-types bullet-entry-types)
;	 )
;    ; Create an agenda with the stuff we've prepared above
;    (org-agenda-list nil
;		     (format "%04d-%02d-01"
;			     start-year
;			     start-mon)
;		     len-last-month)
;    ))


;--- New ---
;;; http://whattheemacsd.com//setup-org.el-01.html
;;; I mainly use org-mode for a collection of TODO-lists.
;;; So I get a little annoyed when the [17/23] cookies at the parent level
;;; aren't updated when I remove an item.  This code fixes that.
;(defun myorg-update-parent-cookie ()
;  (when (equal major-mode 'org-mode)
;    (save-excursion
;      (ignore-errors
;        (org-back-to-heading)
;        (org-update-parent-todo-statistics)))))
;
;(defadvice org-kill-line (after fix-cookies activate)
;  (myorg-update-parent-cookie))
;
;(defadvice kill-whole-line (after fix-cookies activate)
;  (myorg-update-parent-cookie))

;;; Strike-through finished todos
;;; sachachua.com/blog/2012/12/emacs-strike-through-headlines-for-done-tasks-in-org/
(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"
                 :weight normal
                 :strike-through t))))
 '(org-headline-done
            ((((class color) (min-colors 16) (background dark))
               (:foreground "LightSalmon" :strike-through t)))))

;;; Make sure to hightlight mysql sql keywords:
;;; ex. #+BEGIN_SRC sql
;;;        SELECT foo FROM bar
;;;     #+END_SRC
;(add-hook 'sql-mode-hook
;          (lambda ()
;            (sql-highlight-mysql-keywords)))

;;; Enable other org-babel langauges
(org-babel-do-load-languages
  (quote org-babel-load-languages)
  (quote ((emacs-lisp . t)
          (asymptote . t) ;Asymptote
          (awk . t)       ;Awk
          (C . t)         ;C
          ;(C++ . t)       ;C++
          (clojure . t)   ;Clojure
          (css . t)       ;CSS
          ;(d . f)        ;D
          (ditaa . f)     ;ditaa
          (dot . t)       ;Graphviz
          (calc . t)      ; Emacs Calc
          ;(fortran . t)   ;Fortran
          (gnuplot . t)   ;gnuplot
          ;(haskell . t)   ;Haskell
          (java . t)      ;Java
          (js . t)        ;Javascript
          (latex . t)     ;LaTeX
          (ledger . f)    ;Ledger
          (lisp . t)      ;Lisp
          ;(lilypond . t)  ;Lilypond
          ;(lua . t)       ;Lua
          ;(matlab . t)    ;MATLAB
          ;(mscgen . t)    ;Mscgen
          ;(ocaml . t)     ;Objective Caml
          ;(octave . t)    ;octave
          (org . t)       ;Org mode
          ;(oz . f)        ;Oz
          (perl . t)      ;Perl
          ;(plantuml . t)  ;Plantuml
          ;(processing . t) ;Processing.js
          (python . t)    ;Python
          (R . t)         ;R
          (ruby . t)      ;Ruby
          ;(sass . t)      ;Sass
          ;(scheme . t)    ;Scheme
          (screen . t)    ;GNU Screen
          (sed . t)       ;Sed
          (shell . t)     ;shell
          (sql . t)       ; SQL
          (sqlite .t)     ;SQLite
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load org-journal
;(autoload 'org-journal "org-journal" "Autoload journal" t) ; handled by elpa
;(setq org-journal-dir "~/org/journal/")
