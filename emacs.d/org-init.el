;; -- org-mode - for keeping notes, maintaining TODO lists, doing project
;; planning, and authoring with a fast and effective plain-text system
;; org-mode-contrib - all org files plus all contribs files
;;
;; Info:
;;   Call up the info with C-h i.
;;   Then call g (Info-goto-node).
;;   Enter (org) at the prompt.

;;(message "*** Loaded org-mode-init.el")
(require 'org)
(require 'org-contacts) ;takes 0.59s To load

;; Load additional exporters
;(eval-after-load 'org
;      (lambda()
;        (require 'ox-md)
;        (require 'ox-odt)
;        (require 'ox-opml)
;        (require 'ox-confluence)))

;; Bind C-h o to org-info
(define-key global-map (kbd "C-h o") 'org-info)

; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

;; Set to the location of your Org files on your local system
(setq org-directory "~/org")

;; Open all txt files in org-mode
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))


;;; Agenda
;; Agenda window setup
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


;;; Capture
;; Setup default target for notes and a global hotkey for new ones
;; NOTE:  Need org-mode version 6.3.6 or later for this to work
;; http://stackoverflow.com/questions/3622603/org-mode-setup-problem-when-trying-to-use-capture
(setq org-default-notes-file (expand-file-name "~/org/notes.org"))

;; Capture templates - C-c c t
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

;;; Strike-through finished todos
;; sachachua.com/blog/2012/12/emacs-strike-through-headlines-for-done-tasks-in-org/
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
(add-hook 'sql-mode-hook
          (lambda ()
            (sql-highlight-mysql-keywords)))

;;; Enable other org-babel langauges
(org-babel-do-load-languages
  (quote org-babel-load-languages)
  (quote ((emacs-lisp . t)
          ;(asymptote . t) ;Asymptote
          (awk . t)       ;Awk
          (C . t)         ;C
          ;(C++ . t)       ;C++
          ;(clojure . t)   ;Clojure
          ;(css . t)       ;CSS
          ;(d . f)        ;D
          ;(ditaa . f)     ;ditaa
          ;(dot . t)       ;Graphviz
          ;(calc . t)      ; Emacs Calc
          ;(fortran . t)   ;Fortran
          ;(gnuplot . t)   ;gnuplot
          ;(haskell . t)   ;Haskell
          (java . t)      ;Java
          (js . t)        ;Javascript
          ;(latex . t)     ;LaTeX
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
          ;(R . t)         ;R
          (ruby . t)      ;Ruby
          ;(sass . t)      ;Sass
          ;(scheme . t)    ;Scheme
          (screen . t)    ;GNU Screen
          (sed . t)       ;Sed
          (shell . t)     ;shell
          (sql . t)       ; SQL
          ;(sqlite .t)     ;SQLite
)))
