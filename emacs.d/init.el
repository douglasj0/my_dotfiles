; -*- mode: emacs-lisp -*-

;; Keep track of loading time
(defconst emacs-start-time (current-time))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize) ; required to load proper org-plus-contrib version

(require 'org)
(org-babel-load-file
  (expand-file-name "emacs-init.org"
                     user-emacs-directory))

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                           emacs-start-time))))
(message "Loading settings...done (%.3fs)" elapsed))
