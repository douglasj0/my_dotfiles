 ;;; Only use Themes in GUI. Set color theme (load-theme) for >= Emacs 24
 (when (display-graphic-p)
   (if (fboundp 'load-theme)
    (progn
      (push "~/.emacs.d/themes/" custom-theme-load-path)
      (load-theme 'zenburn))))
