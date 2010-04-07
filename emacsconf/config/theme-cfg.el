;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; theme-cfg.el ---
;; Time-stamp: <2010-03-12 17:40:45 Friday by jqian>
;; Created: 2010 Julian Qian
;; Version: $Id: theme-cfg.el,v 0.0 2010/01/28 10:39:26 julian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))

;; (require 'my-face)
;; (require 'color-theme-ahei)

;;{{{ color-theme
;; (if window-system
;;     (progn
;;       (require 'color-theme)
;;       (color-theme-initialize)
;;       ;; set default color theme
;;       ;;(color-theme-blue-mood)
;;       ;;       (color-theme-subtle-hacker)
;;       (if macos
;;    ()
;;  (require 'color-theme-tango)
;;  (color-theme-tango))))
;;}}}

(setq use-black-background nil)

(if use-black-background
    (progn
      (set-background-color "black")
      (set-foreground-color "white")
      (set-cursor-color "green")
      (custom-set-variables
       '(font-lock-comment-face 'gray-face)
       '(font-lock-string-face 'magenta-face)
       '(font-lock-function-name-face 'yellow-blue-face)
       '(font-lock-constant-face 'while-blue-face)
       '(font-lock-variable-name-face 'dark-yellow-face)
       '(font-lock-keyword-face 'dark-cyan-face)
       '(font-lock-comment-delimiter-face 'dark-red-face)
       '(font-lock-warning-face 'red-face)
       '(font-lock-doc-face 'green-red-face)
       '(font-lock-type-face 'green-face)
       '(font-lock-regexp-grouping-backslash 'red-yellow-face)
       '(font-lock-regexp-grouping-construct 'yellow-red-face)))
      ;; else
  (custom-set-variables
   '(font-lock-comment-face 'gray-face)
   '(font-lock-string-face 'blue-face)))

(when (facep 'semantic-tag-highlight-face)
  (set-face-foreground 'semantic-tag-highlight-face "red")
  (set-face-background 'semantic-tag-highlight-face "blue"))

(setq browse-kill-ring-separator-face 'font-lock-comment-delimiter-face)

(set-face-foreground 'highlight "red")
(set-face-background 'highlight "blue")

(copy-face 'region-face 'region-invert)
(invert-face 'region-invert)

(setq apropos-match-face 'red-face)
(setq apropos-symbol-face 'magenta-face)
(setq apropos-keybinding-face 'cyan-face)
(setq apropos-label-face 'underline-green-face)
(setq apropos-property-face 'font-yellow-face)

;; compile
(custom-set-faces '(compilation-info
                    ((((type tty)) :bold t :foreground "green")
                     (t :foreground "green"))))
(setq compilation-message-face nil)
(setq compilation-warning-face 'red-face)
(setq compilation-enter-directory-face 'beautiful-blue-face)
(setq compilation-leave-directory-face 'magenta-face)

;; svn
(custom-set-faces '(svn-status-filename-face
                    ((((type tty)) :bold t :foreground "yellow")
                     (t :foreground "yellow"))))

;; dired+
(require 'dired+)
(custom-set-faces '(diredp-display-msg
                    ((((type tty)) :foreground "blue")
                     (t :foreground "cornflower blue"))))
(custom-set-faces '(diredp-date-time
                    ((((type tty)) :foreground "yellow")
                     (t :foreground "goldenrod1"))))
(custom-set-faces '(diredp-dir-heading
                    ((((type tty)) :background "yellow" :foreground "blue")
                     (t :background "Pink" :foreground "DarkOrchid1"))))
(setq diredp-ignored-file-name 'green-face)
(setq diredp-file-name 'darkred-face)
(setq diredp-file-suffix 'magenta-face)
(setq diredp-exec-priv 'darkred-face)
(setq diredp-other-priv 'white-face)
(setq diredp-no-priv 'darkmagenta-face)
(setq diredp-write-priv 'darkcyan-face)
(setq diredp-read-priv 'darkyellow-face)
(setq diredp-link-priv 'lightblue-face)
(setq diredp-symlink 'darkcyan-face)
(setq diredp-rare-priv 'white-red-face)
(setq diredp-dir-priv 'beautiful-blue-face)
(setq diredp-compressed-file-suffix 'darkyellow-face)

(custom-set-faces '(match
                    ((((class color) (min-colors 88) (background light))
                      :background "yellow1")
                     (((class color) (min-colors 88) (background dark))
                      :background "RoyalBlue3" :foreground "cyan")
                     (((class color) (min-colors 8) (background light))
                      :background "yellow" :foreground "black")
                     (((class color) (min-colors 8) (background dark))
                      :background "blue" :foreground "white")
                     (((type tty) (class mono))
                      :inverse-video t)
                     (t :background "gray"))))

(custom-set-faces
 '(sh-heredoc
   ((((min-colors 88) (class color)
      (background dark))
     (:foreground "deeppink"))
    (((class color)
      (background dark))
     (:foreground "deeppink"))
    (((class color)
      (background light))
     (:foreground "tan1" ))
    (t
     (:weight bold)))))

(custom-set-faces
 '(semantic-highlight-func-current-tag-face
   ((((type tty)) nil)
    (((class color) (background dark))
     (:background "gray20"))
    (((class color) (background light))
     (:background "gray90")))))

(defface hl-line-nonunderline-face
  '((((type tty)))
    (t :background "AntiqueWhite4" :inverse-video nil))
  "`hl-line-face' without `underline'.")

(provide 'theme-cfg)

;;; theme-cfg.el ends here
