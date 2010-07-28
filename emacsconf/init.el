;; -*- mode: emacs-lisp -*-

;;; To distinguish running environment
(defconst mswin  (eq window-system 'w32)  "Non-nil means windows system.")
(defconst xwin   (eq window-system 'x)    "Non-nil means X window")
(defconst macos  (eq window-system 'mac)  "Non-nil means mac os x system")
(defconst nowin  (eq window-system nil)   "Non-nil means no window manager")
(defconst cygwin (eq system-type 'cygwin) "Non-nil means cygwin system.")
;;; To distinguish different emacs version
(defconst is-before-emacs-21 (>= 21 emacs-major-version) "emacs version before 21")
(defconst is-after-emacs-23  (<= 23 emacs-major-version) "emacs version after 23")

;;; global prefix setting
(defconst my-emacs-dir "~/.emacs.d/"
  "Emacs configuration path, default is `~/.emacs.d/'")
(defconst my-config-dir (concat my-emacs-dir "config/")
  "Emacs customization settings")
(defconst my-lisp-dir (concat my-emacs-dir "my-lisp/")
  "Some lisp snippets and functions")
(defconst my-site-lisp-dir (concat my-emacs-dir "site-lisp/")
  "Useful lisp projects and products")
(defconst my-template-dir (concat my-emacs-dir "templates/")
  "Default templates for creating different files")
(defconst my-snippet-dir (concat my-emacs-dir "snippets/"))
(defconst my-org-dir
  (if mswin "d:/My Dropbox/Notes/" "~/Dropbox/Notes/")
  "Put my temporary notes here, eg: in dropbox directory")
(defconst my-temp-dir
  (if mswin "c:/windows/temp/emacs/" "~/.tmp-emacs/")
  "Temporary directory to store autosave, desktop, session, backup files.")

(defconst system-head-file-dir (list "/usr/include/" "/usr/local/include/" "/usr/include/sys/") "System header file directories")
(defconst user-head-file-dir   (list "." "../hdr/" "../include/") "User header file directories")

;;; add all directories into load-path
(load (concat my-lisp-dir "my-subdirs")) ; use this function
(my-add-subdirs-to-load-path my-config-dir)
(my-add-subdirs-to-load-path my-lisp-dir)
(my-add-subdirs-to-load-path my-site-lisp-dir)

;; Personal Info.
(setq user-full-name "Julian Qian")

(require 'util)

;;{{{ Font setting in different platform
(when mswin
  ;; (create-fontset-from-fontset-spec
  ;;  "-outline-Consolas-normal-r-normal-normal-16-*-96-96-c-*-fontset-chinese")
  ;; (set-default-font "fontset-chinese")
  (load "fontset-win")
  (huangq-fontset-consolas 16)

  (defconst my-cygwin-dir "d:/cygwin/")
  (load "cygwin-cfg")

  ;; file name encoding
  (setq file-name-coding-system 'chinese-gbk)
  (setq default-file-name-coding-system 'chinese-gbk)

  ;; tramp setting
  (setq tramp-default-method "plink")
  (setq tramp-auto-save-directory my-temp-dir)
  (setq auto-save-file-name-transforms
        '(("\\`/[^/]*:\\(.+/\\)*\\(.*\\)"
           (concat my-temp-dir "\\2"))))
  (setq ange-ftp-ftp-program-name "w32-ftp.exe"))

(when macos
  (set-default-font "-*-andale mono-medium-r-normal--14-*-*-*-m-*-*-*")
  ;; Maximize when emacs starts up
  (require 'maxframe)
  (add-hook 'window-setup-hook 'maximize-frame t)
  ;;  (global-set-key [f13] (lambda () (interactive) (switch-to-buffer (other-buffer))))
  )

(when xwin
  ;;else, linux or freebsd etc.
  (set-frame-font "Consolas-12")        ;; set-default-font has been obsoleted
  (set-fontset-font (frame-parameter nil 'font)
                    'han '("Microsoft YaHei" . "unicode-bmp")))

(if nowin
    ;;; fix emacs comment color bug.
    (custom-set-faces
     '(font-lock-comment-face ((((class color)) (:foreground "red"))))))
;;}}}

;;; transparent frame
(set-frame-parameter (selected-frame) 'alpha '(95 85))
(add-to-list 'default-frame-alist '(alpha 95 85))

(server-start)

(load "my-face")

;;; some useful tools
(load "my-sudo")
(load "my-grep")
(load "my-isearch")
(load "my-tools")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; usual settings
(load "keybind-cfg")
(load "mode-line-cfg")
(load "config-cfg")
(load "assist-cfg")

;; modes
(load "ccmode-cfg")
;; (load "ilisp-cfg")
(load "python-cfg")
(load "php-cfg")
(load "webdev-cfg")
(load "org-cfg")
;; (load "muse-cfg")
(load "shell-cfg")

;; development
(load "svn-cfg")
(load "gud-cfg")
(load "hideshow-cfg")

;; editor
(load "edit-cfg")
(load "recentf-cfg")
(load "recent-jump-cfg")
(load "highlight-cfg")
(load "ibuffer-cfg")
(load "dired-cfg")
(load "speedbar-cfg")
(load "skeleton-cfg")

;; assistance
(load "ido-cfg")
(load "gtags-cfg")
(load "auto-complete-cfg")
;; (load "company-cfg")
(load "cedet-cfg")
(load "yasnippet-cfg")
(load "desktop-cfg")
(load "common-cfg")

;; (load "gnus-cfg")
;; (load "mew-cfg")

(load "theme-cfg")

(let ((nxhtml-init-file (concat my-site-lisp-dir "nxhtml/autostart.el")))
  (if (file-exists-p nxhtml-init-file)
      (load-file nxhtml-init-file)))

(require 'google-maps)

;;; imenu-tree
(require 'imenu-tree)
;;; (autoload 'imenu-tree "imenu-tree" "Imenu tree" t)
;;; (autoload 'tags-tree "tags-tree" "TAGS tree" t)
;;; (eval-after-load "tree-widget"
;;;   '(if (boundp 'tree-widget-themes-load-path)
;;;        (add-to-list 'tree-widget-themes-load-path "~/.emacs.d/icons/")))
;;; (add-hook 'tree-mode-hook
;;;           (lambda ()
;;;             (toggle-truncate-lines t)))

;;; CSharp
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs\\'" . csharp-mode)) auto-mode-alist))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-comment-face (quote gray-face) t)
 '(font-lock-string-face (quote light-blue-face) t)
 '(safe-local-variable-values (quote ((code . utf-8) (c-hanging-comment-ender-p))))
 '(semantic-self-insert-show-completion-function (lambda nil (semantic-ia-complete-symbol-menu (point)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(compilation-info ((((type tty)) :bold t :foreground "green") (t :foreground "green")))
 '(diredp-date-time ((((type tty)) :foreground "yellow") (t :foreground "goldenrod1")))
 '(diredp-dir-heading ((((type tty)) :background "yellow" :foreground "blue") (t :background "Pink" :foreground "DarkOrchid1")))
 '(diredp-display-msg ((((type tty)) :foreground "blue") (t :foreground "cornflower blue")))
 '(match ((((class color) (min-colors 88) (background light)) :background "yellow1") (((class color) (min-colors 88) (background dark)) :background "RoyalBlue3" :foreground "cyan") (((class color) (min-colors 8) (background light)) :background "yellow" :foreground "black") (((class color) (min-colors 8) (background dark)) :background "blue" :foreground "white") (((type tty) (class mono)) :inverse-video t) (t :background "gray")))
 '(semantic-highlight-func-current-tag-face ((((type tty)) nil) (((class color) (background dark)) (:background "gray20")) (((class color) (background light)) (:background "gray90"))))
 '(sh-heredoc ((((min-colors 88) (class color) (background dark)) (:foreground "deeppink")) (((class color) (background dark)) (:foreground "deeppink")) (((class color) (background light)) (:foreground "tan1")) (t (:weight bold))))
 '(svn-status-filename-face ((((type tty)) :bold t :foreground "yellow") (t :foreground "yellow"))))
