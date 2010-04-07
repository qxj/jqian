;; -*- mode: emacs-lisp -*-

;;; To distinguish running environment
(defconst mswin  (eq window-system 'w32)  "Non-nil means windows system.")
(defconst xwin   (eq window-system 'X)    "Non-nil means X window")
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
  (if mswin "c:/windows/temp/emacs/" "/var/tmp/emacs/")
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
  (set-default-font "Consolas-16")
  (set-fontset-font (frame-parameter nil 'font)
                    'han '("Vera Sans YuanTi" . "unicode-bmp")))

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
(load "keybind-cfg")
(load "mode-line-cfg")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Settings ;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)
;; windmov
(windmove-default-keybindings)
(setq windmove-window-distance-delta 2)

;; echo key strokes quickly
(setq echo-keystrokes 0.1)

(setq default-directory "~/")

(setenv "CVS_RSH" "ssh")

;; Turn off the status bar if we're not in a window system
(menu-bar-mode (if window-system 1 -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Display Time
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;; replace yes/no <RET> to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; TimeStamp
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-active t)
(setq time-stamp-warn-inactive t)
(setq time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S %:a by %u")

;; Highlight selected regions in Gnu Emacs
(setq transient-mark-mode t)

;; Wrap too long lines
(toggle-truncate-lines t)
(setq hscroll-margin 1)
(setq hscroll-step 1)
(put 'scroll-left 'disabled nil)

;; Abbrev mode setting
(setq-default abbrev-mode t)
;; (add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))
(setq abbrev-file-name (concat my-temp-dir ".abbrev_defs"))
(if (file-exists-p abbrev-file-name)
    (read-abbrev-file abbrev-file-name))
(setq save-abbrevs t)

;; hippie-expand
(autoload 'senator-try-expand-semantic "senator")

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; whether use file dialog
(setq use-file-dialog nil)

;; autosave bookmark into the diskete
(setq bookmark-save-flag 1)
(setq bookmark-default-file (concat my-temp-dir "emacs.bookmark"))

;; minor mode
(setq resize-minibuffer-mode nil)
;; (icomplete-mode t)

;; ### some notes about search M-p, M-n, backward or forward
;; the search words typed ever.  the keybinding for pasting
;; the text onto the search string is M-y and not C-y
;; Hitting Enter will then be your only way to exit the
;; search
;; (setq search-exit-option t)
;; Search case match
;; (setq-default case-fold-search nil)
;; Highlight Search
(setq search-highlight t)
(setq query-replace-highlight t)
;; Word search : C-s RET C-w, C-r RET C-w Regular Search :
;; M-C-s (isearch-forward-regexp) and M-C-r
;; (isearch-backward-regexp)
(setq search-whitespace-regexp "[ \t\r\n]+")
;; Recurively Editing : C-r, C-M-c, C-]
;; Occour Search: C-2 M-x occur RET <h3> RET.

;; whether add a new line at the bottom
(setq next-line-add-newlines nil)

;; auto fill : M-q
(setq adaptive-fill-mode t)
(setq default-fill-column 80)

(setq visible-bell t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq column-number-mode t)
(setq mouse-yank-at-point t)

;; indent without tab '\t' but white space
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120 124 128 132 136 140 144 148 152 156 160))
;; chinese charactor at the end of sentences
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
;; (setq sentence-end "\\([(setq make-backup-files t)
(setq sentence-end-double-space nil)

;; underscore donot divide word in text-mode
(modify-syntax-entry ?\_ "w" text-mode-syntax-table)

(setq enable-recursive-minibuffers t)

;; scroll-margin, which conflict with pager-mode, thus set to zero
(setq scroll-margin 0
      scroll-conservatively 500)

;; default major mode
(setq default-major-mode 'text-mode)

;; Auto fill and outline mode
(add-hook 'text-mode-hook
          (function (lambda ()
                      ;;                       (turn-on-auto-fill)
                      (outline-minor-mode t))))


;; show matching parentheses
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(mouse-avoidance-mode 'animate)
(auto-image-file-mode t)

;; syntax highlight
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)
       (setq scalable-fonts-allowed t)))
(setq font-lock-maximum-size
      (quote ((t . 1280000) (c-mode . 256000) (c++-mode . 256000))))

;; Turn on the features disabled default
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)

;; Store all backup files into a separated directories
(setq make-backup-files t)
(setq version-control t)
(setq kept-new-versions 3)
(setq kept-old-versions 2)
(setq delete-old-versions t)
(setq dired-kept-versions 1)
(add-to-list 'backup-directory-alist
             (cons "." (concat my-temp-dir "backup/")))
(setq backup-by-copying t)

(setq auto-save-list-file-prefix
      (concat my-temp-dir "emacs-autosave-"))

;; Make terminal colorful
(setq ansi-color-for-comint-mode t)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; use clipboard
(setq x-select-enable-clipboard t)

;; date issue, appt-add & appt-delete
(setq appt-issue-message t)
;; diary
;; (add-hook 'diary-mode-hook 'turn-on-auto-fill)
;; (add-hook 'diary-mode-hook 'appt-make-list)

;; hide inputed password when using telnet/shell/w3m etc.
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extensions ;;;;;;;;;;;;;;;;

;;{{{ recent finded buffers
(recentf-mode t)
(setq recentf-max-saved-items nil)
(setq recentf-save-file (concat my-temp-dir "emacs.recentf"))
(defun recentf-open-files-compl ()
  (interactive)
  (let* ((all-files recentf-list)
         (tocpl (mapcar (function
                         (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
         (prompt (append '("File name: ") tocpl))
         (fname (completing-read (car prompt) (cdr prompt) nil nil)))
    (find-file (cdr (assoc-ignore-representation fname tocpl)))))
(global-set-key (kbd "C-x C-o") 'recentf-open-files-compl)
;; Also store recent opened directories besides files
(defun recentf-add-dir ()
  "Add directory name to recentf file list."
  (recentf-add-file dired-directory))
(add-hook 'dired-mode-hook 'recentf-add-dir)
;;}}}


;;(require 'newcomment)

;;{{{ unicad to distinguish charset
(require 'unicad)
;;}}}

;;{{{ more highlight colors
(require 'generic-x)
;;}}}

;;{{{ make cursor become a line
(require 'bar-cursor)
;;}}}

;;{{{ display SPC and TAB charactors
(require 'blank-mode)
;;{{{

;;{{{ A visual table editor, very cool
(autoload 'table-insert "table" "WYGIWYS table editor")
;;}}}

;;{{{ ebrowse
(require 'ebrowse)
(add-to-list 'auto-mode-alist '("BROWSE\\.*" . ebrowse-tree-mode))
;;}}}

;;{{{ session
(require 'session)
(setq session-save-file (concat my-temp-dir "emacs.session"))
(add-hook 'after-init-hook 'session-initialize)
(setq session-save-file-coding-system 'utf-8-unix)
;;}}}

;;; {{{ template
(require 'template)
(template-initialize)

(setq template-default-directories (list my-template-dir))
(add-to-list 'template-find-file-commands 'ido-exit-minibuffer)

;; (defvar last-template nil "Recently used template file")
;; (defun my-template-expand-template (template)
;;   "Expand template file"
;;   (interactive
;;    (list
;;     (read-file-name
;;      (if last-template (format "Please specify templete file(defautly %s): " last-template) "Please specify template file: ")
;;      (concat my-emacs-path "templates") last-template t)))
;;   (template-expand-template template)
;;   (setq last-template template))
;; (dolist (map (list emacs-lisp-mode-map
;;                    c-mode-base-map
;;                    makefile-mode-map
;;                    makefile-automake-mode-map
;;                    sh-mode-map
;;                    text-mode-map))
;;   (define-key map (kbd "C-c T") 'my-template-expand-template)
;;   (define-key map (kbd "C-c C-t") 'template-expand-template))
;;; }}}

;;{{{ fold mode
;;; (setq fold-mode-prefix-key "\C-c\C-o")
;;; (setq fold-autoclose-other-folds nil)
;;; (require 'fold nil t)
;;; (when (featurep 'fold)
;;;   (add-hook 'find-file-hook 'fold-find-file-hook t))
;;}}}

;;{{{ ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;}}}

;;{{{ winner mode: restore the last windows arrangement
(winner-mode 1)
;;}}}

;;{{{ uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
;;}}}

;;{{{ kill buffers every 24 hr
(require 'midnight)
;;}}}

;;{{{ show function name where cursor stay
(require 'which-func)
(which-func-mode 1)
(setq which-func-unknown "unknown")
;;}}}

;;{{{ help to switch between source and header files
(require 'sourcepair)
;; (define-key c-mode-map (kbd "C-c s") 'sourcepair-load)
;; (define-key c++-mode-map (kbd "C-c s") 'sourcepair-load)
(setq sourcepair-source-path '( "." "../src"))
(setq sourcepair-header-path user-head-file-dir)
(setq sourcepair-recurse-ignore '("CVS" "bin" "lib" "Obj" "Debug" "Release" ".svn"))
;;}}}

;;{{{ find file at point
(require 'ffap)
(setq ffap-c-path (append ffap-c-path system-head-file-dir user-head-file-dir))
;;}}}

;;{{{ find out all header files that .cpp includes
(require 'c-includes)
(setq c-includes-binding t)
(setq c-includes-path ffap-c-path)
;;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization ;;;;;;;;;;;;;;;;

;;{{{ ####  hooks
;; auto kill shell buffer, when exit
(defun wcy-shell-mode-hook-func  ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'wcy-shell-mode-kill-buffer-on-exit))
(defun wcy-shell-mode-kill-buffer-on-exit (process state)
  (kill-buffer (current-buffer)))

;; auto rename shell name, to open more shell window
(defun wcy-shell-mode-auto-rename-buffer (text)
  (if (eq major-mode 'shell-mode)
      (rename-buffer  (concat "shell: " default-directory) t)))
;; add-hook
(add-hook 'comint-output-filter-functions 'wcy-shell-mode-auto-rename-buffer)
(add-hook 'shell-mode-hook 'wcy-shell-mode-hook-func)
;; (setq-default shell-cd-regexp nil)
;; (setq-default shell-pushd-regexp nil)
;; (setq-default shell-popd-regexp nil)

;;{{{ custom Info-mode-map
(add-hook 'Info-mode-hook
          (lambda ()
            (define-key Info-mode-map '[down] 'pager-row-down)
            (define-key Info-mode-map (kbd "M-n") 'pager-row-down)
            (define-key Info-mode-map '[up] 'pager-row-up)))
;;}}}


;;{{{ custom Man-mode-map
(add-hook 'Man-mode-hook
          (lambda ()
            (define-key Man-mode-map '[down] 'pager-row-down)
            (define-key Man-mode-map (kbd "M-n") 'pager-row-down)
            (define-key Man-mode-map (kbd "M-p") 'pager-row-up)
            (define-key Man-mode-map '[up] 'pager-row-up)))
;;}}}

;;{{{ etags setting
;; (setq tags-file-name "~/projects/TAGS")

(defun generate-tag-table ()
  "Generate tag tables under current directory(Linux)."
  (interactive)
  (let ((exp "") (dir ""))
    (setq dir (read-from-minibuffer "generate tags in: " default-directory)
          exp (read-from-minibuffer "suffix: "))
    (with-temp-buffer
      (shell-command
       (concat "find " dir " -name \"" exp "\" | xargs etags ")
       (buffer-name)))))
;;}}}

(set-default 'auto-mode-alist
             (append '(("\\.\\(php\\|inc\\)\\'" . php-mode)
                       ("\\(Makefile\\|\\.mak\\)" . makefile-mode)
                       ("\\.p[lm]\\'" . cperl-mode)
                       ("\\.\\(cl\\|li?sp\\)\\'" . lisp-mode)
                       ("\\.css\\'" . css-mode)
                       ("\\.html\\'" . html-mode)
                       ("\\.js\\'" . js2-mode)
                       ("\\.\\(jpg\\|png\\|gif\\)\\'" . image-mode))
                     auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some useful tools
(load "my-sudo")
(load "my-grep")
(load "my-isearch")
(load "my-tools")

;;; usual settings
(load "common-cfg")

;; modes
(load "ccmode-cfg")
;; (load "ilisp-cfg")
(load "python-cfg")
(load "php-cfg")
(load "webdev-cfg")
(load "org-cfg")
;; (load "muse-cfg")

;; development
(load "svn-cfg")
(load "gud-cfg")
(load "hideshow-cfg")

;; editor
(load "edit-cfg")
(load "recent-jump-cfg")
(load "highlight-cfg")
(load "ibuffer-cfg")
(load "dired-cfg")
(load "speedbar-cfg")
(load "skeleton-cfg")

;; assistance
(load "auto-complete-cfg")
;; (load "company-cfg")
(load "cedet-cfg")
(load "yasnippet-cfg")
(load "ido-cfg")
(load "desktop-cfg")

;; (load "gnus-cfg")
;; (load "mew-cfg")

(load "theme-cfg")

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
 '(midnight-mode t nil (midnight))
 '(safe-local-variable-values (quote ((code . utf-8) (c-hanging-comment-ender-p))))
 '(semantic-self-insert-show-completion-function (lambda nil (semantic-ia-complete-symbol-menu (point)))))
