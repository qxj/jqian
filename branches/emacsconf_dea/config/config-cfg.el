;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; config-cfg.el ---
;; Time-stamp: <2010-07-15 12:29:06 Thursday by jqian>
;; Created: 2010 Julian Qian
;; Version: $Id: config-cfg.el,v 0.0 2010/04/21 11:44:48 jqian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))

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

;; Make typing overwrite text selection
(setq delete-selection-mode t)

(setq truncate-partial-width-windows nil)

;; Wrap too long lines
(toggle-truncate-lines nil)
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
;; auto complete file name ignoring case
(setq read-file-name-completion-ignore-case t)
(setq completion-ignore-case t)

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
(setq default-justification 'full)
(setq adaptive-fill-mode nil)
(setq default-fill-column 78)

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
                      ;; (turn-on-auto-fill)
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
(put 'scroll-left 'disabled nil)
(put 'erase-buffer 'disabled nil)
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

;; use clipboard
(setq x-select-enable-clipboard t)

;; date issue, appt-add & appt-delete
(setq appt-issue-message t)
;; diary
;; (add-hook 'diary-mode-hook 'turn-on-auto-fill)
;; (add-hook 'diary-mode-hook 'appt-make-list)

;; hide inputed password when using telnet/shell/w3m etc.
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

(setq diff-switches "-ubB")
(setq outline-minor-mode-prefix "")

(setq view-read-only t)

(setq woman-cache-filename "~/.wmncache.el")

(provide 'config-cfg)

;;; config-cfg.el ends here
