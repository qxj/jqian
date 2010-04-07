;; -*- mode: emacs-lisp -*-

;;; load-path
(add-to-list 'load-path "~/.emacs.d/load-path")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;;; global prefix setting
(defvar my-temp-dir
  (if (eq system-type 'windows-nt) "c:/windows/temp" "/var/tmp/emacs")
  "Temporary directory to store autosave, desktop, session, backup files.")
(setq config-dir "~/.emacs.d/config/")

;;; {{{ font setting in different platform
(if (eq window-system 'w32)
	(progn
;;;       (create-fontset-from-fontset-spec
;;;        "-outline-Consolas-normal-r-normal-normal-16-*-96-96-c-*-fontset-chinese")
;;;       (set-default-font "fontset-chinese")
	  (load "fontset-win")
	  (huangq-fontset-consolas 16)

	  (load-file (concat config-dir "2-cygwin.el"))

	  ;; file name encoding
	  (setq file-name-coding-system 'chinese-gbk)
	  (setq default-file-name-coding-system 'chinese-gbk)
	  
	  ;; tramp setting
	  (setq tramp-default-method "plink")
	  (setq tramp-auto-save-directory my-temp-dir)
      (setq auto-save-file-name-transforms
            '(("\\`/[^/]*:\\(.+/\\)*\\(.*\\)" "c:/windows/temp/\\2")))
      (setq ange-ftp-ftp-program-name "w32-ftp.exe")))

(if (eq window-system 'mac)
	(progn
	  (set-default-font "-*-andale mono-medium-r-normal--14-*-*-*-m-*-*-*")
	  ;; Maximize when emacs starts up
	  (require 'maxframe)
	  (add-hook 'window-setup-hook 'maximize-frame t)
	  ;;  (global-set-key [f13] (lambda () (interactive) (switch-to-buffer (other-buffer))))
	  ))

(if (eq window-system 'X)
	(progn
	  ;;else, linux or freebsd etc.
	  (set-default-font "Consolas-16")
	  (set-fontset-font (frame-parameter nil 'font)
						'han '("Vera Sans YuanTi" . "unicode-bmp"))))

(if (eq window-system nil)
	;;; fix emacs comment color bug.
	(custom-set-faces
	 '(font-lock-comment-face ((((class color)) (:foreground "red"))))))
;;; }}}

;;; transparent frame
(set-frame-parameter (selected-frame) 'alpha '(95 85))
(add-to-list 'default-frame-alist '(alpha 95 85))

(server-start)

;;{{{ keybind
;;; region action
(global-set-key [f4] 'indent-region)
(global-set-key [(shift f4)] 'fill-region)
;;; like windows action in Win32 system
(global-set-key [(meta f4)] 'delete-frame)
;;; reload from the disk
(global-set-key [f5] 'revert-buffer)
;;; winner
(global-set-key [(shift f9)] 'winner-undo)
(global-set-key [(shift f10)] 'winner-redo)
;;; go to previous or next buffer
(global-set-key [(f11)] 'bury-buffer)
(global-set-key [(shift f11)] 
                (lambda()
                  (interactive)
                  (switch-to-buffer (car (reverse (buffer-list))))))
(global-set-key [f12] 
				(lambda ()
				  (interactive)
				  (switch-to-buffer (other-buffer))))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-n") 'pager-row-down)
(global-set-key (kbd "M-p") 'pager-row-up)

(global-set-key [(delete)] 'delete-char)
(global-set-key (kbd "C-c j") 'ffap)
(global-set-key (kbd "C-c i") 'imenu)

(global-set-key [(meta g)] 'goto-line)
(global-set-key [(meta ?/)] 'hippie-expand)

;; shell
;;; (define-key shell-mode-map '[up] 'comint-previous-input)
;;; (define-key shell-mode-map '[down] 'comint-next-input)
;; default
;;; (global-set-key [(control f)] 'forward-char)
;; when isearch-mode, donot delete charactors in the buffer.
(define-key isearch-mode-map '[backspace] 'isearch-delete-char)

;;; some useful keybinding
(defun display-buffer-name ()
  (interactive)
  (message (buffer-file-name (current-buffer))))
(defun my-kill-other-buffers (&optional list)
  "Kill other buffers except the current one."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
		   (name (buffer-name buffer)))
      (if (not (string-equal name (buffer-name (current-buffer))))
		  (and name	; Can be nil for an indirect buffer, if we killed the base buffer.
			   (not (string-equal name ""))
			   (/= (aref name 0) ?\s)
			   (kill-buffer buffer))))
    (setq list (cdr list))))
(defun my-kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
;; begin setting
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'my-kill-other-buffers)
(global-set-key (kbd "M-3") 'delete-other-frames)
(global-set-key (kbd "M-4") 'my-kill-current-buffer)
(global-set-key (kbd "M-5") 'display-buffer-name)
(global-set-key (kbd "M-6") 'other-window)
(global-set-key (kbd "C-x f") 'find-file-at-point)
(global-set-key [(meta left)] 'backward-sexp)
(global-set-key [(meta right)] 'forward-sexp)

;; C-x C-j open the directory of current buffer
(global-set-key (kbd "C-x C-j")
                (lambda ()
                  (interactive)
                  (if (buffer-file-name)
                      (dired default-directory))))

(global-set-key "\C-x\C-k"
                (lambda()
                  (interactive)
                  (kill-buffer (current-buffer))))

;; Support by zslevin (LinuxForum GNU Emacs/XEmacs)
;; C-. find tag at point in another window -- "C-x 4 ."
;; C-, close tag view window
;; M-. find tag at point and jump
;; M-, jump back
;; C-M-, prompt tag to find and jump
(defun lev/find-tag (&optional show-only)
  "Show tag in other window with no prompt in minibuf."
  (interactive)
  (let ((default (funcall (or find-tag-default-function
                              (get major-mode 'find-tag-default-function)
                              'find-tag-default))))
    (if show-only
        (progn (find-tag-other-window default)
               (shrink-window (- (window-height) 12)) ;; limit 12 lines
               (recenter 1)
               (other-window 1))
      (find-tag default))))


(global-set-key [(control .)] '(lambda () (interactive) (lev/find-tag t)))
(global-set-key [(control ,)] 'delete-other-windows)
;; (global-set-key [(meta .)] 'lev/find-tag)
;;; (global-set-key [(meta ,)] 'pop-tag-mark)
(global-set-key [(meta ,)] 'gtags-pop-stack)
;; (global-set-key (kbd "C-M-,") 'find-tag)
(global-set-key [(meta .)] 'gtags-find-tag-from-here)
(global-set-key (kbd "C-M-,") 'gtags-find-rtag)

(global-set-key (kbd "C-x C-2") 'clone-indirect-buffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c s") '(lambda() (interactive) (switch-to-buffer "*scratch*")))

;; imenu -> ido-completing-read
(defun ido-imenu-completion (index-alist &optional prompt)
  ;; Create a list for this buffer only when needed.
  (let ((name (thing-at-point 'symbol))
        choice
        (prepared-index-alist
         (if (not imenu-space-replacement) index-alist
           (mapcar
            (lambda (item)
              (cons (subst-char-in-string ?\s (aref imenu-space-replacement 0)
                                          (car item))
                    (cdr item)))
            index-alist))))
    (when (stringp name)
      (setq name (or (imenu-find-default name prepared-index-alist) name)))
    (setq name (ido-completing-read "Index item: "
                                    (mapcar 'car prepared-index-alist)
                                    nil t nil 'imenu--history-list
                                    (and name (imenu--in-alist
                                               name prepared-index-alist) name)))
    (when (stringp name)
      (setq choice (assoc name prepared-index-alist))
      (if (imenu--subalist-p choice)
          (imenu--completion-buffer (cdr choice) prompt)
        choice))))
(defalias 'imenu--completion-buffer 'ido-imenu-completion)


;; C-d, donot put it into kill-ring
(defun my-delete-char-or-region ()
  (interactive)
  (if (and mark-active transient-mark-mode)
      (delete-region (region-beginning) (region-end))
    (call-interactively 'delete-char)))
(global-set-key (kbd "C-d") 'my-delete-char-or-region)


(setq highlight-symbol-idle-delay 1.5) ; hack it
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)

(require 'recent-jump)
(setq recent-jump-threshold 4)
(setq recent-jump-ring-length 10)
(global-set-key (kbd "M-9") 'recent-jump-jump-backward)
(global-set-key (kbd "M-0") 'recent-jump-jump-forward)

(require 'pager)
(global-set-key "\C-v"	   'pager-page-down)
(global-set-key [next] 	   'pager-page-down)
(global-set-key "\M-v"	   'pager-page-up)
(global-set-key [prior]	   'pager-page-up)
(global-set-key '[up]    'pager-row-up)
(global-set-key '[M-kp-8]  'pager-row-up)
(global-set-key '[down]  'pager-row-down)
(global-set-key '[M-kp-2]  'pager-row-down)
;;; smooth to scroll when the cursor near edge
;;; (defadvice next-line (before wcy-next-line (arg))
;;;   (let ((pos
;;;          (save-excursion
;;;            (forward-line arg)
;;;            (point)))
;;;         (end-pos (window-end)))
;;;     (if (not (pos-visible-in-window-p pos))
;;;         (let ((linenum (1+ (count-lines end-pos pos))))
;;;           (scroll-up linenum)))))
;;; (ad-activate 'next-line)
;;; (defadvice previous-line (before wcy-previous-line (arg))
;;;   (let ((pos
;;;          (save-excursion
;;;            (forward-line (* -1 arg))
;;;            (point))))
;;;     (if (not (pos-visible-in-window-p pos))
;;;         (let ((linenum (1+ (count-lines pos (window-start)))))
;;;           (scroll-down linenum)))))
;;; (ad-activate 'previous-line)
;;}}} end keybind

;;{{{ recent finded buffers
(recentf-mode t)
(setq recentf-max-saved-items nil)
(setq recentf-save-file (concat my-temp-dir "/emacs.recentf"))
(defun recentf-open-files-compl ()
  (interactive)
  (let* ((all-files recentf-list)
         (tocpl (mapcar (function
                         (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
         (prompt (append '("File name: ") tocpl))
         (fname (completing-read (car prompt) (cdr prompt) nil nil)))
    (find-file (cdr (assoc-ignore-representation fname tocpl)))))
(global-set-key (kbd "C-x C-o") 'recentf-open-files-compl)
;;}}}

;;{{{ browser kill ring
(require 'browse-kill-ring)
(global-set-key [(control c)(k)] 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
;;}}}

;;(require 'newcomment)

;;{{{ color-theme
(if (not (eq window-system nil))
	(progn
	  (require 'color-theme)
	  (color-theme-initialize)
	  ;; set default color theme
	  ;;(color-theme-blue-mood)
;; 	  (color-theme-subtle-hacker)
	  (if (eq window-system 'mac)
		  ()
		(require 'color-theme-tango)
		(color-theme-tango)
        )
	  ))
;;}}}

;;{{{ unicad
(require 'unicad)
;;}}}

;;{{{ a table editor
(autoload 'table-insert "table" "WYGIWYS table editor")
;;}}}

;;{{{ ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (setq ibuffer-filter-groups
                  '(
                    ("*buffer*" (name . "\\*.*\\*"))
                    ("TAGS" (name . "^TAGS\\(<[0-9]+>\\)?$"))
                    ("dired" (mode . dired-mode))))))

(defface ibuffer-sql-face '((t (:foreground "Goldenrod"))) "Ibuffer sql face")
(defface ibuffer-text-face '((t (:foreground "SkyBlue"))) "Ibuffer text face")
(defface ibuffer-readonly-face '((t (:foreground "Blue" :weight bold))) "Ibuffer read only face")
(defface ibuffer-lisp-face '((t (:foreground "gold"))) "Ibuffer lisp face")

(setq
 ibuffer-fontification-alist
 '(	;; Sql-mode buffers
   (5  (string-match ".sql$" (buffer-name)) ibuffer-sql-face)
   (10 (eq major-mode 'text-mode) ibuffer-text-face)
   (15 (eq major-mode 'emacs-lisp-mode) ibuffer-lisp-face)
   (20 (string-match "^*" (buffer-name)) ibuffer-readonly-face)))

(setq ibuffer-formats '((mark modified read-only " " (name 21 21) " "
							  (size 6 -1 :right) " " (mode 25 25 :center)
							  ;;                         " " (process 8 -1) 
							  " " filename)
						(mark " " (name 16 -1) " " filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&")
;;}}}

;;{{{ session
(require 'session)
(setq session-save-file (concat my-temp-dir "/emacs.session"))
(add-hook 'after-init-hook 'session-initialize)
(setq session-save-file-coding-system 'utf-8-unix)
;;}}}

;;; {{{ template
(require 'template)
(setq template-default-directories (quote ("~/.emacs.d/templates")))

(template-initialize)
(add-to-list 'template-find-file-commands 'ido-exit-minibuffer)
;;; }}}

;;{{{ fold mode
;;; (setq fold-mode-prefix-key "\C-c\C-o")
;;; (setq fold-autoclose-other-folds nil)
;;; (require 'fold nil t)
;;; (when (featurep 'fold)
;;;   (add-hook 'find-file-hook 'fold-find-file-hook t))
;;}}}

;;{{{ Setting for ido: fast switch buffers
(require 'ido)
(ido-mode t)
;; use normal find-file function for ftp files
(setq ido-slow-ftp-host-regexps '(".*"))
;; don't search for other directorys
(setq ido-work-directory-list-ignore-regexps '(".*"))
(setq ido-save-directory-list-file (concat my-temp-dir "/emacs.ido-last"))
;;}}}

;;{{{ ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;}}}

;;{{{ command redo
(require 'redo)
(global-set-key [(control ?\')] 'redo)
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

;;{{{ C-a, C-e rebind
(defun my-end-of-line ()
  (interactive)
  (if (eq (point) (line-end-position))
      (skip-chars-backward " \t")
    (move-end-of-line 1)))

(defun my-beginning-of-line ()
  (interactive)
  (if (eq (point) (line-beginning-position))
      (skip-chars-forward " \t")
    (beginning-of-line)))

;; bind it!
(global-set-key "\C-a" 'my-beginning-of-line)
(global-set-key "\C-e" 'my-end-of-line)
;;}}}

;;{{{ count Chinese, English words of a region
(defun my-count-ce-word (beg end)
  "Count Chinese and English words in marked region."
  (interactive "r")
  (let* ((cn-word 0)
         (en-word 0)
         (total-word 0)
         (total-byte 0))
	;; string-to-int
    (setq cn-word (count-matches "\\cc" beg end)
          en-word (count-matches "\\w+\\W" beg end)
          total-word (+ cn-word en-word)
          total-byte (+ cn-word (abs (- beg end))))
    (message (format "Total: %d (cn: %d, en: %d) words, %d bytes."
                     total-word cn-word en-word total-byte))))
;;}}}

;;{{{ insert current time-stamp
(defun my-insert-date ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))

										;(global-set-key (kbd "C-c C-d d") 'my-insert-date)
;;}}}

;;; Settings
(prefer-coding-system 'utf-8)
;; windmov
(windmove-default-keybindings)
(setq windmove-window-distance-delta 2)

;; (icomplete-mode t)

(setenv "CVS_RSH" "ssh")

(setq isearch-allow-scroll t)

(setq frame-title-format "%b/%n%F")
(setq-default icon-title-format "Emacs - %b")

;; Turn off the status bar if we're not in a window system
;; (menu-bar-mode (if window-system 1 -1))
(menu-bar-mode nil)
(if window-system (setq tool-bar-mode nil))


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
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
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
(setq bookmark-default-file "~/.emacs.d/.bookmark")

;; minor mode
(setq resize-minibuffer-mode nil)
(setq icomplete-mode t)

;; ### some notes about search M-p, M-n, backward or forward
;; the search words typed ever.  the keybinding for pasting
;; the text onto the search string is M-y and not C-y
;; Hitting Enter will then be your only way to exit the
;; search
                                        ; (setq search-exit-option t)
;; Search case match
                                        ; (setq-default case-fold-search nil)
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
(setq default-fill-column 72)

(setq visible-bell t)
(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq mouse-yank-at-point t)

;; kill a whole line
(setq-default kill-whole-line t)
(setq kill-ring-max 50)

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
(setq auto-image-file-mode t)

;; syntax highlight
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it	   
	   (global-font-lock-mode t)
       ;; Maximum colors	   
	   (setq font-lock-maximum-decoration t)))
(setq font-lock-maximum-size
	  (quote ((t . 1280000) (c-mode . 256000) (c++-mode . 256000))))


;; Turn on the features disabled default
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)


(setq make-backup-files t)
(setq version-control t)
(setq kept-new-versions 3)
(setq kept-old-versions 2)
(setq delete-old-versions t)
(setq dired-kept-versions 1)
(add-to-list 'backup-directory-alist
			 (cons "." (concat my-temp-dir "/backup")))
(setq backup-by-copying t)

(setq auto-save-list-file-prefix
	  (concat my-temp-dir "/emacs-autosave-"))

;; terminal
(setq ansi-color-for-comint-mode t)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Personal Info.
(setq user-full-name "Julian Qian")

;; Dired related
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

;; use clipboard
(setq x-select-enable-clipboard t)

;; date issue, appt-add & appt-delete
(setq appt-issue-message t)
;; diary
;; (add-hook 'diary-mode-hook 'turn-on-auto-fill)
;; (add-hook 'diary-mode-hook 'appt-make-list)


;;{{{ ####  hooks
;;; shell window related
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

;;{{{ debug
(setq gdb-many-windows t)
(setq gdb-use-inferior-io-buffer t) 

(add-hook 'gdb-mode-hook
          (lambda () 
            (gud-def my-watch "watch %e" 
                     "\C-w" "Watch my variables.")))
;;}}}

;;{{{  speedbar within frame
;; Speedbar
(setq speedbar-update-speed 3)
(setq speedbar-use-images t)
(require 'speedbar)
(defvar my-speedbar-buffer-name 
  (if (buffer-live-p speedbar-buffer)
      (buffer-name speedbar-buffer)
    "*SpeedBar*"))
(defun my-speedbar-no-separate-frame ()
  (interactive)
  (when (not (buffer-live-p speedbar-buffer))
    (setq speedbar-buffer (get-buffer-create my-speedbar-buffer-name)
          speedbar-frame (selected-frame)
          dframe-attached-frame (selected-frame)
          speedbar-select-frame-method 'attached
          speedbar-verbosity-level 0
          speedbar-last-selected-file nil)
    (set-buffer speedbar-buffer)
    (speedbar-mode)
    (speedbar-reconfigure-keymaps)
    (speedbar-update-contents)
    (speedbar-set-timer 1)
    (make-local-hook 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook
              (lambda () (when (eq (current-buffer) speedbar-buffer)
                           (setq speedbar-frame nil
                                 dframe-attached-frame nil
                                 speedbar-buffer nil)
                           (speedbar-set-timer nil)))))
  (set-window-buffer (selected-window)
                     (get-buffer my-speedbar-buffer-name)))
;;}}}


;;{{{ my-comment-or-uncomment-region: by Julian
(defun my-comment-or-uncomment-region (&optional line)
  "This function is to comment or uncomment a line or a region"
  (interactive "P")
  (unless (or line (and mark-active (not (equal (mark) (point)))))
    (setq line 1))
  (if line
      (save-excursion
        (comment-or-uncomment-region
         (progn
           (beginning-of-line)
           (point))
         (progn
           (end-of-line)
           (point))))
    (call-interactively 'comment-or-uncomment-region)))
;; bind it
(global-set-key [(control ?\\)] 'my-comment-or-uncomment-region)
;;}}}

;;{{{ similar to dd & yy in VIM
(defadvice kill-ring-save (before slickcopy activate compile)
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
                   (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line inste
ad."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
                   (line-beginning-position 2)))))
;;}}}

;;{{{ select a word
(defun my-isearch-word ()
  (interactive)
  (when (not mark-active)
    (let (word-beg word-end)
      (unless (looking-at "\\<")
        (if (eq (char-syntax (char-after)) ?w)
            (backward-word)
          (and (forward-word) (backward-word))))
      (setq word-beg (point))
      (forward-word)
      (setq word-end (point))
      (setq my-isearch-word (filter-buffer-substring word-beg word-end nil t))
      (backward-word))
    (when (> (length my-isearch-word) 0)
      (kill-new my-isearch-word)
      (setq my-isearch-word (concat "\\<" my-isearch-word "\\>"))
      (isearch-update-ring my-isearch-word t)
      (add-hook 'isearch-mode-end-hook 'my-isearch-word-end-hook)
      (isearch-mode t t)
      (isearch-repeat 'forward)
      (message "%s" isearch-string))))
(global-set-key (kbd "C-M-1") 'my-isearch-word)
;;}}}

;;{{{
(defun my-move-line-up (p)
  "move current line up"
  (interactive "p")
  (let ((c (current-column)))
    (beginning-of-line)
    (kill-line 1)
    (previous-line p)
    (beginning-of-line)
    (yank)
    (previous-line 1)
    (move-to-column c)))

(defun my-move-line-down (p)
  "move current line down"
  (interactive "p")
  (let ((c (current-column)))
    (beginning-of-line)
    (kill-line 1)
    (next-line p)
    (beginning-of-line)
    (yank)
    (previous-line 1)
    (move-to-column c)))

(defun my-dup-line-down () 
  "duplicate this line at next line"
  (interactive)
  (let ((c (current-column)))
    (beginning-of-line)
    (ue-select-line-down)
    (beginning-of-line)
    (yank)
    (previous-line 1)
    (move-to-column c)))

(defvar ue-selection-last-point nil
  "Indicates whether to kill-append or kill-new")
(defvar ue-selection-total-lines 0
  "Total number of lines appended so far to the last item in kill-ring")

(defun ue-selection-update-cont ()
  "Update `ue-selection-last-point' variable"
  (if (and this-command
           (string= (format "%s" this-command)
                    "ue-select-line-down-continued"))
      (setq ue-selection-last-point (point))
    (setq ue-selection-total-lines 0)
    (setq ue-selection-last-point  nil)))
(add-hook 'post-command-hook 'ue-selection-update-cont)

(defun ue-select-line-down-continued () 
  "like Shift+down in UltraEdit, but do no selection.
Just put these continuous lines all toghether as a new item in kill-ring.
Messages will appear in minibuffer telling you how many lines were copied.
Continuous commands with no interruption append new lines to the item.
Any non-ue-select-line-down-continued command will stop gathering new lines
and make a new item in kill-ring.

post-command-hook is used in detecting the continuity of this command.
check `ue-selection-update-cont' and `post-command-hook'."
  (interactive)
  (let ((s (point)))
    (setq next-line-add-newlines t)
    (next-line 1)
    (setq next-line-add-newlines nil)
    (if (and ue-selection-last-point
             (= (save-excursion
                  (when (> (count-lines (point-min) (point)) 1)
                    (previous-line 1))
                  (point))
                ue-selection-last-point))
        (progn
          (kill-append (buffer-substring s (point)) nil)
          (setq ue-selection-total-lines (1+ ue-selection-total-lines))
          (message (format "%s lines copied" ue-selection-total-lines)))
      (kill-new (buffer-substring s (point)))
      (message "1 line copied")
      ;; so add-hook every time after we add the first line
      ;; some errors will cause post-command-hook deletion
      (add-hook 'post-command-hook 'ue-selection-update-cont)
      (setq ue-selection-total-lines 1))))

(defun ue-select-line-down ()
  "like Shift+down in UltraEdit."
  (interactive)
  (let ((s (point)))
	(setq next-line-add-newlines t)
	(next-line 1)
	(setq next-line-add-newlines nil)
	(kill-new (buffer-substring s (point)))))

(global-set-key [C-S-up]   'my-move-line-up)
(global-set-key [C-S-down] 'my-move-line-down)
(global-set-key [C-M-down] 'my-dup-line-down)
(global-set-key [M-S-down] 'ue-select-line-down-continued)
;;}}}



;; ###### Development ######
;;;{{{ for perl mode
;;; load required libraries
(defalias 'perl-mode 'cperl-mode)

(autoload 'perl-mode "cperl-mode" "Perl editing mode." t)

;; global
(autoload 'gtags-mode "gtags" "" t)
;;;}}}

;; tabs
(defun my-build-tab-stop-list (width)
  (let ((num-tab-stops (/ 80 width))
        (counter 1)
        (ls nil))
    (while (<= counter num-tab-stops)
      (setq ls (cons (* width counter) ls))
      (setq counter (1+ counter)))
    (set (make-local-variable 'tab-stop-list) (nreverse ls))))

(defun my-mode-common-hook ()
  (my-build-tab-stop-list default-tab-width)
  (setq c-basic-offset default-tab-width))
(add-hook 'sgml-mode-hook 'my-mode-common-hook)
(add-hook 'php-mode-hook 'my-mode-common-hook)
(add-hook 'c-mode-hook 'my-mode-common-hook)
(add-hook 'c++-mode-hook 'my-mode-common-hook)
(add-hook 'objc-mode-hook 'my-mode-common-hook)
(add-hook 'cperl-mode-hook 'my-mode-common-hook)
(add-hook 'python-mode-hook 'my-mode-common-hook)

;;;{{{ c-mode
(setq-default comment-column 72)
(setq compilation-scroll-output t)

(add-hook 'c-mode-hook 
          ' (lambda () 
              (c-set-style "stroustrup")
              (gtags-mode)))

(add-hook 'c++-mode-hook 
          ' (lambda () 
              (c-set-style "stroustrup")
              (gtags-mode)))

;;; auto align and indent
(defun my-indent-or-complete ()
  (interactive)
  (if (looking-at "\\>")
      (hippie-expand nil)
    (indent-for-tab-command)))

(add-hook 'c-mode-common-hook
          (function (lambda ()
					  ;;(define-key c-mode-base-map [(tab)] 'my-indent-or-complete)
					  ;;(define-key c-mode-base-map [(control m)] 'align-newline-and-indent)
                      (define-key c-mode-base-map [(control m)] 'newline-and-indent))))
;;;}}}

(set-default 'auto-mode-alist
             (append '(("\\.\\(php\\|inc\\)\\'" . php-mode)
                       ("Makefile\\'" . makefile-mode)
                       ("\\.mak\\'" . makefile-mode)
                       ("\\.p[lm]\\'" . cperl-mode)
                       ("\\.\\(cl\\|li?sp\\)\\'" . lisp-mode)
                       ("\\.css\\'" . css-mode)
		       ("\\.html\\'" . html-mode)
                       ("\\.js\\'" . javascript-mode)
                       ("\\.\\(jpg\\|png\\|gif\\)\\'" . image-mode))
                     auto-mode-alist))

;;{{{ sdcv
(global-set-key (kbd "M-1") 'ywb-dictionary-search-wordap)

(defvar ywb-dictionary-list '("朗道英汉字典5.0" "朗道汉英字典5.0"))
(defun ywb-dictionary-search-wordap (&optional word)
  (interactive)
  (or word (setq word (current-word)))
  (shell-command (format "sdcv -n %s %s"
                         (mapconcat (lambda (dict)
                                      (concat "-u " dict))
                                    ywb-dictionary-list " ")
                         word)))
(defun ywb-dictionary-search (word)
  (interactive "sword: ")
  (ywb-dictionary-search-wordap word))
;;}}}

;;{{{ sudo find file
(defvar find-file-root-prefix
  (if (featurep 'xemacs)
      "/[sudo/root@localhost]"
    "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")


(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."
  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
         ;; use a separate history list for "root" files.
         (file-name-history find-file-root-history)
         (name (or buffer-file-name default-directory))
         (tramp (and (tramp-tramp-file-p name)
                     (tramp-dissect-file-name name)))
         path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-path tramp)
            dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))

(global-set-key (kbd "C-c C-f") 'find-file-root)
;;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; other settings
(load-file (concat config-dir "8-muse.el"))
;;; (load-file (concat config-dir "9-w3m.el"))
(load-file (concat config-dir "10-org.el"))
;; (load-file (concat config-dir "3-tools.el"))
;;; (load-file (concat config-dir "19-ilisp.el"))
(load-file (concat config-dir "18-php.el"))
(load-file (concat config-dir "7-dired.el"))
(load-file (concat config-dir "20-skeleton.el"))
;; (load-file (concat config-dir "21-cedet.el"))
;; (load-file (concat config-dir "22-company.el"))
(load-file (concat config-dir "23-auto-complete.el"))
(load-file (concat config-dir "9-mode-line.el"))


;;;  Saving the buffer list : M-x desktop-save
(defun semantic-desktop-ignore-this-minor-mode (buffer)
  "Installed as a minor-mode initializer for Desktop mode.
BUFFER is the buffer to not initialize a Semantic minor mode in."
  )
(desktop-save-mode 1)
(setq desktop-base-file-name "emacs.desktop")
(setq desktop-path (list my-temp-dir))
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
;;; Do not save to desktop
(setq desktop-buffers-not-to-save
     (concat "\\(" "\\.log\\|\\.diary\\|\\.elc" "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
;; if error occurred, no matter it!
;; (condition-case nil
;;         (desktop-read)
;;     (error nil))

;;; weblogger
;;; (add-to-list 'load-path "~/.emacs.d/site-lisp/weblogger/lisp")
;;; (require 'weblogger)
;;; (global-set-key "\C-cbs" 'weblogger-start-entry)

;;; Yet Another Snippet -  pluskid@newsmth
(require 'dropdown-list)
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
(setq yas/window-system-popup-function
      #'yas/dropdown-list-popup-for-template)

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

;; css-mode
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
;; javascript
(autoload 'javascript-mode "javascript" nil t)

(font-lock-add-keywords 'c-mode '(("\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" 1
                                   font-lock-function-name-face)) t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(initial-scratch-message nil)
 '(midnight-mode t nil (midnight))
 '(org-startup-folded nil)
 '(safe-local-variable-values (quote ((code . utf-8) (c-hanging-comment-ender-p)))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-level-2 ((t (:inherit outline-2 :foreground "green" :weight bold)))))
