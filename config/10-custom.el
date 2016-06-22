;; -*- coding: utf-8 -*-

;; customize variables

(setq user-full-name "Julian Qian"
      user-mail-address "junist@gmail.com")

(setq browse-url-browser-function '(("/HyperSpec/" . w3m-browse-url)
                                    ("." . browse-url-generic))
      browse-url-generic-program "google-chrome")

;;# NOTE affect `require' function
;; (setq find-function-C-source-directory "~/src/emacs-23.2/src/"
;;       find-function-source-path '("~/src/emacs-23.2/lisp/"))

(setq-default default-directory (expand-file-name "~/"))

(setq package-archives
      '(("gnu"          . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa"        . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
        ("org"          . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ("marmalade"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")))

;; for morden machine, initiate GC every 20MB allocated
(setq gc-cons-threshold 20000000)

;; syntax highlight
;; (cond ((fboundp 'global-font-lock-mode)
;;        ;; Turn on font-lock in all modes that support it
;;        (global-font-lock-mode t)
;;        ;; Maximum colors
;;        (setq font-lock-maximum-decoration t
;;              scalable-fonts-allowed t)))
(setq font-lock-maximum-size
      (quote ((t . 1280000) (c-mode . 256000) (c++-mode . 256000))))
;; echo key strokes quickly
(setq echo-keystrokes 0.1)
;; auto fill : M-q
(setq default-justification 'full)
(setq adaptive-fill-mode nil)

;; Lines should be 80 characters wide, not 72
(setq default-fill-column 78)

;; no splash screen
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
(setq column-number-mode t)
(setq ring-bell-function 'ignore)
;; (setq visible-bell t)

(setq system-time-locale "C")

(setq truncate-partial-width-windows nil)

;;# only split window horzontal ( root cause of spliting too many windows :< )
;; (setq split-height-threshold 0
;;       split-width-threshold nil)

;; highlight trailing whitespace
(setq show-trailing-whitespace t)

;; Wrap too long lines
(toggle-truncate-lines 1)

;; delete whole line
(setq-default kill-whole-line t)
(setq kill-ring-max 1000
      mark-ring-max 1000)
(setq history-length 100)
;; indent without tab '\t' but white space
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-stop-list nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; (setq confirm-nonexistent-file-or-buffer nil)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-length nil ; do not truncate printed expressions
      eval-expression-print-level nil) ; print nested expressions

(setq display-time-mail-file "~/.emacs.d/mail")

;; chinese charactor at the end of sentences
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)
;; smooth scrolling
;; (setq hscroll-margin 1
;;       hscroll-step 1)
;; (setq redisplay-dont-pause t
;;       scroll-margin 1 ; conflict with pager-mode
;;       scroll-step 1
;;       scroll-conservatively 10000
;;       scroll-preserve-screen-position 1)
;; show matching parentheses
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; (mouse-avoidance-mode 'animate)

(auto-image-file-mode t)
;; Highlight selected regions in Gnu Emacs
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Make typing overwrite text selection
(delete-selection-mode t)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Smart indenting and pairing for all (emacs24)
;; (electric-pair-mode t)
;; (electric-indent-mode t)
;; (electric-layout-mode t)

;; (partial-completion-mode 1)             ; deprecated in emacs24
(icomplete-mode 1)
(winner-mode 1)
;; (auto-insert-mode 1)
(electric-pair-mode 1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; use clipboard
(setq x-select-enable-clipboard t)
;; (setq x-select-enable-primary t)

(setq mouse-yank-at-point t)

;; show arrow in fringe to indicate boundary of current buffer
(setq-default indicate-buffer-boundaries 'left)

(setq kill-do-not-save-duplicates t)

(setq tab-always-indent 'complete)

;; find-dired
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

;; Turn on the features disabled default
(setq disabled-command-function nil)

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Handy way of getting back to previous places
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(deh-section backup                   ;backup & autosave
  (setq make-backup-files t
        version-control t
        kept-new-versions 3
        delete-old-versions t
        kept-old-versions 2
        dired-kept-versions 1
        backup-by-copying t)
  ;; Write backup files to own directory
  (add-to-list 'backup-directory-alist
               (cons ".*" (expand-file-name "backup" my/data-dir)))

  ;; (setq make-backup-file-name-function
  ;;       (lambda (fpath)
  ;;         "Return a new file path of a given file path.
  ;; If the new path's directories does not exist, create them."
  ;;         (let* ((backup-root (expand-file-name "backup" my/data-dir))
  ;;                (bpath (concat backup-root fpath "~")))
  ;;           (make-directory (file-name-directory bpath) bpath)
  ;;           bpath)))

  ;; Put autosave files (ie #foo#) and backup files (ie foo~) in a
  ;; separated place. http://snarfed.org/gnu_emacs_backup_files
  (setq auto-save-file-name-transforms `((".*" ,(concat my/data-dir "/\\1") t))
        auto-save-list-file-prefix (expand-file-name "emacs.autosave-" my/data-dir))
  ;; Make backups of files, even when they're in version control
  (setq vc-make-backup-files t))

;; A saner ediff
(deh-package ediff
  :defer
  :config
  ;; (global-set-key "\C-cd" 'ediff-show-registry)
  (setq diff-switches "-ubB"
        ediff-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; appearance
  (set-face-foreground 'ediff-odd-diff-B "#ffffff")
  (set-face-background 'ediff-odd-diff-B "#292521")
  (set-face-foreground 'ediff-even-diff-B "#ffffff")
  (set-face-background 'ediff-even-diff-B "#292527")

  (set-face-foreground 'ediff-odd-diff-A "#ffffff")
  (set-face-background 'ediff-odd-diff-A "#292521")
  (set-face-foreground 'ediff-even-diff-A "#ffffff")
  (set-face-background 'ediff-even-diff-A "#292527"))

(deh-package abbrev
  :defer
  :config
  (setq abbrev-file-name (expand-file-name "emacs.abbrev_defs" my/data-dir))
  (if (file-exists-p abbrev-file-name)
      (read-abbrev-file abbrev-file-name))
  (setq save-abbrevs t)
  (put 'define-abbrev-table 'lisp-indent-function 1))

;; diary, todo, calendar
(deh-section calendar
  (setq diary-file (expand-file-name "diary" my/org-dir)
        todo-file-do (expand-file-name "todo-do" my/org-dir)
        todo-file-done (expand-file-name "todo-done" my/org-dir)
        todo-file-top (expand-file-name "todo-top" my/org-dir))
  (deh-add-hook initial-calendar-window-hook (toggle-truncate-lines 1))
  ;; calendar
  ;; for calendar-sunrise-sunset
  (setq calendar-longitude 114
        calendar-latitude 22.3
        calendar-location-name "Beijing"))

;; formats and timestamp
(deh-section formats
  (setq frame-title-format
        '((:eval
           (let ((login-name (getenv-internal "LOGNAME")))
             (if login-name (concat login-name "@") "")))
          (:eval (system-name))
          ":"
          (:eval (or (buffer-file-name) (buffer-name))))))

(deh-section timestamp
  (setq time-stamp-active t
        time-stamp-warn-inactive t
        time-stamp-format "%U %:y-%02m-%02d %02H:%02M:%02S"))

;; set my file register
(deh-section register
  (set-register ?. `(file . ,my/config-dir))
  (set-register ?b '(file . "~/Dropbox/"))
  (set-register ?t '(file . "~/temp/"))
  (set-register ?s '(file . "~/src/"))
  (set-register ?w '(file . "~/works/"))
  (set-register ?d '(file . "~/Desktop/")))

;; prevent no response if click the memu in File
(deh-section printer
  (fset 'print-buffer 'ignore)
  (setq lpr-command "")
  (setq printer-name ""))

(deh-section defadvice
  ;;   (defadvice pop-to-mark-command (around ensure-new-position activate)
  ;;     "When popping the mark, continue popping until the cursor
  ;; actually moves Also, if the last command was a copy - skip past
  ;; all the expand-region cruft."
  ;;     (let ((p (point)))
  ;;       (when (eq last-command 'save-region-or-current-line)
  ;;         ad-do-it
  ;;         ad-do-it
  ;;         ad-do-it)
  ;;       (dotimes (i 10)
  ;;         (when (= p (point)) ad-do-it))))

  (defadvice kill-buffer (around bury-scratch-buffer activate)
    "bury *scratch* buffer instead of kill it"
    (let ((buffer-to-kill (ad-get-arg 0)))
      (if (equal buffer-to-kill "*scratch*")
          (bury-buffer)
        ad-do-it)))

  (defadvice kill-line (before check-position activate)
    "killing the newline between indented lines and remove extra
spaces."
    (if (member major-mode
                '(emacs-lisp-mode scheme-mode lisp-mode
                                  c-mode c++-mode objc-mode python-mode
                                  latex-mode plain-tex-mode))
        (if (and (eolp) (not (bolp)))
            (progn (forward-char 1)
                   (just-one-space 0)
                   (backward-char 1)))))

  (defadvice kill-ring-save (before slickcopy activate compile)
    "When called interactively with no active region, copy the
current single line to `kill-ring' instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))

  (defadvice kill-region (before slickcut activate compile)
    "When called interactively with no active region, kill the
current single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))

  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
    (cl-flet ((process-list ())) ad-do-it))

  (defadvice kill-new (before kill-new-push-xselection-on-kill-ring activate)
    "Before putting new kill onto the kill-ring, add the
clipboard/external selection to the kill ring"
    (let ((have-paste (and interprogram-paste-function
                           (funcall interprogram-paste-function))))
      (when have-paste (push have-paste kill-ring))))

  ;;;{{{ Auto indent pasted content
  (defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not
automatically occur. Indent too many content will impact yank
performance!")

  (defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

  (dolist (command '(yank yank-pop))
    (eval
     `(defadvice ,command (after yank-indent-region activate)
        (and (not (ad-get-arg 0))
             (member major-mode
                     '(emacs-lisp-mode
                       ;; python-mode
                       c-mode c++-mode
                       ;; latex-mode
                       ;; js-mode
                       ;; php-mode
                       plain-tex-mode))
             (let ((mark-even-if-inactive transient-mark-mode))
               (yank-advised-indent-function
                (region-beginning) (region-end)))))))

  (defun yank-unindented ()
    (interactive) (yank 1))
  ;;;}}}

  (defadvice occur (around occur-mark-region)
    (save-restriction
      (if (and mark-active transient-mark-mode)
          (narrow-to-region (region-beginning) (region-end)))
      ad-do-it))
  (ad-activate 'occur)

  ;; (defadvice browse-url-generic (before ywb-browse-url-generic)
  ;;   (setq url (replace-regexp-in-string "\\cC" 'url-hexify-string url)))
  ;; (ad-activate 'browse-url-generic)

  (defadvice browse-url-file-url (after ywb-url-han)
    (let ((file ad-return-value))
      (while (string-match "[\x7f-\xff]" file)
        (let ((enc (format "%%%x" (aref file (match-beginning 0)))))
          (setq file (replace-match enc t t file))))
      (setq ad-return-value file)))
  (ad-activate 'browse-url-file-url)

  ;;# this defadvice is un-necessary, apt-get install emacs23-el
  ;; (defadvice find-library-name (before find-library-new-place activate)
  ;;   "Find library in another source path."
  ;;   (ad-set-arg 0 (replace-regexp-in-string "/usr/share/emacs/23.1/"
  ;;                                           "~/src/emacs-23.2/"
  ;;                                           (ad-get-arg 0))))
  )

;;; customization
(setq custom-file (expand-file-name "emacs.custom.el" my/data-dir))
(load custom-file t)
