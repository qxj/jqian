;; -*- coding: utf-8 -*-

;; customize variables

(setq debug-on-error nil debug-on-quit nil)

;;{{{ Generic Settings
;; syntax highlight
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)
       (setq scalable-fonts-allowed t)))
(setq font-lock-maximum-size
      (quote ((t . 1280000) (c-mode . 256000) (c++-mode . 256000))))
(setq-default default-directory (expand-file-name "~/"))
;; echo key strokes quickly
(setq echo-keystrokes 0.1)
;; auto fill : M-q
(setq default-justification 'full)
(setq adaptive-fill-mode nil)
(setq default-fill-column 72)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
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

;; (setq confirm-nonexistent-file-or-buffer nil)
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
(transient-mark-mode t)
;; Make typing overwrite text selection
(delete-selection-mode t)
;; revert buffers automatically when underlying files are changed externally
;; (global-auto-revert-mode t)

;; Smart indenting and pairing for all (emacs24)
;; (electric-pair-mode t)
;; (electric-indent-mode t)
;; (electric-layout-mode t)

;; (partial-completion-mode 1)             ; deprecated in emacs24
(icomplete-mode 1)
(winner-mode 1)
;; (auto-insert-mode 1)

(deh-section "backup"                   ;backup & autosave
  (setq make-backup-files t
        version-control t
        kept-new-versions 3
        delete-old-versions t
        kept-old-versions 2
        dired-kept-versions 1
        backup-by-copying t)
  ;; DO NOT depends on the backup, it is not really useful
  (add-to-list 'backup-directory-alist
               (cons ".*" (expand-file-name "backup" my-data-dir)))

  ;; (setq make-backup-file-name-function
  ;;       (lambda (fpath)
  ;;         "Return a new file path of a given file path.
  ;; If the new path's directories does not exist, create them."
  ;;         (let* ((backup-root (expand-file-name "backup" my-data-dir))
  ;;                (bpath (concat backup-root fpath "~")))
  ;;           (make-directory (file-name-directory bpath) bpath)
  ;;           bpath)))

  ;; Put autosave files (ie #foo#) and backup files (ie foo~) in a
  ;; separated place. http://snarfed.org/gnu_emacs_backup_files
  (setq auto-save-file-name-transforms `((".*" ,(concat my-data-dir "/\\1") t))
        auto-save-list-file-prefix (expand-file-name "emacs.autosave-" my-data-dir))
)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

(dolist (ext '(".zip" ".tgz" ".gz" ".tar" ".bz2"
               ".rpm" ".deb"
               ".out" ".exe" ".so"))
  (add-to-list 'completion-ignored-extensions ext))

;; use clipboard
(setq x-select-enable-clipboard t)

;; show arrow in fringe to indicate boundary of current buffer
(setq-default indicate-buffer-boundaries 'left)

(setq kill-do-not-save-duplicates t)

(setq tab-always-indent 'complete)

(setq browse-url-browser-function '(("/HyperSpec/" . w3m-browse-url)
                                    ("." . browse-url-generic))
      browse-url-generic-program "google-chrome")

(setq find-function-C-source-directory "~/src/emacs-23.2/src/"
      find-function-source-path '("~/src/emacs-23.2/lisp/"))

;; find-dired
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

(deh-section "ediff"
  ;; (global-set-key "\C-cd" 'ediff-show-registry)
  (setq diff-switches "-ubB"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(deh-section "abbrev"
  (setq abbrev-file-name (expand-file-name "emacs.abbrev_defs" my-data-dir))
  (if (file-exists-p abbrev-file-name)
      (read-abbrev-file abbrev-file-name))
  (setq save-abbrevs t)
  (put 'define-abbrev-table 'lisp-indent-function 1))

;; diary, todo, calendar
(deh-section "calendar"
  (setq diary-file (expand-file-name "diary" my-org-dir)
        todo-file-do (expand-file-name "todo-do" my-org-dir)
        todo-file-done (expand-file-name "todo-done" my-org-dir)
        todo-file-top (expand-file-name "todo-top" my-org-dir))
  (deh-add-hook initial-calendar-window-hook (toggle-truncate-lines 1))
  ;; calendar
  ;; for calendar-sunrise-sunset
  (setq calendar-longitude 114
        calendar-latitude 22.3
        calendar-location-name "Beijing"))

;; formats and timestamp
(deh-section "formats"
  (setq frame-title-format
        '((:eval
           (let ((login-name (getenv-internal "LOGNAME")))
             (if login-name (concat login-name "@") "")))
          (:eval (system-name))
          ":"
          (:eval (or (buffer-file-name) (buffer-name))))))

(deh-section "timestamp"
  (setq time-stamp-active t
        time-stamp-warn-inactive t
        time-stamp-format "%U %:y-%02m-%02d %02H:%02M:%02S"))

;; set my file register
(deh-section "register"
  (set-register ?. `(file . ,my-config-dir))
  (set-register ?b '(file . "~/Dropbox/"))
  (set-register ?t '(file . "~/temp/"))
  (set-register ?s '(file . "~/src/"))
  (set-register ?w '(file . "~/works/"))
  (set-register ?d '(file . "~/Desktop/")))

;; prevent no response if click the memu in File
(deh-section "printer"
  (fset 'print-buffer 'ignore)
  (setq lpr-command "")
  (setq printer-name ""))
;;;}}}

;;{{{ Customized keywords
(dolist (mode '(c-mode c++-mode java-mode lisp-mode emacs-lisp-mode python-mode
                       php-mode lisp-interaction-mode sh-mode sgml-mode))
  (font-lock-add-keywords
   mode
   '(("\\<\\(FIXME\\|TODO\\|BUG\\|HACK\\|WORKAROUND\\|DEPRECATED\\)" 1 font-lock-warning-face prepend)
     ("\\<\\(DONE\\|NOTE\\)" 1 font-lock-doc-face t)
     ;; highlight too long lines
     ;; ("^[^\n]\\{120\\}\\(.*\\)$" 1 font-lock-warning-face t)
     ;; highlight parentheses
     ;; ("(\\|)\\|\\[\\|]\\|<\\|>\\|{\\|}" . font-lock-builtin-face)
     ;; hightlight numbers
     ("\\<\-?[0-9]*\\.?[0-9]+\\>" . font-lock-constant-face))))
;;}}}

;; Turn on the features disabled default
(setq disabled-command-function nil)

(setq tooltip-use-echo-area nil)
(setq folding-folding-on-startup nil)

(setq user-full-name "Julian Qian"
      user-mail-address "junist@gmail.com")

;; for morden machine, initiate GC every 20MB allocated
(setq gc-cons-threshold 20000000)

;;; customization
(setq custom-file (expand-file-name "emacs.custom.el" my-data-dir))
(load custom-file t)
