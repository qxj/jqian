;; -*- mode: Emacs-Lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end


(deh-section "std-lib"
  ;; (partial-completion-mode 1)
  (icomplete-mode 1)
  (winner-mode 1)
  ;; (auto-insert-mode 1)
  ;; (filesets-init)
  (require 'generic-x)

  ;; smart mark, useful when edit markuped documents
;  (require 'smart-mark)
  ;; visible-line
  (require 'visible-lines nil t)
  ;; for normal term
  ;; (add-hook 'term-mode-hook 'kill-buffer-when-shell-command-exit)
  ;; .vimrc syntax hightlight
  (require 'vimrc-mode))

;; (deh-require 'auto-install
;;   ;; (auto-install-update-emacswiki-package-name t)
;;   (auto-install-compatibility-setup))

(deh-section "mode-line"
  (size-indication-mode 1)
  ;; (setq-default mode-line-buffer-identification (propertized-buffer-identification "%b"))

  (defun get-lines-4-mode-line ()
    (let ((lines (count-lines (point-min) (point-max))))
      (concat (propertize
               (concat "%l:" (format "%dL" lines))
               'mouse-face 'mode-line-highlight
               ;; 'face 'mode-line-lines-face
               'help-echo (format "%d lines" lines)) " ")))

  (defun get-size-indication-format ()
    (if (and transient-mark-mode mark-active)
        (format "%d chars" (abs (- (mark t) (point))))
      "%I"))

  (defun get-mode-line-region-face ()
    (and transient-mark-mode mark-active
         (if window-system 'region 'region-invert)))

  (setq-default
   mode-line-position
   `((:eval (get-lines-4-mode-line))
     (:propertize
      ;; "%p " ;; no need to indicate this position
      'local-map mode-line-column-line-number-mode-map
      'mouse-face 'mode-line-highlight
      'help-echo "Size indication mode\n\
mouse-1: Display Line and Column Mode Menu")
     ;; caculate word numbers of selected region. Otherwise, indicate all word number of this buffer, if no region selected.
     (size-indication-mode
      (:eval
       (propertize (get-size-indication-format)
                   'face (and transient-mark-mode mark-active (get-mode-line-region-face))
                   'local-map mode-line-column-line-number-mode-map
                   'mouse-face 'mode-line-highlight
                   'help-echo "Buffer position, mouse-1: Line/col menu")))))

  (let* ((help-echo
          "mouse-1: Select (drag to resize)\n\
mouse-2: Make current window occupy the whole frame\n\
mouse-3: Remove current window from display")
         (recursive-edit-help-echo "Recursive edit, type C-M-c to get out")
         (dashes (propertize "--" 'help-echo help-echo)))
    (setq-default mode-line-format
                  (list
                   "%e%t"
                   ;; (propertize "-" 'help-echo help-echo)
                   'mode-line-mule-info
                   'mode-line-client
                   'mode-line-modified
                   'mode-line-remote
                   'mode-line-frame-identification
                   'mode-line-buffer-identification
                   (propertize " " 'help-echo help-echo)
                   'mode-line-position
                   `(which-func-mode (" " which-func-format))
                   '(vc-mode vc-mode)
                   ;; (propertize "  " 'help-echo help-echo)
                   'mode-line-modes
                   `(global-mode-string ("" global-mode-string ,dashes))
                   (propertize "-%-" 'help-echo help-echo))))
  )

(deh-section "minibuffer"
  (mapcar (lambda (keymap)
            (define-key keymap "\C-r" 'minibuf-isearch-prev)
            (define-key keymap "\C-s" 'minibuf-isearch-next))
          (delq nil (list (and (boundp 'minibuffer-local-map)
                               minibuffer-local-map)
                          (and (boundp 'minibuffer-local-ns-map)
                               minibuffer-local-ns-map)
                          (and (boundp 'minibuffer-local-completion-map)
                               minibuffer-local-completion-map)
                          (and (boundp 'minibuffer-local-must-match-map)
                               minibuffer-local-must-match-map)))))

;;; Directories and buffers
(deh-section-after "dired"
  ;; Setting for dired
  (unless (eq system-type 'usg-unix-v)  ; solaris
    (setq dired-listing-switches "-alvh"))

  (setq dired-recursive-copies 'top
        dired-recursive-deletes 'top
        dired-isearch-filenames t       ; only search filename
        dired-dwim-target t
        dired-auto-revert-buffer t)

  ;; No confirm operations
  (setq dired-no-confirm
        '(byte-compile chgrp chmod chown compress copy delete hardlink load move print shell symlink uncompress))

  ;;# Keybind for dired
  (deh-define-key dired-mode-map
    ([return] . 'dired-find-file-single-buffer)
    ("\M-u" . 'dired-up-directory)   ; remember previous upper directory
    ;; ("\M-=" . 'dired-backup-diff)
    ("b"    . 'browse-url-of-dired-file)
    ("W"    . 'woman-dired-find-file)
    ("r"    . 'wdired-change-to-wdired-mode) ; editable mode, 'C-c C-k' abort
    (" "    . 'dired-count-directory-size)
    ("E"    . 'dired-w3m-visit)
    ("z"    . 'dired-compress-directory)
    ("s"    . 'one-key-menu-dired-sort)
    ("/"    . 'one-key-menu-dired-filter))

  ;;# hooks
  (deh-add-hook dired-load-hook
    (load "dired-x")
    ;; Make the execuatable file with different color
    (add-to-list 'dired-font-lock-keywords
                 (list dired-re-exe
                       '(".+" (dired-move-to-filename) nil (0 font-lock-type-face))) t))
  (deh-add-hook dired-after-readin-hook
    (set (make-local-variable 'truncate-lines) t)
    (save-excursion                     ; sort directories first
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point)
                            (point-max)))))

  ;;# helper functions
  (defun dired-find-file-single-buffer ()
    "kill current buffer when moving to subdirectory"
    (interactive)
    (let ((previous-dired-buffer (current-buffer))
          (file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (progn (dired-find-file)
                 (kill-buffer previous-dired-buffer))
        (dired-find-file))))
  (defun dired-compress-directory ()
    "Compress directory in `dired-mode'."
    (interactive)
    (let ((files (dired-get-marked-files t)))
      (if (and (null (cdr files))
               (string-match "\\.\\(tgz\\|tar\\.gz\\)" (car files)))
          (shell-command (concat "tar -xvf " (car files)))
        (let ((cfile (concat (file-name-nondirectory
                              (if (null (cdr files))
                                  (car files)
                                (directory-file-name
                                 default-directory))) ".tgz"))
              proc)
          (setq cfile
                (read-from-minibuffer "Compress file name: " cfile))
          (setq proc
                (apply 'start-process (append (list "diredz" nil "tar"
                                                    "-hzcvf" cfile) files)))
          (set-process-sentinel proc
                                (lambda (&rest args)
                                  (message "Compress finished. Press g to flush directory!")))))))
  (defun dired-w3m-visit (file)
    (interactive (list (dired-get-filename nil t)))
    (w3m-goto-url (concat "file://" file)))
  (defun dired-count-directory-size ()
    (interactive)
    (let* ((file (dired-get-filename nil t))
           (dir (if (file-directory-p file) file
                  (file-name-directory file)))
           (buf (get-buffer-create "*dired-count-directory-size*"))
           (proc (start-process-shell-command "dirsize" buf
                                              (format "du -hs \"%s\"" dir))))
      (set-process-sentinel
       proc
       (lambda (proc event)
         (let ((buf (process-buffer proc)))
           (with-current-buffer buf
             (message "%s" (buffer-substring-no-properties (point-min) (point-max))))
           (kill-buffer buf))))))

  ;;# sort functions
  (defun dired-sort-size ()
    "Dired sort by size."
    (interactive)
    (dired-sort-other (concat dired-listing-switches "S")))
  (defun dired-sort-extension ()
    "Dired sort by extension."
    (interactive)
    (dired-sort-other (concat dired-listing-switches "X")))
  (defun dired-sort-ctime ()
    "Dired sort by create time."
    (interactive)
    (dired-sort-other (concat dired-listing-switches "ct")))
  (defun dired-sort-utime ()
    "Dired sort by access time."
    (interactive)
    (dired-sort-other (concat dired-listing-switches "ut")))
  (defun dired-sort-time ()
    "Dired sort by time."
    (interactive)
    (dired-sort-other (concat dired-listing-switches "t")))
  (defun dired-sort-name ()
    "Dired sort by name."
    (interactive)
    (dired-sort-other (concat dired-listing-switches "")))

  ;; one-key functions
  (defun one-key-menu-dired-sort ()
    "The `one-key' menu for DIRED-SORT."
    (interactive)
    (one-key-menu
     "DIRED-SORT"
     '((("s" . "Size")          . dired-sort-size)
       (("x" . "Extension")     . dired-sort-extension)
       (("n" . "Name")          . dired-sort-name)
       (("t" . "Modified Time") . dired-sort-time)
       (("u" . "Access Time")   . dired-sort-utime)
       (("c" . "Create Time")   . dired-sort-ctime))
     t))
  (defun one-key-menu-dired-filter ()
    "The `one-key' menu for DIRED-FILTER."
    (interactive)
    (one-key-menu
     "DIRED-SORT"
     '((("r" . "Filter by regexp")    . ywb-dired-filter-regexp)
       (("." . "Filter by extension") . ywb-dired-filter-extension)
       (("/" . "Filter match")        . my-dired-omit-expunge))
     t)))

(deh-section-after "dired-x"
  (deh-add-hook dired-mode-hook
    (dired-omit-mode t))

  (dolist (ext '(".bak"))
    (add-to-list 'dired-omit-extensions ext))

  (setq dired-omit-files
        (concat "^[.#]" "\\|"
                "^" (regexp-opt '(".." "." "TAGS" "GPATH" "GRTAGS" "GSYMS" "GTAGS") t) "$"))

  (setq my-dired-guess-command-alist
        '(("acroread" "pdf")
          ("evince" "pdf")
          ;; ("xpdf" "pdf")
          ("xdvi" "dvi")
          ("dvipdf" "dvi")
          ("zxpdf" "pdf.gz")
          ("ps2pdf" "ps" "eps")
          ("gv" "ps" "eps")
          ("unrar x" "rar")
          ("kchmviewer" "chm")
          ("mplayer -stop-xscreensaver" "avi" "mpg" "rmvb" "rm" "flv" "wmv" "mkv")
          ("mplayer -playlist" "list")
          ("display" "gif" "jpeg" "jpg" "tif" "png" )
          ("eog" "gif" "jpeg" "jpg" "tif" "png")
          ("docview.pl" "doc")
          ("ooffice -writer" "ods" "doc")
          ("ooffice -calc"  "xls")
          ("ooffice -impress" "odt" "ppt")
          ("gnumeric" "xls")
          ("7z x" "7z")
          ("djview" "djvu")
          ("perl" "pl")
          ("firefox" "xml" "html" "htm" "mht")))
  ;; Based upon the name of a file, Dired tries to guess what shell
  ;; command you might want to apply to it. For example, if you have
  ;; point on a file named foo.tar and you press !, Dired will guess
  ;; you want to ‘tar xvf’ it and suggest that as the default shell
  ;; command.
  (dolist (file my-dired-guess-command-alist)
    (add-to-list 'dired-guess-shell-alist-default
                 (list (concat "\\." (regexp-opt (cdr file) t) "$")
                       (car file))))
  ;;# Dired Association
  (if (eq system-type 'windows-nt)
      (progn
        (defun dired-custom-execute-file (&optional arg)
          (interactive "P")
          (mapcar #'(lambda (file)
                      (w32-shell-execute "open" (convert-standard-filename file)))
                  (dired-get-marked-files nil arg))))
    ;; Redefine of this function
    (defun dired-run-shell-command (command)
      (let ((handler
             (find-file-name-handler (directory-file-name default-directory)
                                     'shell-command)))
        (if handler
            (apply handler 'shell-command (list command))
          (start-process-shell-command "dired-run" nil command)))
      ;; Return nil for sake of nconc in dired-bunch-files.
      nil))
  )

(deh-require 'ido
  ;; (ido-mode 1) ;; avoid recursive tramp load error, it's a reported bug
  (ido-everywhere t)
  (add-hook 'term-setup-hook 'ido-mode)

  (setq ido-enable-regexp t
        ido-enable-dot-prefix t
        ido-enable-flex-matching t
        ido-enable-tramp-completion nil
        ido-record-ftp-work-directories nil
        ;; ido-enable-last-directory-history nil ; avoid tramp connect when start
        ido-use-faces t
        ;; ido-use-filename-at-point 'guess
        ;; ido-use-url-at-point t
        ido-auto-merge-work-directories-length -1
        ido-max-work-file-list 20)

  (setq ido-save-directory-list-file
        (expand-file-name "emacs.ido-last" my-temp-dir)
        org-id-locations-file
        (expand-file-name "emacs.ido-locations" my-temp-dir))
  (setq ido-ignore-buffers
        '("\\` " "^\\*.+" "_region_" "^TAGS$")
        ido-ignore-directories
        '("^auto/" "^CVS/" "^\\.")
        ido-ignore-files
        '("\\.\\(aux\\|nav\\|out\\|log\\|snm\\|toc\\|vrb\\|dsp\\|dsw\\|sln\\|vcproj\\|vspscc\\|vssscc\\)$"
          "^\\(CVS\\|TAGS\\|GPATH\\|GRTAGS\\|GSYMS\\|GTAGS\\)$"
          "_region_" "^[.#]")
        ido-work-directory-list-ignore-regexps
        `(,tramp-file-name-regexp))
  (setq ido-file-extensions-order
        '(".h" ".c" ".cpp" ".e." ".txt" ".org"))

  (add-hook 'ido-setup-hook 'ido-my-keys)
  (defun ido-my-keys ()
    "Add my keybindings for ido."
    (deh-define-key ido-completion-map
      ((kbd "C-n")   . 'ido-next-match-dir)
      ((kbd "C-p")   . 'ido-prev-match-dir)
      ((kbd "M-u")   . 'ido-up-directory)
      ((kbd "C-M-h") . 'ido-goto-home)
      ((kbd "C-u")   . 'ido-clean-text)
      ((kbd "C-w")   . 'ido-delete-backward-word-updir)
      ;; Remind keybinds
      ;; ((kbd "C-a") . 'ido-toggle-ignore)
      ((kbd "C-S-p") . 'ido-toggle-prefix)
      ;; ((kbd "C-t") . 'ido-enable-regexp)
      ;; ((kbd "M-n") . 'ido-next-work-directory)
      ;; ((kbd "M-p") . 'ido-prev-work-directory)
      ))

  (defun ido-clean-text ()
    "Clean `ido-text'."
    (interactive)
    (if (= (minibuffer-prompt-end) (point))
        (ido-up-directory t)
      (delete-region (minibuffer-prompt-end) (point-max))))

  (defun ido-goto-home ()
    (interactive)
    "Go to home directory when use `ido-find-file'."
    (ido-set-current-home)
    (setq ido-exit 'refresh)
    (exit-minibuffer))

  ;; visit with dired also push the diretory to `ido-work-directory-list'
  (defadvice ido-file-internal (after ido-dired-add-work-directory)
    (when (eq ido-exit 'dired)
      (ido-record-work-directory (expand-file-name default-directory))))
  (ad-activate 'ido-file-internal)

  ;; Replace completing-read wherever possible, unless directed otherwise
  (defvar ido-enable-replace-completing-read nil
    "If t, use ido-completing-read instead of completing-read if possible.

    Set it to nil using let in around-advice for functions where the
    original completing-read is required.  For example, if a function
    foo absolutely must use the original completing-read, define some
    advice like this:

    (defadvice foo (around original-completing-read-only activate)
      (let (ido-enable-replace-completing-read) ad-do-it))")
  (defadvice completing-read
    (around use-ido-when-possible activate)
    (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
            (boundp 'ido-cur-list)) ; Avoid infinite loop from ido calling this
        ad-do-it
      (let ((allcomp (all-completions "" collection predicate)))
        (if allcomp
            (setq ad-return-value
                  (ido-completing-read prompt
                                       allcomp
                                       nil require-match initial-input hist def))
          ad-do-it))))

 ;; push the most used directory to `ido-work-directory-list'
  (mapc (lambda (dir)
          (add-to-list 'ido-work-directory-list
                       (expand-file-name dir)))
        '("~/.emacs.d/config/"
          "~/.emacs.d/site-lisp/"
          "~/projects/"
          "~/work/"
          "~/temp/"
          "~/bin/"
          "~/")))

(deh-section-after "ibuffer"
  (setq ibuffer-old-time 24
        ibuffer-show-empty-filter-groups nil)
  ;; keybinds
  ;; (global-set-key (kbd "C-x C-b") 'ibuffer)
  (deh-define-key ibuffer-mode-map
    ("s" . 'one-key-menu-ibuffer-sort)
    ("r" . 'ibuffer-rename-buffer)
    ("\C-x\C-f" . 'ibuffer-find-file)
    (" " . 'scroll-up))

  (defun one-key-menu-ibuffer-sort ()
    "The `one-key' menu for IBUFFER-SORT."
    (interactive)
    (one-key-menu
     "IBUFFER-SORT"
     '((("a" . "Alphabetic") . ibuffer-do-sort-by-alphabetic)
       (("f" . "File Name")  . ibuffer-do-sort-by-file-name)
       (("r" . "Recenctly")  . ibuffer-do-sort-by-recency)
       (("m" . "Major Mode") . ibuffer-do-sort-by-major-mode)
       (("n" . "Mode Name")  . ibuffer-do-sort-by-mode-name)
       (("s" . "File Size")  . ibuffer-do-sort-by-size)
       (("p" . "File Name/Process") . ibuffer-do-sort-by-filename/process))
     t))

  (define-ibuffer-sorter file-name
    "Sort buffers by associated file name"
    (:description "file name")
    (apply 'string<
           (mapcar (lambda (buf)
                     (with-current-buffer (car buf)
                       (or buffer-file-name default-directory)))
                   (list a b))))
  (defun ibuffer-rename-buffer ()
    (interactive)
    (call-interactively 'ibuffer-update)
    (let* ((buf (ibuffer-current-buffer))
           (name (generate-new-buffer-name
                  (read-from-minibuffer "Rename buffer(to new name): "
                                        (buffer-name buf)))))
      (with-current-buffer buf
        (rename-buffer name)))
    (call-interactively 'ibuffer-update))
  (defun ibuffer-find-file ()
    (interactive)
    (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                               (if (buffer-live-p buf)
                                   (with-current-buffer buf
                                     default-directory)
                                 default-directory))))
      (call-interactively 'ido-find-file)))

  (deh-add-hook ibuffer-mode-hook
    (require 'ibuf-ext nil t)
    (ibuffer-switch-to-saved-filter-groups "default"))

  (eval-after-load "ibuf-ext"
    '(progn
       (setq ibuffer-saved-filter-groups
             '(("default"
                ("*buffers*" (or (mode . term-mode)
                                 (name . "^\\*gud")
                                 (name . "^\\*scratch")
                                 ;; slime
                                 (name . "^\\*slime-repl")
                                 (mode . message-mode)))
                ("programming" (or (mode . c++-mode)
                                   (mode . c-mode)
                                   (mode . makefile-mode)))
                ("script" (or (mode . python-mode)
                              (mode . sh-mode)
                              (mode . perl-mode)
                              (mode . org-mode)
                              (mode . LaTeX-mode)))
                ("web" (or  (mode . html-mode)
                            (mode . css-mode)
                            (mode . php-mode)
                            (mode . javascript-mode)
                            (mode . js2-mode)))
                ("elisp" (or (mode . emacs-lisp-mode)
                             (mode . lisp-interaction-mode)))
                ("dired" (mode . dired-mode))
                ("*others*" (name . "\\*.*\\*")))))))
  )

(deh-require 'uniquify
  (setq uniquify-buffer-name-style 'forward)
  ;; (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  )

(deh-section "tramp"
  ;; If Tramp still isn’t fast enough for you (or if you don’t use
  ;; linux), try [Accelerating OpenSSH connections with ControlMaster |
  ;; http://linux.com/feature/54498]
  ;;
  ;; Set up the ControlMaster feature by adding the following lines to
  ;; ~/.ssh/config:
  ;;
  ;; Host *
  ;; ControlMaster auto
  ;; ControlPath ~/.ssh/master-%r@%h:%p
  ;;

  ;; TODO: if prompted to input password for some tramped hosts when
  ;; emacs start, it'd be caused by the variables
  ;; ido-last-directory-list, etc. in .ido-last. A workaound is to
  ;; delete it away from emacs. To be fixed.
  ;;

  ;; (setq tramp-mode nil)                  ; disable tramp
  (setq tramp-auto-save-directory my-temp-dir
        tramp-persistency-file-name (expand-file-name "tramp" my-temp-dir)
        tramp-default-method "ssh"
        ;; tramp-syntax 'url
        password-cache-expiry nil)
  ;;# avoid to backup tramp files
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))

  ;; tramp-compile is deperated, which is integrated into compile
  ;; command.
  )

;;; Session management
(deh-section "bookmark"
  ;; autosave bookmark into the diskete
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file (expand-file-name "emacs.bookmark" my-temp-dir))
  (deh-add-hook bookmark-bmenu-mode-hook
    (font-lock-add-keywords
     nil
     '(("^\\s-+\\(.*+\\)[ ]\\{2,\\}"
        (1 (let ((file (split-string (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)) " \\{2,\\}")))
             (if (and (not (file-remote-p (nth 2 file)))
                      (file-directory-p (nth 2 file)))
                 font-lock-function-name-face
               nil))))
       ("^>.*" . font-lock-warning-face)
       ("^D.*" . font-lock-type-face)))))

(deh-require-if 'desktop
  (not (emacs-process-duplicated-p))

  (setq desktop-base-file-name (concat "emacs.desktop-" (system-name))
        desktop-path (list my-temp-dir)
        desktop-restore-eager 8        ; firstly restore 8 buffers
        history-length 100)

  ;;# not to save
  (setq desktop-globals-to-save
        (delq 'tags-table-list desktop-globals-to-save))
  (setq desktop-globals-to-save
        (delq 'file-name-history desktop-globals-to-save))
  (setq desktop-buffers-not-to-save
        (concat "\\(" "\\.log\\|\\.diary\\|\\.elc" "\\)$"))
  (dolist (mode '(dired-mode Info-mode info-lookup-mode fundamental-mode))
    (add-to-list 'desktop-modes-not-to-save mode))

  ;;# to save
  (add-to-list 'desktop-globals-to-save 'kill-ring)
  (if (boundp 'windata-name-winconf)
      (add-to-list 'desktop-globals-to-save 'windata-named-winconf))

  ;; if error occurred, no matter it!
  ;; (condition-case nil
  ;;     (desktop-read)
  ;;   (error nil))
  (desktop-save-mode 1)
  ;; for multiple desktops
  ;; (require 'desktop-menu)
  ;; (setq desktop-menu-directory my-temp-dir
  ;;       desktop-menu-base-filename desktop-base-file-name
  ;;       desktop-menu-list-file "emacs.desktops")

  ;;# persist desktop into file every 10 mins
  (run-with-idle-timer 600 600 'desktop-save-in-desktop-dir)

  (defun my-save-desktop (file)
    (interactive
     (list (let ((default-directory "~"))
             (read-file-name "Save desktop: "))))
    (let ((desktop-base-file-name (file-name-nondirectory file)))
      (desktop-save (file-name-directory file))))
  (defun my-load-desktop (file)
    (interactive
     (list (let ((default-directory "~"))
             (read-file-name "Load desktop: "))))
    (if (y-or-n-p "kill all buffer")
        (mapc (lambda (buf)
                (let ((name (buffer-name buf))
                      (file (buffer-file-name buf)))
                  (unless (or (and (string= (substring name 0 1) " ") (null file))
                              (string-match "^\\*.*\\*" (buffer-name buf)))
                    (kill-buffer buf))))
              (buffer-list)))
    (let ((desktop-base-file-name (file-name-nondirectory file)))
      (desktop-read (file-name-directory file))))
  )

(deh-require-reserved 'session
  (setq session-save-file (expand-file-name "emacs.session" my-temp-dir))
  (setq session-save-file-coding-system 'utf-8-unix)
  (add-to-list 'session-globals-exclude 'org-mark-ring)
  (add-hook 'after-init-hook 'session-initialize))

(deh-section-after "bm"

  (deh-define-key bm-show-mode-map
    ("n" . 'bm-show-next)
    ("p" . 'bm-show-prev)
    ("d" . 'bm-show-remove-bookmark))

  (setq-default bm-buffer-persistence t)
  (setq bm-repository-file
        (expand-file-name "emacs.bm-repository" my-temp-dir))
  (setq bm-cycle-all-buffers t
        bm-highlight-style
        (if (and window-system (> emacs-major-version 21))
            'bm-highlight-only-fringe
          'bm-highlight-only-line))
  ;; For persistent bookmarks
  (add-hook' after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))
  ;; Sync bookmarks
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  ;; make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)

  ;; mouse setting
  (global-set-key [left-margin mouse-2] 'bm-toggle-mouse)
  (global-set-key [left-margin mouse-3] 'bm-next-mouse)

  ;; remove bookmark in bm-show
  (defun bm-show-remove-bookmark nil
    "Remove the bookmark on current line in the `bm-show-buffer-name' buffer."
    (interactive)
    (let ((buffer-name (get-text-property (point) 'bm-buffer))
          (bookmark (get-text-property (point) 'bm-bookmark)))
      (if (null buffer-name)
          (message "No bookmark at this line.")
        (bm-bookmark-remove bookmark)
        ;; TODO: refresh bookmark show
        (when bm-electric-show (bm-show-quit-window))
        )))

  ;; hack bm.el
  (defvar bm-previous-window-conf nil
    "Window configuration before switching to buffer.")
  (defun bm-show-goto-bookmark nil
    "Goto the bookmark on current line in the `bm-show-buffer-name' buffer."
    (interactive)
    (let ((buffer-name (get-text-property (point) 'bm-buffer))
          (bookmark (get-text-property (point) 'bm-bookmark)))
      (if (null buffer-name)
          (message "No bookmark at this line.")
        (pop-to-buffer (get-buffer buffer-name) nil t) ; keep pop buffer in the same window
        (bm-goto bookmark)
        (when bm-electric-show
          (bm-show-quit-window)
          (set-window-configuration bm-previous-window-conf)
          (setq bm-previous-window-conf nil)))))
  (defun bm-show-all nil
    "Show bookmarked lines in all buffers."
    (interactive)
    (let ((lines
           (save-excursion
             (mapconcat '(lambda (buffer)
                           (set-buffer buffer)
                           (bm-show-extract-bookmarks))
                        (buffer-list) ""))))
      (setq bm-previous-window-conf (current-window-configuration))
      (bm-show-display-lines lines)))
  (defun bm-show nil
    "Show bookmarked lines in current buffer."
    (interactive)
    (setq bm-previous-window-conf (current-window-configuration))
    (bm-show-display-lines (bm-show-extract-bookmarks)))
  )

;; recent-jump
(deh-require 'recent-jump)

;; recent opened files
(deh-section "recentf"
  ;; recent finded buffers
  (setq recentf-max-saved-items 1000
        recentf-save-file (expand-file-name "emacs.recentf" my-temp-dir)
        recentf-exclude `(,my-temp-dir
                          ,tramp-file-name-regexp))
  (recentf-mode t)

  (defun recentf-open-files-compl ()
    "Open files opened recently with `ido-completing-read'."
    (interactive)
    (let (el (elist '()))
      (dolist (item recentf-list)
        (setq el (file-name-nondirectory item))
        (unless (zerop (length el))     ; skip dir
          (let ((orig el))
            ;; distinguish collided file name
            (while (assoc el elist)
              (if (string-match (format "%s<\\([0-9]+\\)>$" orig) el)
                  (setq el (format "%s<%d>" orig
                                   (+ 1 (string-to-number (match-string 1 el)))))
                (setq el (format "%s<%d>" orig 1))
                )))
          (add-to-list 'elist (cons el item))))
      ;; use `ido-completing-read' instead of `completing-read'
      (find-file (cdr (assoc (ido-completing-read "Open file: "
                                                  (mapcar 'car elist))
                             elist)))))
  (eval-after-load "recentf"
    '(progn
       ;; Also store recent opened directories besides files
       (add-hook 'dired-mode-hook
                 (lambda () (recentf-add-file dired-directory))))))

(deh-require 'pager
  (global-set-key (kbd "C-v") 'pager-page-down)
  (global-set-key (kbd "M-v") 'pager-page-up)
  (global-set-key (kbd "<up>") 'pager-row-up)
  (global-set-key (kbd "M-p") 'pager-row-up)
  (global-set-key (kbd "<down>") 'pager-row-down)
  (global-set-key (kbd "M-n") 'pager-row-down)
  ;; Some individual keybind overrides
  (eval-after-load "info"
    '(progn
       (define-key Info-mode-map (kbd "M-p") 'pager-row-up)
       (define-key Info-mode-map (kbd "M-n") 'pager-row-down)))
  (eval-after-load "man"
    '(progn
       (define-key Man-mode-map (kbd "M-p") 'pager-row-up)
       (define-key Man-mode-map (kbd "M-n") 'pager-row-down)))
  (eval-after-load "woman"
    '(progn
       (define-key woman-mode-map (kbd "M-p") 'pager-row-up)
       (define-key woman-mode-map (kbd "M-n") 'pager-row-down)))
  (eval-after-load "w3m"
    '(progn
       (define-key w3m-mode-map (kbd "M-p") 'pager-row-up)
       (define-key w3m-mode-map (kbd "M-n") 'pager-row-down)))
  )

(deh-section "ffap"
  ;; (ffap-bindings)

  ;; for windows path recognize
  (setq ffap-string-at-point-mode-alist
        '((file "--{}:\\\\$+<>@-Z_a-z~*?\x100-\xffff" "<@" "@>;.,!:")
          (url "--:=&?$+@-Z_a-z~#,%;*" "^A-Za-z0-9" ":;.,!?")
          (nocolon "--9$+<>@-Z_a-z~" "<@" "@>;.,!?")
          (machine "-a-zA-Z0-9." "" ".")
          (math-mode ",-:$+<>@-Z_a-z~`" "<" "@>;.,!?`:")))

  (eval-after-load "ffap"
    '(setq ffap-c-path (append ffap-c-path user-include-dirs)))

  (eval-after-load "filecache"
    '(progn (file-cache-add-directory-list load-path)
            (file-cache-add-directory-list user-include-dirs)
            (file-cache-add-directory "/usr/include")
            (file-cache-add-directory-recursively "/usr/include/c++")
            (file-cache-add-directory-recursively "/usr/local/include")))
  )

(deh-section "hippie-expand"
  ;; Recommand hippie-expand other than dabbrev-expand for `M-/'
  (eval-after-load "dabbrev" '(defalias 'dabbrev-expand 'hippie-expand))
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-visible
          try-expand-list
          try-expand-line
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          ;; try-expand-list-all-buffers
          ;; try-expand-line-all-buffers
          try-complete-file-name-partially
          try-complete-file-name
          try-complete-lisp-symbol
          try-complete-lisp-symbol-partially
          try-expand-whole-kill)))

;;; Buffer view
(deh-section-after "view"
  (setq view-read-only t)

  (deh-define-key view-mode-map
    ;; simulate vi keybinds
    ("h" . 'backward-char)
    ("l" . 'forward-char)
    ("j" . 'next-line)
    ("k" . 'previous-line)
    ("c" . 'recenter-top-bottom)
    ("0" . 'beginning-of-line)
    ("$" . 'end-of-line)
    ("g" . 'beginning-of-buffer)
    ("G" . 'end-of-buffer)
    ("n" . 'View-scroll-line-forward)
    ("p" . 'View-scroll-line-backward)
    ((kbd "<backspace>") . 'View-scroll-page-backward)
    ((kbd "SPC") . 'View-scroll-page-forward)
    ("?" . 'View-search-regexp-backward)
    ;; register
    ("m" . 'point-to-register)
    ("'" . 'register-to-point)
    ;; gtags
    ("." . 'gtags-find-tag)
    ("," . 'gtags-pop-stack)
    ("i" . 'gtags-find-tag)
    ("u" . 'gtags-pop-stack)
    ;; sourcepair
    ("a" . 'sourcepair-load)
    ;; eassist
    ("L" . 'eassist-list-methods)
    ;; generic
    ("f" . 'ido-find-file)
    ("d" . 'dired-jump)
    ("o" . 'my-switch-recent-buffer)
    ("q" . 'bury-buffer)
    ("\C-k" . 'kill-this-buffer)))

(deh-section "doc-view"
  (deh-add-hook doc-view-mode-hook
    (define-key doc-view-mode-map [remap move-beginning-of-line] 'image-bol)
    (define-key doc-view-mode-map [remap move-end-of-line] 'image-eol)))

(deh-section "image"
  (defun image-display-info ()
    (interactive)
    (let ((image (image-get-display-property))
          info)
      (setq info
            (list
             (cons "File Size"
                   (let ((size (length (plist-get (cdr image) :data))))
                     (if (> size (* 10 1024))
                         (format "%.2f kb" (/ size 1024))
                       size)))
             (cons "Image Type" (plist-get (cdr image) :type))
             (cons "Image Size"
                   (let ((size (image-size image t)))
                     (format "%d x %d pixels" (car size) (cdr size))))))
      (with-current-buffer (get-buffer-create "*image-info*")
        (erase-buffer)
        (dolist (item info)
          (insert (format "%12s: %s\n"
                          (propertize (car item) 'face 'bold)
                          (cdr item))))
        (display-buffer (current-buffer)))))
  (add-hook 'image-mode-hook
            (lambda ()
              (define-key image-mode-map "I" 'image-display-info))))

(deh-section-after "w3m"
  (setq w3m-verbose t                   ; log in *Messages*
        w3m-default-display-inline-images t)
  (deh-define-key w3m-mode-map
    ("n" . (lambda nil (interactive) (ywb-w3m-goto-url w3m-next-url)))
    ("p" . (lambda nil (interactive) (ywb-w3m-goto-url w3m-previous-url)))
    ("t" . (lambda nil (interactive) (ywb-w3m-goto-url w3m-contents-url))))
  (deh-add-hook w3m-load-hook
    (add-to-list
     'w3m-relationship-estimate-rules
     `(w3m-relationship-simple-estimate
       ""
       ,(concat "<a\\s-+href=" w3m-html-string-regexp
                "\\s-*>.\\{,25\\}\\(?:next\\|后\\|下\\)")
       ,(concat "<a\\s-+href=" w3m-html-string-regexp
                "\\s-*>.\\{,25\\}\\(?:prev\\|前\\|上\\)")
       nil
       ,(concat "<a\\s-+href=" w3m-html-string-regexp
                "\\s-*>.\\{,25\\}\\(?:index\\|目录\\)"))))

  (defun ywb-w3m-goto-url (url)
    (if (and url (stringp url))
        (w3m-goto-url url)))
  (defun w3m-browse-anchor-external ()
    "Browse one w3m's anchor link with another external browser."
    (interactive)
    (let ((deactivate-mark nil)
          (url (or (w3m-anchor) (w3m-image))))
      (if url
          (browse-url url)
        (w3m-message "Invalid url."))))
  (defun my-toggle-w3m ()
    "Switch to a w3m buffer or return to the previous buffer."
    (interactive)
    (if (derived-mode-p 'w3m-mode)
        ;; Currently in a w3m buffer
        ;; Bury buffers until you reach a non-w3m one
        (while (derived-mode-p 'w3m-mode)
          (bury-buffer))
      ;; Not in w3m
      ;; Find the first w3m buffer
      (let ((list (buffer-list)))
        (while list
          (if (with-current-buffer (car list)
                (derived-mode-p 'w3m-mode))
              (progn
                (switch-to-buffer (car list))
                (setq list nil))
            (setq list (cdr list))))
        (unless (derived-mode-p 'w3m-mode)
          (call-interactively 'w3m))))))

;;; Edit
(deh-section "occur"
  (deh-add-hook occur-mode-hook
    (require 'moccur-edit nil t)
    (setq truncate-lines t))

  ;; make cursor become a line
  ;; (require 'bar-cursor)

  (eval-after-load "moccur-edit"
    '(progn
       (defadvice moccur-edit-change-file (after save-after-moccur-edit-buffer activate)
         (save-buffer))))

  ;; handy functions
  (defun moccur-word-all-buffers (regexp)
    "Run `multi-occur' to find regexp in all buffers."
    (if (= 0 (length regexp))
        (message "Regexp is blank.")
      (let ((buffers (buffer-list)))
        (dolist (buffer buffers)
          (let ((pos (string-match " *\\*" (buffer-name buffer))))
            (when (and pos (= 0 pos))
              (setq buffers (remq buffer buffers)))))
        (multi-occur buffers regexp))))

  (defun moccur-all-buffers (&optional prompt)
    "Run `multi-occur' to find current word in all buffers."
    (interactive "P")
    (let ((word (grep-tag-default)))
      (when (or prompt (= (length word) 0))
        (setq word (read-regexp "List lines matching regexp" word)))
      (moccur-word-all-buffers word)))

  (defun moccur-todo-all-buffers ()
    "Run `multi-occur' to find 'TODO' in all buffers."
    (interactive)
    ;; (moccur-word-all-buffers "\\<\\([Tt][Oo][Dd][Oo]\\|[Ff][Ii][Xx][Mm][Ee]\\)\\>")
    (moccur-word-all-buffers "\\<\\(FIXME\\|TODO\\):")
    )
  )

(deh-section "grep"
  (eval-after-load "grep"
    '(add-to-list 'grep-files-aliases '("hcpp" . "*.h *.c *.[hc]pp")))

  (defun grep-current-dir (&optional prompt wd)
    "Run `grep' to find current word in current directory."
    (interactive "P")
    (let* ((word (or wd (grep-tag-default)))
           (cmd (concat "grep -inrHIE \"" word "\" ."
                        " | grep -vE \"\.svn/|\.git/|\.hg/|\.bzr/|CVS/\"")))
      (grep-apply-setting 'grep-use-null-device nil)
      (if (or prompt (= (length word) 0))
          (grep (read-shell-command
                 "Run grep (like this): " cmd 'grep-history))
        (if (= 0 (length word))
            (message "Word is blank.")
          (grep cmd)))))

  (defun grep-todo-current-dir ()
    "Run `grep' to find 'TODO' in current directory."
    (interactive)
    (grep-current-dir nil "TODO|FIXME")))

(deh-section "auto-complete"
  (require 'auto-complete-config)
  ;; specify a file stores data of candidate suggestion
  (setq ac-comphist-file (expand-file-name "ac-comphist.dat" my-temp-dir))
  (setq ac-auto-start 3
        ac-auto-show-menu 1.5
        ;; ac-candidate-limit ac-menu-height ; improve drop menu performance
        ac-ignore-case nil
        ac-show-menu-immediately-on-auto-complete nil
        ;; ac-expand-on-auto-complete nil
        ;; ac-trigger-key nil
        ac-quick-help-delay 1.5
        ac-disable-faces nil
        ac-dwim t)

  ;; disable auto-complete in comments
  ;; (setq ac-disable-faces
  ;;       '(font-lock-string-face font-lock-doc-face))
  (setq ac-disable-faces '(font-lock-string-face))

  ;; for terminal, works well with `global-hl-line-mode'
  (if (null window-system)
      (set-face-background 'ac-completion-face "blue"))

  (add-to-list 'ac-dictionary-directories
               (expand-file-name "ac-dict" my-startup-dir))
  (add-to-list 'ac-user-dictionary-files
               (expand-file-name "ac.dict" my-startup-dir))

  ;;# enable auto-complete in some modes
  (add-to-list 'ac-modes 'org-mode)
  (add-to-list 'ac-modes 'LaTeX-mode)

  (ac-config-default)

  ;; donot use RET for auto complete, only TAB
  (deh-define-key ac-completing-map
    ((kbd "<return>") . nil)
    ((kbd "RET") . nil)
    ((kbd "TAB") . 'ac-complete)
    ;; ((kbd "M-/") . 'ac-stop)
    )
  ;; when completion menu is displayed
  (setq ac-use-menu-map t)
  (deh-define-key ac-menu-map
    ("\C-n" . 'ac-next)
    ("\C-p" . 'ac-previous))

  (ac-set-trigger-key "TAB")

  ;; press <TAB> to active `auto-complete'
  ;; (deh-local-set-key auto-complete-mode-hook
  ;;   ((kbd "TAB") . 'auto-complete-tab-action))
  (defun auto-complete-tab-action ()
    "If cursor at one word end, try auto complete it. Otherwise,
indent line."
    (interactive)
    (if (looking-at "\\>")
        (auto-complete)
      (indent-for-tab-command)))

  ;; c/c++
  ;; hack auto-complete.el (deperated)
  ;; add ac-prefix "->" to function `ac-prefix-c-dot'
  (defun ac-cc-mode-setup ()
    "customized setup for `c-mode-common-hook'"
    (dolist (command `(c-electric-backspace
                       c-electric-backspace-kill))
      (add-to-list 'ac-trigger-commands-on-completing command))
    (setq ac-sources (append '(ac-source-yasnippet
                               ;; ac-source-gtags
                               ac-source-semantic
                               ac-source-imenu) ac-sources))
    ;; firstly compile clang trunk: http://mike.struct.cn/blogs/entry/15/
    (when (executable-find "clang")
      (require 'auto-complete-clang)
      (add-to-list 'ac-sources 'ac-source-clang))
    )

  ;; python
  (defun ac-python-mode-setup ()
    (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))
  (add-hook 'python-mode-hook 'ac-python-mode-setup)

  ;; Org
  (defun ac-org-mode-setup ()
    (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))
  (add-hook 'org-mode-hook 'ac-org-mode-setup)

  ;; Slime
  (defun ac-slime-candidates ()
    "Complete candidates of the symbol at point."
    (if (memq major-mode '(lisp-mode))
        (let* ((end (point))
               (beg (slime-symbol-start-pos))
               (prefix (buffer-substring-no-properties beg end))
               (result (slime-simple-completions prefix)))
          (destructuring-bind (completions partial) result
            completions))))

  (ac-define-source slime
    '((candidates . ac-slime-candidates)
      (requires . 3)
      (symbol . "s")))

  (defun ac-slime-setup ()
    (slime-mode t)
    (push 'ac-source-slime ac-sources))

  (add-hook 'lisp-mode-hook 'ac-slime-setup)

  ;; for autopair
  (defun ac-settings-4-autopair ()
    "`auto-complete' settings for `autopair'."
    (defun ac-trigger-command-p (command)
      "Return non-nil if `this-command' is a trigger command."
      (or
       (and
        (symbolp command)
        (or (memq command ac-trigger-commands)
            (string-match "self-insert-command" (symbol-name command))
            (string-match "electric" (symbol-name command))
            (let* ((autopair-emulation-alist nil)
                   (key (this-single-command-keys))
                   (beyond-autopair (or (key-binding key)
                                        (key-binding (lookup-key local-function-key-map key)))))
              (or
               (memq beyond-autopair ac-trigger-commands)
               (and ac-completing
                    (memq beyond-autopair ac-trigger-commands-on-completing)))))))))
  (eval-after-load "autopair"
    '(ac-settings-4-autopair))
  )

(deh-require 'yasnippet
  (setq yas/root-directory my-snippet-dir)
  (yas/load-directory yas/root-directory)
  ;; (yas/initialize)     ;; enable yas/minor-mode globally
  (yas/global-mode 1)

  (setq yas/wrap-around-region t)

  (require 'dropdown-list)
  (setq yas/prompt-functions '(yas/dropdown-prompt
                               yas/ido-prompt
                               yas/completing-prompt))

  ;; FOR `hippie-try-expand' setting
  (add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)

  ;; FOR `auto-complete-mode', so disable default yasnippet expand action
  (if (fboundp 'auto-complete-mode)
      (progn
        ;; (setq yas/trigger-key nil) ; deperecated tweak
        (define-key yas/keymap (kbd "<right>") 'yas/next-field-or-maybe-expand)
        (define-key yas/keymap (kbd "<left>") 'yas/prev-field)))

  ;; List all snippets for current mode
  (define-key yas/minor-mode-map (kbd "C-c y") 'yas/insert-snippet)

;;;###autoload
  (defun yasnippet-reload-after-save ()
    (let* ((bfn (expand-file-name (buffer-file-name)))
           (root (expand-file-name yas/root-directory)))
      (when (string-match (concat "^" root) bfn)
        (yas/load-snippet-buffer)))) )

(deh-require 'autopair
  ;; It's not an ideal way to turn on autopair-global-mode, because it's
  ;; unstable and its keybinds often works in unexcepted manner.
  (deh-add-hooks (java-mode-hook
                  sh-mode-hook
                  c-mode-common-hook
                  python-mode-hook
                  emacs-lisp-mode-hook
                  html-mode-hook)
    (autopair-mode 1))
  ;; some tricks
  (deh-add-hook c++-mode-hook
    (push ? (getf autopair-dont-pair :comment))
    ;; (push '(?< . ?>) (getf autopair-extra-pairs :code))
    )
  (deh-add-hook emacs-lisp-mode-hook
    (push '(?` . ?') (getf autopair-extra-pairs :comment))
    (push '(?` . ?') (getf autopair-extra-pairs :string))) )

(deh-require-reserved 'template-simple
  (setq template-directory-list (list my-template-dir)
        template-skip-directory-list (list my-temp-dir my-template-dir))
  (defadvice ido-find-file (after ido-file-file-template activate)
    (funcall 'template-auto-insert))
  )

(deh-section "isearch"
  (deh-define-key isearch-mode-map
    ("\t" . 'isearch-complete)
    ("\M-<" . 'isearch-beginning-of-buffer)
    ("\M->" . 'isearch-end-of-buffer)
    ("\M-i" . 'isearch-query-replace-current)
    ("\C-u" . 'isearch-clean)
    ("\C-\M-y" . 'isearch-yank-symbol-regexp)
    ("\C-y" . 'isearch-yank-symbol) ; instead of `isearch-yank-line'
    ;; Remind other useful keybinds
    ;; ("\M-e" . 'isearch-edit-string)
    ;; ("\M-y" . 'isearch-yank-kill)
    ;; ("\M-r" . 'isearch-toggle-regexp)
    )

  (defun isearch-beginning-of-buffer ()
    "Move isearch point to the beginning of the buffer."
    (interactive)
    (goto-char (point-min))
    (isearch-repeat-forward))
  (defun isearch-end-of-buffer ()
    "Move isearch point to the end of the buffer."
    (interactive)
    (goto-char (point-max))
    (isearch-repeat-backward))
  (defun isearch-query-replace-current ()
    "Replace current searching string."
    (interactive)
    (let ((case-fold-search isearch-case-fold-search)
          (from-string isearch-string))
      (if (string= from-string "") (isearch-update)
        (if (not isearch-success)
            (progn (message "Search string not found")
                   (sleep-for 0.5) (isearch-update))
          (progn (isearch-done)
                 (goto-char (min (point) isearch-other-end)))
          (perform-replace
           from-string
           (read-from-minibuffer
            (format "Query replace %s with: " from-string)
            "" nil nil query-replace-to-history-variable from-string t)
           t isearch-regexp nil)))))
  (defun isearch-clean ()
    "Clean string in `iserch-mode'."
    (interactive)
    (goto-char isearch-opoint)
    (let ((isearch-command
           (if isearch-forward
               (if isearch-regexp 'isearch-forward-regexp 'isearch-forward)
             (if isearch-regexp 'isearch-backward-regexp 'isearch-backward))))
      (call-interactively isearch-command)))
  (defun isearch-yank-symbol-regexp ()
    "Put symbol at current point into isearch string, and do regexp isearch."
    (interactive)
    (let ((sym (symbol-at-point)))
      (if sym
          (progn
            (setq isearch-regexp t
                  isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                  isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                  isearch-yank-flag t))
        (ding)))
    (isearch-search-and-update))
  (defun isearch-yank-symbol ()
    "Put current symbol into search string."
    (interactive)
    (save-excursion
      (re-search-backward "[^[:alnum:]-_@.]" nil t)
      (forward-char)
      (isearch-yank-internal
       (lambda ()
         (re-search-forward "[[:alnum:]-_@.]*[[:alnum:]_]" nil t)))))

;;   (defadvice isearch-repeat (after isearch-no-fail activate)
;;     "When Isearch fails, it immediately tries again with
;; wrapping. Note that it is important to temporarily disable this
;; defadvice to prevent an infinite loop when there are no matches."
;;     (unless isearch-success
;;       (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
;;       (ad-activate 'isearch-repeat)
;;       (isearch-repeat (if isearch-forward 'forward))
;;       (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
;;       (ad-activate 'isearch-repeat)))
  )

(deh-require 'midnight
  (setq midnight-mode t
        clean-buffer-list-delay-general 2 ; delete after two days
        ;; clean-buffer-list-kill-never-buffer-names '("*scratch*"
        ;;                                             "*Messages*"
        ;;                                             "*server*")
        clean-buffer-list-kill-never-regexps '("^ \\*Minibuf-.*\\*$"
                                               "^ \\*MULTI-TERM-.*")
        clean-buffer-list-kill-regexps '("^ \\*Customize.*")
        ))

;;; Enhanced terminal
(deh-section "multi-term"
  (setq multi-term-dedicated-window-height 10
        multi-term-dedicated-max-window-height 10)

  (eval-after-load "multi-term"
    '(progn
       ;; compatible with normal terminal keybinds
       (add-to-list 'term-bind-key-alist '("<M-backspace>" . term-send-backward-kill-word))
       (add-to-list 'term-bind-key-alist '("<C-backspace>" . term-send-backward-kill-word))
       (add-to-list 'term-bind-key-alist '("M-DEL" . term-send-backward-kill-word))
       (add-to-list 'term-bind-key-alist '("<backspace>" . term-send-backspace))
       (add-to-list 'term-bind-key-alist '("C-d" . term-send-del))
       (add-to-list 'term-bind-key-alist '("<delete>" . term-send-del))
       (add-to-list 'term-bind-key-alist '("M-d" . term-send-forward-kill-word))
       (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))
       (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
       (add-to-list 'term-bind-key-alist '("C-y" . term-paste))
       ;; Only close dedicated window
       (add-to-list 'term-bind-key-alist '("C-q" . multi-term-dedicated-close))
       ;; unbind keys
       (setq term-unbind-key-list (append term-unbind-key-list '("C-v" "M-v")))

       ;; hack to backward kill word as it does in terminal
       (defun term-send-backward-kill-word ()
         "Backward kill word in term mode."
         (interactive)
         (term-send-raw-string "\e\C-?"))))

  (defun multi-term-dedicated-open-select ()
    (interactive)
    (unless (multi-term-dedicated-exist-p)
      (multi-term-dedicated-open))
    (multi-term-dedicated-select))

  (defun my-toggle-multi-term ()
    "Toggle dedicated `multi-term' window and select."
    (interactive)
    (if (multi-term-dedicated-exist-p)
        (multi-term-dedicated-close)
      (multi-term-dedicated-open-select)))
  )

(deh-section "shell"
  (setenv "HISTFILE" (expand-file-name "shell.history" my-temp-dir))
  (defun wcy-shell-mode-kill-buffer-on-exit (process state)
    "Auto save command history and kill buffers when exit ibuffer."
    (shell-write-history-on-exit process state)
    (kill-buffer (process-buffer process)))
  (defun ywb-shell-mode-hook ()
    (rename-buffer  (concat "*shell: " default-directory "*") t)
    (set-process-sentinel (get-buffer-process (current-buffer))
                          #'wcy-shell-mode-kill-buffer-on-exit)

    (ansi-color-for-comint-mode-on)
    (setq-default
     comint-dynamic-complete-functions
     (let ((list (default-value 'comint-dynamic-complete-functions)))
       (add-to-list 'list 'shell-dynamic-complete-command t)))
    )
  (add-hook 'shell-mode-hook 'ywb-shell-mode-hook)

  ;; shell-completion
  (deh-try-require 'shell-completion
    (setq shell-completion-sudo-cmd "\\(?:sudo\\|which\\)")
    (defvar my-lftp-sites (if (file-exists-p "~/.lftp/bookmarks")
                              (shell-completion-get-file-column "~/.lftp/bookmarks" 0 "[ \t]+")))
    (add-to-list 'shell-completion-options-alist
                 '("lftp" my-lftp-sites))
    (add-to-list 'shell-completion-prog-cmdopt-alist
                 '("lftp" ("help" "open" "get" "mirror" "bookmark")
                   ("open" my-lftp-sites)
                   ("bookmark" "add")))))

;; browse-kill-ring
(deh-section "browse-kill-ring"
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-highlight-current-entry t))

;; fold content
;; (deh-require 'fold
;;   (setq fold-mode-prefix-key "\C-c\C-o")
;;   (setq fold-autoclose-other-folds nil)
;;   (add-hook 'find-file-hook 'fold-find-file-hook t))

;; (deh-section "linum"
;;   (setq linum-format (concat (propertize "%6d " 'face 'default)
;;                              (propertize " " 'face 'fringe)))
;;   (autoload 'linum-mode "linum" "Display line number" t))

(deh-section "anything"

  (eval-after-load "anything"
    '(deh-define-key anything-map
       ("\C-n" . 'anything-next-line)
       ("\C-p" . 'anything-previous-line)
       ("\M-n" . 'anything-next-source)
       ("\M-p" . 'anything-previous-source)))

  ;; redefine anything-command-map-prefix-key
  (setq anything-command-map-prefix-key "")

  (eval-after-load "anything-config"
    '(progn
       (setq anything-c-adaptive-history-file
             (expand-file-name "anything-c-adaptive-history" my-temp-dir)
             anything-c-yaoddmuse-cache-file
             (expand-file-name "yaoddmuse-cache.el" my-temp-dir))
       (setq anything-c-find-files-show-icons t
             ;; anything-c-external-programs-associations nil
             anything-c-google-suggest-url "http://www.google.com/complete/search?output=toolbar&q="
             ;; anything-google-suggest-use-curl-p t
             anything-kill-ring-threshold 50
             anything-su-or-sudo "sudo")

       (defun anything-info-pages ()
         "Preconfigured anything for info pages."
         (interactive)
         (anything-other-buffer 'anything-c-source-info-pages "*info pages*"))
       )))

;;; Navigate buffer
(deh-section "speedbar"
  ;;# speedbar in one frame
  (require 'sr-speedbar)
  (defun my-toggle-sr-speedbar ()
    "Toggle sr speedbar window."
    (interactive)
    (sr-speedbar-toggle) (sr-speedbar-select-window))
  ;; (global-set-key (kbd "M-9") 'sr-speedbar-select-window)

  (deh-define-key speedbar-key-map
    ("j" . 'speedbar-next)
    ("k" . 'speedbar-prev)
    ("\M-u" . 'speedbar-up-directory))
  (deh-define-key speedbar-file-key-map
    ((kbd "RET") . 'speedbar-toggle-line-expansion)) ; SPC

  (setq speedbar-directory-unshown-regexp
        "^\\(CVS\\|RCS\\|SCCS\\|\\.bak\\|\\..*\\)\\'")

  ;; add supported extensions
  (dolist (ext (list ".php" ".js" ".css" ".xml"
                     ".sh"
                     ".txt" ".org" ".md" "README"
                     ".pdf" ".doc"
                     ".jpg" ".png"))
    (speedbar-add-supported-extension ext))
  ;; (setq speedbar-show-unknown-files t)

  (add-to-list 'speedbar-fetch-etags-parse-list
               '("\\.php" . speedbar-parse-c-or-c++tag))

  (setq sr-speedbar-skip-other-window-p t
        ;; sr-speedbar-delete-windows t
        sr-speedbar-width-x 22
        sr-speedbar-max-width 30)

  ;; WORKAROUND: shortkey cofflict, disable view-mode in speedbar
  (setq speedbar-mode-hook '(lambda () (View-exit))))

(deh-section-after "hideshow"
  (deh-define-key hs-minor-mode-map
    ("\C-chh" . 'hs-hide-block)
    ("\C-chs" . 'hs-show-block)
    ("\C-chH" . 'hs-hide-all)
    ("\C-chS" . 'hs-show-all)
    ("\C-cht" . 'hs-toggle-hiding)
    ((kbd "<left-fringe> <mouse-2>") . 'hs-mouse-toggle-hiding))

  (defvar hs--overlay-keymap nil "keymap for folding overlay")
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'hs-show-block)
    (setq hs--overlay-keymap map))
  (setq hs-set-up-overlay
        (defun my-display-code-line-counts (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (overlay-put ov 'display
                         (propertize
                          (format "...<%d lines>"
                                  (count-lines (overlay-start ov)
                                               (overlay-end ov)))
                          'face 'mode-line))
            (overlay-put ov 'priority (overlay-end ov))
            (overlay-put ov 'keymap hs--overlay-keymap)
            (overlay-put ov 'pointer 'hand)))))

(deh-section-after "outline"
  (setq outline-minor-mode-prefix (kbd "C-c o"))
  (deh-define-key outline-minor-mode-map
    ("\C-cos" . 'show-subtree)
    ("\C-coS" . 'show-all)
    ("\C-coh" . 'hide-subtree)
    ("\C-coH" . 'hide-body)
    ;; shortcuts
    ((kbd "<right>") . 'show-subtree)
    ((kbd "<M-right>") . 'show-all)
    ((kbd "<left>") . 'hide-subtree)
    ((kbd "<M-left>") . 'hide-body)
    ((kbd "<up>") . 'outline-previous-heading)
    ((kbd "<down>") . 'outline-next-heading)
    ((kbd "<M-up>") . 'outline-previous-visible-heading)
    ((kbd "<M-down>") . 'outline-next-visible-heading)
    ;; xwl keybinds
    ("\C-con" . 'xwl-narrow-to-outline-level)
    ("\C-cou" . 'xwl-outline-toggle-enter-exit)
    ("\C-coq" . 'xwl-outline-toggle-show-hide))

  (defadvice outline-mode (after hide-sublevels)
    "Enter overview after start up `outline-mode'."
    (hide-sublevels 1))

  (defadvice outline-minor-mode (after hide-sublevels)
    "Enter overview after start up `outline-minor-mode'."
    (hide-sublevels 2))

  (setq outline-font-lock-keywords
        '((eval list
                (concat "^\\(?:" outline-regexp "\\).+")
                0
                '(outline-font-lock-face)
                nil t)))

  (eval-after-load "outline" '(require 'foldout))

  ;; keys
  (defun xwl-hide-body ()
    "Make `hide-body' take effects at any moment."
    (interactive)
    (show-all)
    (hide-body))

  (defun xwl-outline-invisible-p ()
    "Are we inside a outline fold?"
    (interactive)
    (let ((overlays (overlays-at (line-end-position))))
      (and overlays
           (eq (overlay-get (car overlays) 'invisible)
               'outline))))

  (defun xwl-foldout-exit-fold ()
    "Goto current folded line."
    (interactive)
    (call-interactively 'foldout-exit-fold) ; FIX ME
    (previous-line 1)
    (next-line 1))

  (defun xwl-outline-toggle-enter-exit ()
    "Toggle entering and exiting fold."
    (interactive)
    (if (xwl-outline-invisible-p)
        (foldout-zoom-subtree)
      (xwl-foldout-exit-fold)))

  (defun xwl-outline-toggle-show-hide ()
    "Toggle showing or hiding contents."
    (interactive)
    (if (xwl-outline-invisible-p)
        (show-subtree)
      (hide-subtree)))

  (defun xwl-narrow-to-outline-level ()
    "Narrow to current outline level."
    (interactive)
    (save-excursion
      (call-interactively 'outline-next-visible-heading)
      (let ((end (point)))
        (call-interactively 'outline-previous-visible-heading)
        (narrow-to-region (point) end))))
  )

(deh-require 'which-func
  (add-to-list 'which-func-modes 'org-mode)
  (which-func-mode 1))

(deh-section-after "imenu"
  (add-to-list 'imenu-after-jump-hook 'recenter)
  (setq imenu-max-item-length 60
        imenu-max-items 500))

(deh-section "ediff"
  ;; (global-set-key "\C-cd" 'ediff-show-registry)
  (setq diff-switches "-ubB"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

;;; highlight
(deh-require 'highlight-parentheses
  ;; colors is applied by reversed order
  (setq hl-paren-colors
        '("orange1" "yellow1" "greenyellow" "green1"
          "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))
  (deh-add-hooks (emacs-lisp-mode-hook
                  c-mode-common-hook)
    (highlight-parentheses-mode 1)
    ;; compatible with autopair-mode
    (eval-after-load "autopair"
      '(progn
         (setq autopair-handle-action-fns
               (append (if (boundp 'autopair-handle-action-fns)
                           autopair-handle-action-fns
                         '(autopair-default-handle-action))
                       '((lambda (action pair pos-before)
                           (hl-paren-color-update)))))))
    ))

(deh-section "highlight-line"
  ;; (global-hl-line-mode 1)
  (setq hl-line-face 'underline)
  ;; (set-face-background 'hl-line "white smoke") ; list-colors-display
  )

(deh-require 'highlight-symbol
  (deh-add-hooks (emacs-lisp-mode-hook
                  java-mode-hook
                  c-mode-common-hook)
    (when window-system
      (highlight-symbol-mode 1)
      (setq highlight-symbol-idle-delay 0.5
            highlight-symbol-mode nil)))
  )

(deh-section "hi-lock"
  (setq hi-lock-file-patterns-range 5000
        hi-lock-file-patterns-policy '(lambda (dummy) t)))

;;; others

;; ;; erc
;; (deh-section "erc"
;;   (setq erc-log-channels-directory (expand-file-name "erc" my-temp-dir))
;;   (eval-after-load "erc"
;;     '(deh-require 'emoticons
;;        (add-hook 'erc-insert-modify-hook 'emoticons-fill-buffer)
;;        (add-hook 'erc-send-modify-hook 'emoticons-fill-buffer)
;;        (add-hook 'erc-mode-hook
;;                  (lambda ()
;;                    (eldoc-mode t)
;;                    (setq eldoc-documentation-function 'emoticons-help-echo))))))

(deh-section-after "epa-file"
  (setq epa-file-cache-passphrase-for-symmetric-encryption t))

(deh-require-reserved 'gmail-notifier
  ;;# set user/passwd in ~/.authinfo.gpg
  (file-exists-p "~/.authinfo")
  (gmail-notifier-start)
  )
