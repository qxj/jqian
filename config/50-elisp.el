;; -*- mode: Emacs-Lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end


(deh-section "std-lib"
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
  (autoload 'minibuf-isearch-next "minibuf-isearch" "" t)
  (autoload 'minibuf-isearch-prev "minibuf-isearch" "" t)

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
    ([return]  'dired-find-file-single-buffer)
    ("\M-u"  'dired-up-directory)   ; remember previous upper directory
    ;; ("\M-="  'dired-backup-diff)
    ("b"     'browse-url-of-dired-file)
    ("W"     'woman-dired-find-file)
    ("r"     'wdired-change-to-wdired-mode) ; editable mode, 'C-c C-k' abort
    (" "     'dired-count-directory-size)
    ("E"     'dired-w3m-visit)
    ;; ("!"     'dired-do-shell-command)
    ("z"     'dired-compress-directory)
    ("s"     'one-key-menu-dired-sort)
    ("/"     'one-key-menu-dired-filter))

  ;;# hooks
  (deh-add-hook 'dired-mode-hook
    (dired-omit-mode t))
  (deh-add-hook 'dired-load-hook
    (load "dired-x")
    ;; Make the execuatable file with different color
    (add-to-list 'dired-font-lock-keywords
                 (list dired-re-exe
                       '(".+" (dired-move-to-filename) nil (0 font-lock-type-face))) t))
  (deh-add-hook 'dired-after-readin-hook
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
  (dolist (ext '(".bak" ".dSYM"))
    (add-to-list 'dired-omit-extensions ext))

  (setq dired-omit-files
        (concat "^[.#]" "\\|"
                "^" (regexp-opt '("TAGS" "GPATH" "GRTAGS" "GSYMS" "GTAGS") t) "$"))

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
  (when (eq system-type 'windows-nt)
    (defun dired-custom-execute-file (&optional arg)
      (interactive "P")
      (mapcar #'(lambda (file)
                  (w32-shell-execute "open" (convert-standard-filename file)))
              (dired-get-marked-files nil arg))))

  ;; Redefine of this function
  (defun dired-run-shell-command (command)
    "Replace `shell-command' to `start-process-shell-command' to run command asynchronously. Originally defined in dired-aux.el"
    (let ((handler
           (find-file-name-handler (directory-file-name default-directory)
                                   'shell-command)))
      (if handler
          (apply handler 'shell-command (list command))
        (start-process-shell-command "dired-run" nil command)))
    ;; Return nil for sake of nconc in dired-bunch-files.
    nil))

(deh-require 'ido
  ;; (ido-mode 1) ;; avoid recursive tramp load error, it's a reported bug
  (ido-everywhere t)
  (add-hook 'term-setup-hook 'ido-mode)

  (setq ido-enable-regexp nil           ; C-t, M-x ido-toggle-regexp
        ido-enable-dot-prefix t
        ido-enable-flex-matching t
        ido-enable-tramp-completion nil
        ido-record-ftp-work-directories nil
        ;; ido-enable-last-directory-history nil ; avoid tramp connect when start
        ido-use-faces t
        ;; ido-use-filename-at-point 'guess
        ;; ido-use-url-at-point t
        ido-create-new-buffer 'always
        ido-default-file-method 'selected-window
        ido-ignore-extensions t         ; refer to `completion-ignored-extensions'
        ido-auto-merge-work-directories-length -1
        ido-max-work-file-list 20)

  (setq ido-save-directory-list-file
        (expand-file-name "emacs.ido-last" my-data-dir)
        org-id-locations-file
        (expand-file-name "emacs.ido-locations" my-data-dir))
  (setq ido-ignore-buffers
        '("^ " "_region_" "TAGS"
          (lambda (buf)
            (with-current-buffer buf
              (or
               ;; ignore dired-mode
               (eq (buffer-local-value 'major-mode (current-buffer)) 'dired-mode)
               (and
                ;; exclude *scratch*, *info*
                (not (member-ignore-case (buffer-name) '("*scratch*" "*info*")))
                ;; exclude *tumblr ...*
                (not (string-match "^\\*tumblr.+" (buffer-name)))
                (string-match "^\\*.+" (buffer-name)))))))
        ido-ignore-directories
        '("^auto/" "^CVS/" "^\\.")
        ido-ignore-files
        '("^[.#]"
          "~$" "\\.\\(log\\|out\\)$"
          "\\(TAGS\\|GPATH\\|GSYMS\\)")
        ido-work-directory-list-ignore-regexps
        `(,tramp-file-name-regexp))
  (setq ido-file-extensions-order
        '(".h" ".c" ".cpp" ".py" ".sh" ".el" ".txt" ".org" ".md"))

  (deh-add-hook ido-setup-hook
    (deh-define-key ido-completion-map
      ((kbd "C-n")    'ido-next-match-dir)
      ((kbd "C-p")    'ido-prev-match-dir)
      ((kbd "M-u")    'ido-up-directory)
      ((kbd "C-M-h")  'ido-goto-home)
      ((kbd "C-u")    'ido-clean-text)
      ((kbd "C-w")    'ido-delete-backward-word-updir)
      ;; Remind keybinds
      ;; ((kbd "C-a")  'ido-toggle-ignore)
      ((kbd "C-S-p")  'ido-toggle-prefix)
      ;; ((kbd "C-t")  'ido-enable-regexp)
      ;; ((kbd "M-n")  'ido-next-work-directory)
      ;; ((kbd "M-p")  'ido-prev-work-directory)
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
        (if allcomp                 ; only ido for string list, but not alist/hash-table
            (setq ad-return-value
                  (ido-completing-read prompt
                                       allcomp
                                       nil require-match initial-input hist def))
          ad-do-it))))

  ;; HACK: avoid `ido-dir-file-cache' include tramp file
  (defun ido-may-cache-directory (&optional dir)
    (setq dir (or dir ido-current-directory))
    (cond
     ((ido-directory-too-big-p dir)
      nil)
     ((and (ido-is-root-directory dir)
           (or ido-enable-tramp-completion
               (memq system-type '(windows-nt ms-dos))))
      nil)
     ((ido-is-unc-host dir)
      (ido-cache-unc-valid))
     ((ido-is-ftp-directory dir)
      (ido-cache-ftp-valid))
     ;; hacking
     ((string-match "^/[^/:]+:[^/:]+@[^/:]+:" dir)
      nil)
     (t t)))

  ;; push the most used directory to `ido-work-directory-list'
  (mapc (lambda (dir)
          (add-to-list 'ido-work-directory-list
                       (expand-file-name dir)))
        '("~/.emacs.d/config/"
          "~/.emacs.d/site-lisp/"
          "~/projects/"
          "~/works/"
          "~/temp/"
          "~/bin/"
          "~/")))

(deh-require 'smex
  (setq smex-save-file (expand-file-name "emacs.smex-items" my-data-dir)
        smex-history-length 50)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "C-c M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(deh-section-after "ibuffer"
  (setq ibuffer-old-time 24
        ibuffer-show-empty-filter-groups nil)
  ;; keybinds
  ;; (global-set-key (kbd "C-x C-b") 'ibuffer)
  (deh-define-key ibuffer-mode-map
    ("s"  'one-key-menu-ibuffer-sort)
    ("r"  'ibuffer-rename-buffer)
    ("\C-x\C-f"  'ibuffer-find-file)
    (" "  'scroll-up))

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

  (deh-add-hook 'ibuffer-mode-hook
    (require 'ibuf-ext nil t)
    (ibuffer-switch-to-saved-filter-groups "default"))

  (deh-after-load "ibuf-ext"
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
             ("*others*" (name . "\\*.*\\*"))))))
  )

(deh-require 'uniquify
  (setq uniquify-buffer-name-style 'forward
        ;; uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
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
  (setq tramp-auto-save-directory my-data-dir
        tramp-persistency-file-name (expand-file-name "tramp" my-data-dir)
        tramp-default-method "ssh"
        remote-file-name-inhibit-cache 60
        ;; tramp-syntax 'url
        password-cache-expiry nil)
  ;;# avoid to backup tramp files
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))

  ;; tramp-compile is deperated, which is integrated into compile
  ;; command.
  )

;;; Session management
(deh-section-after "bookmark"
  ;; autosave bookmark into the diskete
  (setq bookmark-default-file (expand-file-name "emacs.bookmark" my-data-dir)
        bookmark-save-flag 1)
  (add-to-list 'bookmark-after-jump-hook 'recenter)
  (deh-add-hook 'bookmark-bmenu-mode-hook
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
        desktop-path (list my-data-dir)
        desktop-restore-eager 8        ; firstly restore 8 buffers
        history-length 100)

  ;;# not to save
  (setq desktop-globals-to-save
        (delq nil (mapcar (lambda (x) (if (memq x '(tags-table-list
                                                    file-name-history)) nil x))
                          desktop-globals-to-save)))
  (setq desktop-buffers-not-to-save
        (concat "\\(" "\\.log\\|\\.diary\\|\\.elc" "\\)$"))
  (dolist (mode '(dired-mode info-lookup-mode fundamental-mode))
    (add-to-list 'desktop-modes-not-to-save mode))

  ;;# to save
  (add-to-list 'desktop-globals-to-save 'kill-ring)
  (if (boundp 'windata-name-winconf)
      (add-to-list 'desktop-globals-to-save 'windata-named-winconf))
  (if (boundp 'smex-history)
      (add-to-list 'desktop-globals-to-save 'smex-history))

  ;; if error occurred, no matter it!
  ;; (condition-case nil
  ;;     (desktop-read)
  ;;   (error nil))
  (desktop-save-mode 1)

  ;;# persist desktop into file every 10 mins
  (run-with-idle-timer 600 600 'desktop-save-in-desktop-dir)

  ;; desktop-menu.el can store many desktops, it works besides
  ;; desktop.el and its settings don't cofflict with desktop.el, so
  ;; please don't mix up `desktop-base-file-name' and
  ;; `desktop-menu-base-filename'.
  ;;
  (deh-require 'desktop-menu
    (setq desktop-menu-directory my-data-dir
          desktop-menu-base-filename (concat "emacs.desktops-" (system-name))
          desktop-menu-list-file "emacs.desktops"
          desktop-menu-autosave 600     ; auto save every 10mins and when exit
          desktop-menu-clear 'ask)
    ;; customize some standard `desktop' variables
    (setq desktop-load-locked-desktop t)

    ;;# save individual bm repository with different desktops
    (deh-after-load "bm"
      (defun bm-repository-file-of-desktop-menu ()
        (let ((current-desktop (cdr desktop-menu--current-desktop)))
          (if current-desktop
              (setq bm-repository-file
                    (concat current-desktop ".bm-repo")))))
      (deh-add-hook 'desktop-after-read-hook
        (if (bm-repository-file-of-desktop-menu) (bm-repository-load)))
      (deh-add-hook 'desktop-save-hook
        (when (bm-repository-file-of-desktop-menu)
          (bm-buffer-save-all)
          (bm-repository-save))))
    ))

(deh-require-reserved 'session
  (setq session-save-file (expand-file-name "emacs.session" my-data-dir))
  (setq session-save-file-coding-system 'utf-8-unix)
  (add-to-list 'session-globals-exclude 'org-mark-ring)
  (add-hook 'after-init-hook 'session-initialize))

(deh-require 'saveplace
  (setq save-place-file (expand-file-name "emacs.saveplace" my-data-dir))
  (setq-default save-place t))

(deh-require 'savehist
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "emacs.savehist" my-data-dir))
  (savehist-mode t))

(deh-require 'bm
  (setq bm-cycle-all-buffers t
        bm-highlight-style (if window-system
                               'bm-highlight-line-and-fringe
                             'bm-highlight-line)
        bm-restore-repository-on-load t)
  (setq-default bm-buffer-persistence t)

  (setq bm-repository-file
        (expand-file-name "emacs.bm-repository" my-data-dir))

  ;; buffer setting
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)

  ;; for persistent bookmarks
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))

  ;; mouse setting
  (global-set-key [left-margin mouse-2] 'bm-toggle-mouse)
  (global-set-key [left-margin mouse-3] 'bm-next-mouse)

  (deh-define-key bm-show-mode-map
    ("n"  'bm-show-next)
    ("p"  'bm-show-prev)))

;; recent-jump
(deh-require 'recent-jump)

;; recent opened files
(deh-section "recentf"
  ;; recent finded buffers
  (setq recentf-max-saved-items 1000
        recentf-save-file (expand-file-name "emacs.recentf" my-data-dir)
        recentf-exclude `(,my-data-dir
                          ,tramp-file-name-regexp))
  (recentf-mode t)

  (defun recentf-open-files-compl ()
    "Open files opened recently with `ido-completing-read'."
    (interactive)
    (let (elist)
      (dolist (item recentf-list)
        (let* ((el (if (file-directory-p item)
                       (concat (basename item) "/") ; flag directories with a slash
                     (basename item)))
               (collided-item (cdr (assoc el elist)))
               collided-el)
          ;; distinguish collided file names
          (when collided-item
            (let ((len (directory-depth item))
                  (collided-len (directory-depth collided-item))
                  (slices (split-file-name item))
                  (collided-slices (split-file-name collided-item))
                  (idx 0)
                  found-unique)
              (while (and (not found-unique)
                          (< idx (min len collided-len)))
                (let ((slice (nth idx slices))
                      (collided-slice (nth idx collided-slices)))
                  (if (string= slice collided-slice)
                      (incf idx)
                    (setq found-unique t)
                    (remove-from-list 'elist el)
                    (setq collided-el
                          (if (= idx (1- collided-len))
                              el
                            (format "%s/%s" collided-slice el)))
                    ;; (setq elist (delq (cons el item) elist))
                    (add-to-list 'elist (cons collided-el collided-item))
                    (setq el
                          (if (= idx (1- len))
                              el
                            (format "%s/%s" slice el))))))))
          (unless (zerop (length el))
            (add-to-list 'elist (cons el item)))))
      ;; use `ido-completing-read' instead of `completing-read'
      (find-file (cdr (assoc (ido-completing-read "Open file: "
                                                  (mapcar 'car elist))
                             elist)))))

  (deh-after-load "recentf"
    ;; Also store recent opened directories besides files
    (deh-add-hook 'dired-mode-hook
      (recentf-add-file dired-directory))))

(deh-require 'pager
  (deh-define-key global-map
    ((kbd "C-v")  'pager-page-down)
    ((kbd "M-v")  'pager-page-up)
    ((kbd "<up>") 'pager-row-up)
    ((kbd "M-p")  'pager-row-up)
    ((kbd "<down>") 'pager-row-down)
    ((kbd "M-n")  'pager-row-down))
  ;; Some individual keybind overrides
  (deh-after-load "info"
    (deh-define-key Info-mode-map
      ((kbd "M-p") 'pager-row-up)
      ((kbd "M-n") 'pager-row-down)))
  (deh-after-load "man"
    (deh-define-key Man-mode-map
      ((kbd "M-p") 'pager-row-up)
      ((kbd "M-n") 'pager-row-down)))
  (deh-after-load "woman"
    (deh-define-key woman-mode-map
      ((kbd "M-p") 'pager-row-up)
      ((kbd "M-n") 'pager-row-down)))
  (deh-after-load "w3m"
    (deh-define-key w3m-mode-map
      ((kbd "M-p") 'pager-row-up)
      ((kbd "M-n") 'pager-row-down)))
  )

(deh-section "ffap"
  (autoload 'ffap "ffap" "Alias of find-file-at-point")

  ;; (ffap-bindings)

  ;; for windows path recognize
  (setq ffap-string-at-point-mode-alist
        '((file "--{}:\\\\$+<>@-Z_a-z~*?\x100-\xffff" "<@" "@>;.,!:")
          (url "--:=&?$+@-Z_a-z~#,%;*" "^A-Za-z0-9" ":;.,!?")
          (nocolon "--9$+<>@-Z_a-z~" "<@" "@>;.,!?")
          (machine "-a-zA-Z0-9." "" ".")
          (math-mode ",-:$+<>@-Z_a-z~`" "<" "@>;.,!?`:")))

  (deh-after-load "ffap"
    (setq ffap-c-path (append ffap-c-path my-include-dirs)))
  )

(deh-section "filecache"
  (deh-define-key minibuffer-local-map
    ((kbd "C-M-f")  'file-cache-minibuffer-complete))

  (setq file-cache-ignore-case t)

  (deh-after-load "filecache"
    (message "Loading file cache...")
    (file-cache-add-directory my-config-dir)
    (file-cache-add-directory-list load-path)
    (file-cache-add-directory-using-find (expand-file-name "~/works"))))

;;; Buffer view
(deh-section "windmove"
  (windmove-default-keybindings 'shift))

(deh-section-after "help-mode"
  (setq help-window-select t)
  (deh-define-key help-mode-map
    ((kbd "<left>")  'help-go-back)
    ((kbd "<right>") 'help-go-forward) ))

(deh-section-after "view"
  (setq view-read-only t)

  (deh-define-key view-mode-map
    ;; simulate vi keybinds
    ("h"  'backward-char)
    ("l"  'forward-char)
    ("j"  'next-line)
    ("k"  'previous-line)
    ("c"  'recenter-top-bottom)
    ("0"  'beginning-of-line)
    ("$"  'end-of-line)
    ("g"  'beginning-of-buffer)
    ("G"  'end-of-buffer)
    ("n"  'View-scroll-line-forward)
    ("p"  'View-scroll-line-backward)
    ((kbd "<backspace>")  'View-scroll-page-backward)
    ((kbd "SPC")  'View-scroll-page-forward)
    ("?"  'View-search-regexp-backward)
    ;; register
    ("m"  'point-to-register)
    ("'"  'register-to-point)
    ;; gtags
    ("."  'gtags-find-tag)
    (","  'gtags-pop-stack)
    ("i"  'gtags-find-tag)
    ("u"  'gtags-pop-stack)
    ;; sourcepair
    ("a"  'sourcepair-load)
    ;; eassist
    ("L"  'eassist-list-methods)
    ;; generic
    ("f"  'ido-find-file)
    ("d"  'dired-jump)
    ("o"  'my-switch-recent-buffer)
    ;; ("q"  'bury-buffer)
    ("q"   'View-quit)
    ("\C-k"  'kill-this-buffer)))

(deh-section "doc-view"
  (deh-add-hook 'doc-view-mode-hook
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
  (deh-add-hook image-mode-hook
    (deh-define-key image-mode-map "I" 'image-display-info)))

(deh-section-after "w3m"
  (setq w3m-verbose t                   ; log in *Messages*
        w3m-default-display-inline-images t)
  (deh-define-key w3m-mode-map
    ("n"  (lambda nil (interactive) (ywb-w3m-goto-url w3m-next-url)))
    ("p"  (lambda nil (interactive) (ywb-w3m-goto-url w3m-previous-url)))
    ("t"  (lambda nil (interactive) (ywb-w3m-goto-url w3m-contents-url))))
  (deh-add-hook 'w3m-load-hook
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
      (if url (browse-url url)
        (w3m-message "Invalid url."))))

  (define-mode-toggle "w3m" w3m
    (derived-mode-p 'w3m-mode))
  )

;;; Edit
(deh-section "occur"
  (deh-add-hook 'occur-mode-hook
    (require 'moccur-edit nil t)
    (setq truncate-lines t))

  ;; make cursor become a line
  ;; (require 'bar-cursor)

  (deh-after-load "moccur-edit"
    (defadvice moccur-edit-change-file (after save-after-moccur-edit-buffer activate)
      (save-buffer)))

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
    (moccur-word-all-buffers "\\<\\(FIXME\\|TODO\\)")
    )
  )

(deh-section "mark-multiple"
  (deh-try-require 'inline-string-rectangle
    (deh-define-key global-map
      ((kbd "C-x r t") 'inline-string-rectangle)))

  (deh-try-require 'mark-more-like-this
    (deh-define-key global-map
      ((kbd "C-<") 'mark-previous-like-this)
      ((kbd "C->") 'mark-next-like-this)
      ((kbd "C-M-m") 'mark-more-like-this)
      ((kbd "C-*") 'mark-all-like-this)))

  (deh-after-load "sgml-mode"
    (deh-try-require 'rename-sgml-tag
      (deh-define-key sgml-mode-map
        ((kbd "C-c C-r") 'rename-sgml-tag))))

  (deh-after-load "js2"
    (deh-try-require 'js2-rename-var
      (deh-define-key js2-mode-map
        ((kbd "C-c C-r") 'js2-rename-var)))))

(deh-require 'undo-tree
  (global-undo-tree-mode))

(deh-section "grep"
  (autoload 'grep-tag-default "grep")
  (autoload 'grep-apply-setting "grep")

  (deh-after-load "grep"
    (add-to-list 'grep-files-aliases '("hcpp" . "*.h *.c *.[hc]pp"))
    ;;# avoid print NUL when grep something in Windows.
    (if (eq system-type 'window-nt) (setq grep-use-null-device nil)))

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


(deh-section "isearch"
  (setq isearch-case-fold-search t)     ; case insensitive
  (deh-define-key isearch-mode-map
    ("\t"  'isearch-complete)
    ("\M-<"  'isearch-beginning-of-buffer)
    ("\M->"  'isearch-end-of-buffer)
    ("\M-i"  'isearch-query-replace-current)
    ("\C-u"  'isearch-clean)
    ("\C-\M-y"  'isearch-yank-symbol-regexp)
    ("\C-y"  'isearch-yank-symbol) ; instead of `isearch-yank-line'
    ("\C-o" 'isearch-occur)
    ;; Remind other useful keybinds
    ;; ("\M-e"  'isearch-edit-string)
    ;; ("\M-y"  'isearch-yank-kill)
    ;; ("\M-e" 'isearch-edit-string)
    ;; ("\M-c" 'isearch-toggle-case-fold)
    ;; ("\M-r" 'isearch-toggle-regexp)
    ;; ("\M-sr" 'isearch-toggle-regexp)
    ;; ("\M-sw" 'isearch-toggle-word)
    ;; ("\M-so" 'isearch-occur)
    ;; ("\M-shr" 'isearch-highlight-regexp)
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
      (re-search-backward "[^[:alnum:]-_@]" nil t)
      (forward-char)
      (isearch-yank-internal
       (lambda ()
         (re-search-forward "[[:alnum:]-_@]*[[:alnum:]_]" nil t)))))

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

(deh-require-reserved 'key-chord
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2
        key-chord-in-macros nil)
  ;; global key chords
  (key-chord-define-global ",." "<>\C-b")
  ;; key chord in c++-mode
  (deh-after-load "cc-mode"
    (key-chord-define c++-mode-map ",," "<<")
    (key-chord-define c++-mode-map ".." ">>"))
  (deh-after-load "python"
    (key-chord-define python-mode-map "''" "\"\"\"\"\"\"\C-b\C-b\C-b"))
  )


(deh-section "ace-jump-mode"
  (autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
  (autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)

  (eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))

  (deh-define-key global-map
    ((kbd "C-c SPC") 'ace-jump-mode)
    ((kbd "C-x SPC") 'ace-jump-mode-pop-mark)
    ((kbd "M-4") 'ace-jump-char-mode)
    ((kbd "C-4") 'ace-jump-mode)))

(deh-require 'iy-go-to-char
  (deh-define-key global-map
    ;; ((kbd "C-c f") 'iy-go-to-char)
    ;; ((kbd "C-c ;") 'iy-go-to-char-continue)
    ;; ((kbd "C-c F") 'iy-go-to-char-backward)
    ;; ((kbd "C-c ,") 'iy-go-to-char-continue-backward)
    ((kbd "M-3") 'iy-go-to-char)
    ((kbd "C-3") 'iy-go-to-char))

  (setq iy-go-to-char-key-forward ?\;
        iy-go-to-char-key-backward ?\,))

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
(deh-require 'multi-term
  (setq multi-term-dedicated-window-height 10
        multi-term-dedicated-max-window-height 10)

  (deh-after-load "multi-term"
    ;; compatible with normal terminal keybinds
    (add-to-list 'term-bind-key-alist '("<M-backspace>" . term-send-backward-kill-word))
    (add-to-list 'term-bind-key-alist '("<C-backspace>" . term-send-backward-kill-word))
    (add-to-list 'term-bind-key-alist '("M-DEL" . term-send-backward-kill-word))
    (add-to-list 'term-bind-key-alist '("<backspace>" . term-send-backspace))
    (add-to-list 'term-bind-key-alist '("C-d" . term-send-del))
    (add-to-list 'term-bind-key-alist '("<delete>" . term-send-del))
    (add-to-list 'term-bind-key-alist '("M-d" . term-send-forward-kill-word))
    (add-to-list 'term-bind-key-alist '("<tab>" . (lambda nil (interactive) (term-send-raw-string "\C-i"))))
    ;; some helpful key bindings
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
      (term-send-raw-string "\e\C-?")))

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
  (setenv "HISTFILE" (expand-file-name "shell.history" my-data-dir))

  (deh-add-hook 'shell-mode-hook
    (rename-buffer (concat "*shell: " default-directory "*") t)
    (ansi-color-for-comint-mode-on)
    (setq-default comint-dynamic-complete-functions
                  (let ((list (default-value 'comint-dynamic-complete-functions)))
                    (add-to-list 'list 'shell-dynamic-complete-command t)))
    ;;# Auto save command history and kill buffers when exit ibuffer.
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (process state)
                            (shell-write-history-on-exit process state)
                            (kill-buffer (process-buffer process)))))

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
(deh-require 'browse-kill-ring
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

(deh-section-reserved "anything"
  (autoload 'anything "anything" "" t)

  (deh-after-load "anything"
    (deh-define-key anything-map
      ("\C-n"  'anything-next-line)
      ("\C-p"  'anything-previous-line)
      ("\M-n"  'anything-next-source)
      ("\M-p"  'anything-previous-source)))

  ;; redefine anything-command-map-prefix-key
  (setq anything-command-map-prefix-key "")

  (deh-after-load "anything-config"
    (setq anything-c-adaptive-history-file
          (expand-file-name "anything-c-adaptive-history" my-data-dir)
          anything-c-yaoddmuse-cache-file
          (expand-file-name "yaoddmuse-cache.el" my-data-dir))
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
    ))

(deh-section "helm"
  ;; TODO: more helm setting
  ;; (deh-try-require 'helm-config)
  )

;;; Navigate buffer
(deh-section-after "speedbar"
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

  (deh-define-key speedbar-key-map
    ("j"  'speedbar-next)
    ("k"  'speedbar-prev)
    ("\M-u"  'speedbar-up-directory))
  (deh-define-key speedbar-file-key-map
    ((kbd "RET")  'speedbar-toggle-line-expansion)) ; SPC

  ;; WORKAROUND: shortkey cofflict, disable view-mode in speedbar
  (setq speedbar-mode-hook '(lambda () (View-exit))))

;; speedbar in one frame
(deh-require-reserved 'sr-speedbar
  (setq sr-speedbar-skip-other-window-p t
        ;; sr-speedbar-delete-windows t
        sr-speedbar-width-x 22
        sr-speedbar-max-width 30)

  (defun my-toggle-sr-speedbar ()
    "Toggle sr speedbar window."
    (interactive)
    (sr-speedbar-toggle) (sr-speedbar-select-window))
  )

(deh-section-after "hideshow"
  (deh-define-key hs-minor-mode-map
    ("\C-chh"  'hs-hide-block)
    ("\C-chs"  'hs-show-block)
    ("\C-chH"  'hs-hide-all)
    ("\C-chS"  'hs-show-all)
    ("\C-cht"  'hs-toggle-hiding)
    ((kbd "<left-fringe> <mouse-2>")  'hs-mouse-toggle-hiding))

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
    ("\C-cos"  'show-subtree)
    ("\C-coS"  'show-all)
    ("\C-coh"  'hide-subtree)
    ("\C-coH"  'hide-body)
    ;; shortcuts
    ((kbd "<right>")  'show-subtree)
    ((kbd "<M-right>")  'show-all)
    ((kbd "<left>")  'hide-subtree)
    ((kbd "<M-left>")  'hide-body)
    ((kbd "<up>")  'outline-previous-heading)
    ((kbd "<down>")  'outline-next-heading)
    ((kbd "<M-up>")  'outline-previous-visible-heading)
    ((kbd "<M-down>")  'outline-next-visible-heading)
    ;; xwl keybinds
    ("\C-con"  'xwl-narrow-to-outline-level)
    ("\C-cou"  'xwl-outline-toggle-enter-exit)
    ("\C-coq"  'xwl-outline-toggle-show-hide))

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

  (deh-after-load "outline" (require 'foldout))

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
  (add-to-list 'imenu-after-jump-hook #'(lambda () (recenter 0)))
  (setq imenu-max-item-length 60
        imenu-max-items 500
        imenu-auto-rescan t))

(deh-section "ediff"
  ;; (global-set-key "\C-cd" 'ediff-show-registry)
  (setq diff-switches "-ubB"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

;;; highlight
(deh-require-reserved 'highlight-parentheses
  ;; colors is applied by reversed order
  (setq hl-paren-colors
        '("orange1" "yellow1" "greenyellow" "green1"
          "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))
  (deh-add-hook '(emacs-lisp-mode-hook
                  c-mode-common-hook)
    (highlight-parentheses-mode 1)))

(deh-section "highlight-line"
  ;; (global-hl-line-mode 1)
  (setq hl-line-face 'underline)
  ;; (set-face-background 'hl-line "white smoke") ; list-colors-display
  )

(deh-require 'highlight-symbol
  (deh-add-hook '(emacs-lisp-mode-hook
                  python-mode-hook
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

(deh-require 'diminish
  (diminish 'abbrev-mode "Abv")
  (diminish 'highlight-parentheses-mode)
  (diminish 'hi-lock-mode)
  (diminish 'undo-tree-mode)
  (deh-after-load "eldoc" (diminish 'eldoc-mode)))

;; ;; erc
;; (deh-section "erc"
;;   (setq erc-log-channels-directory (expand-file-name "erc" my-data-dir))
;;   (deh-after-load "erc"
;;     (deh-require 'emoticons
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
