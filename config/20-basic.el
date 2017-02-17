;; -*- mode: Emacs-Lisp -*-

;; BASIC EDIT FUNCTIONS
;;
;; cursor move, page scroll, search, etc. All enhancement based on the
;; original functions.

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end

(deh-package generic-x)

(deh-package undo-tree        ; disable it, cause C-g abnormal
  :disabled
  :diminish undo-tree-mode
  :bind
  ("C-c c u" . undo-tree-visualize)
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo"))))
  (defadvice undo-tree-make-history-save-file-name
    (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz"))))

(deh-package browse-kill-ring
  :defer
  :config
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-highlight-current-entry t))

;;; mode line
(deh-package smart-mode-line
  :config
  (setq sml/theme 'dark)
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(deh-section mode-line
  :disabled
  (size-indication-mode 1)
  ;; (setq-default mode-line-buffer-identification (propertized-buffer-identification "%b"))

  (defun get-lines-4-mode-line ()
    (let ((lines (count-lines (point-min) (point-max))))
      (concat (propertize
               (concat "%l:%c " (format "%dL" lines))
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

;;; minibuffer

(deh-package minibuffer
  :commands (minibuf-isearch-next minibuf-isearch-prev)
  :config
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

(deh-package dired
  :bind (("C-x C-j" . dired-jump))
  :config
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
        '(byte-compile chgrp chmod chown compress copy delete hardlink
                       load move print shell symlink uncompress))

  ;; Open directory in the same buffer
  (put 'dired-find-alternate-file 'disabled nil)

  ;;# Keybind for dired
  (bind-keys
   :map dired-mode-map
   ("M-u"  . (lambda () (interactive) (find-alternate-file "..")))   ; remember previous upper directory
   ;; ("M-="  . dired-backup-diff)
   ("b"     . browse-url-of-dired-file)
   ("W"     . woman-dired-find-file)
   ("r"     . wdired-change-to-wdired-mode) ; editable mode, "C-c C-k" abort
   (" "     . dired-count-directory-size)
   ("E"     . dired-w3m-visit)
   ;; ("!"     . dired-do-shell-command)
   ("z"     . dired-compress-directory))

  ;;# hooks
  (deh-add-hook dired-load-hook
    (load "dired-x")
    ;; Make the execuatable file with different color
    (add-to-list 'dired-font-lock-keywords
                 (list dired-re-exe
                       '(".+" (dired-move-to-filename) nil (0 font-lock-type-face))) t))
  (deh-add-hook dired-mode-hook (dired-omit-mode t))
  (deh-add-hook dired-after-readin-hook
    (set (make-local-variable 'truncate-lines) t)
    (save-excursion                     ; sort directories first
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point)
                            (point-max)))))

  ;;# helper functions
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


  (defun my/dired-filter-regexp (regexp &optional arg)
    (interactive
     (list (dired-read-regexp
            (concat (if current-prefix-arg "Exclude" "Exclude not")
                    " match (regexp): "))
           current-prefix-arg))
    (dired-mark-files-regexp regexp)
    (or arg (dired-toggle-marks))
    (dired-do-kill-lines))

  (defun my/dired-filter-extension (extension &optional arg)
    (interactive
     (list (read-from-minibuffer
            (concat "Exclude extension is "
                    (if current-prefix-arg "" "not") ": "))
           current-prefix-arg))
    (my/dired-filter-regexp (concat "\\." extension "\\'") arg)))

(deh-package dired-x
  :commands dired-omit-mode
  :config
  (dolist (ext '(".bak" ".dSYM" ".dsp" ".plg" ".vcproj"))
    (add-to-list 'dired-omit-extensions ext))

  (setq dired-omit-files
        (concat "^[.#]" "\\|"
                "^" (regexp-opt '("TAGS" "GPATH" "GRTAGS" "GSYMS" "GTAGS") t) "$"))

  ;; Based upon the name of a file, Dired tries to guess what shell
  ;; command you might want to apply to it. For example, if you have
  ;; point on a file named foo.tar and you press !, Dired will guess
  ;; you want to ‘tar xvf’ it and suggest that as the default shell
  ;; command.
  (dolist (file my/dired-guess-command-alist)
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
    "Replace `shell-command' to `start-process-shell-command' to
run command asynchronously. Originally defined in dired-aux.el"
    (let ((handler
           (find-file-name-handler (directory-file-name default-directory)
                                   'shell-command)))
      (if handler
          (apply handler 'shell-command (list command))
        (start-process-shell-command "dired-run" nil command)))
    ;; Return nil for sake of nconc in dired-bunch-files.
    nil))


(deh-package helm                       ; http://tuhdo.github.io/helm-intro.html
  :diminish helm-mode
  ;; :bind-keymap* ("C-c h" . helm-command-prefix)
  :bind
  ("M-x" . helm-M-x)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  ("C-x C-b" . helm-buffers-list)
  ("C-x C-f" . helm-find-files)
  ("C-x M-f" . helm-for-files)
  ("C-h SPC" . helm-all-mark-rings)
  :init
  (require 'helm-config)
  ;; (global-unset-key (kbd "C-x c"))
  (helm-mode)
  (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
  :config
  (bind-keys
   :map minibuffer-local-map
   ("C-c C-l" . helm-minibuffer-history))

  (bind-keys
   :map helm-map
   ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
   ("C-i"   . helm-execute-persistent-action) ; make TAB works in terminal
   ("C-z"   . helm-select-action)             ; list actions using C-z
   ("C-w"   . backward-kill-word))

  (bind-keys
   :map helm-command-map
   ("i" . helm-semantic-or-imenu)
   ("m" . helm-man-woman)
   ("/" . helm-find)
   ("l" . helm-locate)
   ("a" . helm-apropos)
   ("o" . helm-occur)
   ("s" . helm-swoop)                   ;like occur
   ("<tab>" . helm-lisp-completion-at-point)
   ("b" . helm-resume)
   ("x" . helm-register)
   ("p" . helm-projectile)
   ("g" . helm-do-grep-ag)
   )

  (bind-keys
   :map helm-find-files-map
   ("M-u" . helm-find-files-up-one-level))

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  ;; enable man page at point
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

  ;; enable fuzzy matching
  (setq helm-M-x-fuzzy-match t      ; helm-M-x
        helm-buffers-fuzzy-matching t   ; helm-mini
        helm-recentf-fuzzy-match    t   ; helm-mini
        helm-semantic-fuzzy-match t     ; helm-semantic-or-imenu
        helm-imenu-fuzzy-match    t ; helm-semantic-or-imenu
        helm-locate-fuzzy-match nil ; helm-locate
        helm-apropos-fuzzy-match t      ; helm-apropos
        helm-lisp-fuzzy-completion t    ; helm-lisp-completion-at-point
        helm-ff-guess-ffap-filenames t  ; helm-find-files
        )

  (setq helm-candidate-number-limit 100)
  ;; From https://gist.github.com/antifuchs/9238468
  (setq helm-idle-delay 0.0         ; update fast sources immediately (doesn't).
        helm-input-idle-delay 0.01  ; this actually updates things reeeelatively quickly.
        helm-autoresize-mode t
        helm-quick-update t
        helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        ;; helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-ff-skip-boring-files t
        helm-ff-file-name-history-use-recentf t
        helm-yas-display-key-on-candidate t)

  (deh-package helm-eshell
    :defer
    :config
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (define-key eshell-mode-map
                    (kbd "C-c C-l")  'helm-eshell-history))))

  (deh-package helm-swoop
    :defer
    :bind*
    ("M-i" . helm-swoop)
    ("C-c M-i" . helm-multi-swoop)
    ("C-x M-i" . helm-multi-swoop-all)
    :config
    (bind-key "M-i" 'helm-swoop-from-isearch isearch-mode-map)
    (bind-key "M-i" 'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)
    )

  (deh-package helm-gtags
    :defer 3
    :if (executable-find "gtags")
    :diminish (helm-gtags-mode . "hG")
    :config
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t)
    (bind-keys
     :map helm-gtags-mode-map
     ("M-." . helm-gtags-find-tag)
     ("M-," . helm-gtags-pop-stack)
     ("M-*" . helm-gtags-pop-stack)
     ("M-s d" . helm-gtags-dwim)
     ("M-s r" . helm-gtags-find-rtag)
     ("M-s s" . helm-gtags-find-symbol)
     ("C-c i" . helm-gtags-parse-file)  ;replace imenu
     ("C-c <" . helm-gtags-previous-history)
     ("C-c >" . helm-gtags-next-history)
     )
    (add-hook 'c-mode-hook #'helm-gtags-mode)
    (add-hook 'c++-mode-hook #'helm-gtags-mode))

  (deh-package helm-ag)

  (deh-package helm-bm
    :after bm)

  (deh-package helm-c-yasnippet
    :after yasnippet
    :bind
    ("C-c y" . helm-yas-complete)
    :config
    (setq helm-yas-space-match-any-greedy t))

  (deh-package helm-descbinds
    :defer t
    :bind
    ("C-h w" . helm-descbinds)
    :init
    (helm-descbinds-mode))

  (deh-package wgrep-helm)
  )

(deh-package ido
  :disabled
  :config
  ;; ido everywhere
  (deh-package ido-ubiquitous
    :config (ido-ubiquitous-mode 1))

  (deh-package ido-at-point
    :init (ido-at-point-mode)
    :bind ("C-," . completion-at-point))

  (deh-package flx-ido
    :config
    (flx-ido-mode 1)
    ;; (setq ido-use-faces nil
    ;;       flx-ido-use-faces nil)
    )
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
        ido-default-buffer-method 'selected-window
        ido-ignore-extensions t         ; refer to `completion-ignored-extensions'
        ido-auto-merge-work-directories-length -1
        ;; ido-use-virtual-buffers t
        ido-max-work-file-list 20)

  (setq ido-save-directory-list-file
        (expand-file-name "emacs.ido-last" my/data-dir)
        org-id-locations-file
        (expand-file-name "emacs.ido-locations" my/data-dir))
  (setq ido-ignore-buffers
        '("^ " "_region_" "TAGS$"
          (lambda (buf)
            (with-current-buffer buf
              (or
               ;; ignore dired-mode
               (eq (buffer-local-value 'major-mode (current-buffer)) 'dired-mode)
               (and
                ;; exclude *scratch*, *info*, etc.
                (not (member-ignore-case
                      (buffer-name)
                      '("*scratch*" "*info*" "*grep*")))
                ;; exclude *tumblr ...*
                (not (string-match "^\\*tumblr.+" (buffer-name)))
                (string-match "^\\*.+" (buffer-name)))))))
        ido-ignore-directories
        '("^auto/" "^CVS/" "^\\.")
        ido-ignore-files
        '("^[.#]" "~$" "\\.DS_Store"
          "\\.\\(log\\|out\\|d\\)$"
          "\\(TAGS\\|GPATH\\|GSYMS\\)$")
        ido-work-directory-list-ignore-regexps
        `(,tramp-file-name-regexp))
  (setq ido-file-extensions-order
        '(".h" ".c" ".cpp" ".py" ".sh" ".el" ".txt" ".org" ".md"))

  (deh-add-hook ido-setup-hook
    (bind-keys
     :map ido-file-completion-map
     ("~" . (lambda ()
              (interactive)
              (cond
               ((looking-back "~/") (insert "works/"))
               ((looking-back "/") (insert "~/"))
               (:else (call-interactively 'self-insert-command))))))
    (bind-keys
     :map ido-completion-map
     ("C-n"    .  ido-next-match-dir)
     ("C-p"    .  ido-prev-match-dir)
     ("M-u"    .  ido-up-directory)
     ("C-M-h"  .  ido-goto-home)
     ("C-u"    .  ido-clean-text)
     ("C-w"    .  ido-delete-backward-word-updir)
     ("C-x C-w" . ido-copy-current-file-name)
     ;; Remind keybinds
     ;; ("C-a" .  .  ido-toggle-ignore)
     ("C-S-p"  .  ido-toggle-prefix)
     ;; ("C-t"  .  ido-enable-regexp)
     ;; ("M-n"  .  ido-next-work-directory)
     ;; ("M-p"  .  ido-prev-work-directory)
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
          "~/.emacs.d/lisp/"
          "~/projects/"
          "~/works/"
          "~/temp/"
          "~/bin/"
          "~/")))

(deh-package smex
  :disabled
  :commands smex-initialize
  :bind
  ("M-x"     . smex)
  ("C-c M-x" . smex-major-mode-commands)
  ("C-c C-c M-x"  . execute-extended-command)
  :config
  (setq smex-save-file (expand-file-name "emacs.smex-items" my/data-dir)
        smex-history-length 50)
  )

(deh-package ibuffer
  :disabled
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-old-time 24
        ibuffer-show-empty-filter-groups nil)
  ;; keybinds
  ;; (global-set-key (kbd "C-x C-b") 'ibuffer)
  (bind-keys
   :map ibuffer-mode-map
   ("r"  . ibuffer-rename-buffer)
   ("C-x C-f"  . ibuffer-find-file)
   (" "  . scroll-up))

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

  (deh-package ibuf-ext
    :defer
    :config
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


(deh-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        ;; uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t ; rename after killing uniquified
        uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  )

(deh-package tramp
  :defer
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

  :config
  ;; (setq tramp-mode nil)                  ; disable tramp
  (setq tramp-auto-save-directory my/data-dir
        tramp-persistency-file-name (expand-file-name "tramp" my/data-dir)
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

;;; Better scrolling

(deh-package pager
  :disabled
  :bind
  ([remap scroll-up-command]  .  pager-page-down) ;C-v
  ([remap scroll-down-command]  .  pager-page-up) ;M-v
  ("<up>" .  pager-row-up)
  ("M-p"  .  pager-row-up)
  ("<down>" .  pager-row-down)
  ("M-n"  .  pager-row-down)
  :config
  (setq scroll-margin 1) ; scroll-margin conflict with pager-mode, it should be 0
  )

(deh-package on-screen
  :config
  (on-screen-global-mode 1)
  (setq on-screen-highlight-method 'narrow-line)
  (set-face-underline 'on-screen-narrow-line '(:color "#444" :style wave)))

;;; Search

;; (deh-package ace-jump-mode
;;   :commands (ace-jump-mode
;;              ace-jump-char-mode
;;              ace-jump-word-mode
;;              ace-jump-line-mode
;;              ace-jump-mode-pop-mark)
;;   :bind*
;;   ("C-c C-j" . ace-jump-mode)
;;   ("C-c C-p" . ace-jump-mode-pop-mark)
;;   ("M-4" . ace-jump-char-mode)
;;   ("C-4" . ace-jump-mode)
;;   :config
;;   (ace-jump-mode-enable-mark-sync))

(deh-package avy
  :bind*
  ("M-g w"   . avy-goto-word-1)
  ("C-c C-j" . avy-goto-word-1)
  ("M-4"     . avy-goto-word-or-subword-1)
  ("M-g f"   . avy-goto-char)
  ("M-g l"   . avy-goto-line)
  :config
  (avy-setup-default)
  (setq avy-background t)
  (setq avy-keys (number-sequence ?a ?z)))

;; simulate `f' in VIM
(deh-package jump-char
  :commands (jump-char-forward jump-char-backward)
  :bind*
  ("M-m"   . jump-char-forward)         ;override back-to-indentation
  ("S-M-m" . jump-char-backward)
  ("C-c C-f"   . jump-char-forward)
  ("C-c C-M-f" . jump-char-backward)
  ("M-3"   . jump-char-forward)
  ("C-M-3" . jump-char-backward)
  :config
  ;; Don't highlight matches with jump-char - it's distracting
  (setq jump-char-lazy-highlight-face nil))

(deh-package misc
  :commands zap-to-char)

(deh-package expand-region
  :bind
  ("M-["  .   er/expand-region)
  ("C-1"  .   er/expand-region)
  ("M-2"  .   er/expand-region)
  ("M-]"  .   er/contract-region))

(deh-section occur
  (deh-add-hook occur-mode-hook
    (require 'moccur-edit nil t)
    (set (make-local-variable 'truncate-lines) t))

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

(deh-package grep
  :commands (grep-tag-default grep-apply-setting)
  :config
  (add-to-list 'grep-files-aliases '("hcpp" . "*.h *.c *.[hc]pp"))
  ;;# avoid print NUL when grep something in Windows.
  (if (eq system-type 'windows-nt) (setq grep-use-null-device nil))

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

(deh-package ag
  :if (executable-find "ag")
  :bind* ("C-c ag" . ag))

(deh-section isearch
  (setq isearch-case-fold-search t)     ; case insensitive
  (bind-keys
   :map isearch-mode-map
   ("<tab>"  . isearch-complete)
   ("M-<"    . isearch-beginning-of-buffer)
   ("M->"    . isearch-end-of-buffer)
   ("M-i"    . isearch-query-replace-current)
   ("C-u"    . isearch-clean)
   ("C-M-y"  . isearch-yank-symbol-regexp)
   ("C-y"    . isearch-yank-symbol) ; instead of `isearch-yank-line'
   ("C-o"    . isearch-occur)
   ;; Remind other useful keybinds
   ;; ("M-e"  . isearch-edit-string)
   ;; ("M-y"  . isearch-yank-kill)
   ;; ("M-e"  . isearch-edit-string)
   ;; ("M-c"  . isearch-toggle-case-fold)
   ;; ("M-r"  . isearch-toggle-regexp)
   ;; ("M-sr" . isearch-toggle-regexp)
   ;; ("M-sw" . isearch-toggle-word)
   ;; ("M-so" . isearch-occur)
   ;; ("M-shr" . isearch-highlight-regexp)
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

  (defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
    "Isearch with selected region."
    (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
        (progn
          (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
          (deactivate-mark)
          ad-do-it
          (if (not forward)
              (isearch-repeat-backward)
            (goto-char (mark))
            (isearch-repeat-forward)))
      ad-do-it))

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

;;; navigation

(deh-package hideshow           ; for semantic code
  :commands hs-minor-mode
  :diminish hs-minor-mode
  :config
  (bind-keys
   :map hs-minor-mode-map
   :prefix-map hs-minor-mode-prefix
   :prefix "C-c C-a"
   ("h"  . hs-hide-block)
   ("s"  . hs-show-block)
   ("H"  . hs-hide-all)
   ("S"  . hs-show-all)
   ("t"  . hs-toggle-hiding)
   ("C-a"  . hs-toggle-hiding))
  (bind-key "<left-fringe> <mouse-2>" 'hs-mouse-toggle-hiding
            hs-minor-mode-map)

  (defvar hs--overlay-keymap nil "keymap for folding overlay")
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'hs-show-block)
    (setq hs--overlay-keymap map))
  (setq hs-set-up-overlay
        (defun my/display-code-line-counts (ov)
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

(deh-package outline            ; for literal text
  :commands (outline-mode outline-minor-mode)
  :diminish outline-minor-mode
  ;; :bind-keymap* ("C-c C-a" . outline-minor-mode-prefix)
  :config
  (bind-keys
   :map outline-minor-mode-map
   :prefix-map outline-mode-prefix-map
   :prefix "C-c C-a"
   ("s"  . show-subtree)
   ("S"  . show-all)
   ("h"  . hide-subtree)
   ("H"  . hide-body)
   ;; shortcuts
   ("<right>" . show-subtree)
   ("<M-right>" . show-all)
   ("<left>" . hide-subtree)
   ("<M-left>" . hide-body)
   ("<up>" . outline-previous-heading)
   ("<down>" . outline-next-heading)
   ("<M-up>" . outline-previous-visible-heading)
   ("<M-down>" . outline-next-visible-heading)
   ;; xwl keybinds
   ("n"  . xwl-narrow-to-outline-level)
   ("u"  . xwl-outline-toggle-enter-exit)
   ("q"  . xwl-outline-toggle-show-hide)
   ("t"  . xwl-outline-toggle-show-hide)
   ("C-a"  . xwl-outline-toggle-show-hide))
  (bind-keys
   :map outline-minor-mode-map
   ("M-n" . outline-next-heading)
   ("M-p" . outline-previous-heading))

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

  (deh-after-load 'outline (require 'foldout))

  :init
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
        (narrow-to-region (point) end)))))

(deh-package which-func
  :config
  (which-func-mode 1))

(deh-package imenu
  :bind*
  ("C-c i" . my/imenu)
  :config
  ;; (setq imenu-max-item-length 60
  ;;       imenu-max-items 500
  ;;       imenu-auto-rescan nil ;disable imenu-auto-rescan, cause performance issue
  ;;       imenu-sort-function 'imenu--sort-by-name)
  (add-to-list 'imenu-after-jump-hook #'(lambda () (recenter 0)))

  (defun my/imenu ()
    (interactive)
    (call-interactively (if (fboundp 'helm-semantic-or-imenu)
                            'helm-semantic-or-imenu 'imenu)))
  ;; imenu-everywhere
  (defun ido-imenu-completion (index-alist &optional prompt)
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
  )
