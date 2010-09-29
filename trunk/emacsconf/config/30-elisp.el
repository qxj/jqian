;;;;;;;;;;;;;;;;;;;;;;;;;; standard library ;;;;;;;;;;;;;;;;;;
;;{{{ Dired
(deh-section "dired"
  (require 'dired-x)
  (require 'dired-single)
  (add-hook
   'dired-load-hook
   (lambda ()
     ;; Make the execuatable file with different color
     (add-to-list 'dired-font-lock-keywords
                  (list dired-re-exe
                        '(".+" (dired-move-to-filename) nil (0 font-lock-type-face))) t)))
  (add-hook
   'dired-mode-hook
   (lambda ()
     ;; No confirm operations
     (setq dired-no-confirm
           '(byte-compile chgrp chmod chown compress copy delete hardlink load move print shell symlink uncompress))
     ;; Keybind for dired
     ;; (define-key dired-mode-map (kbd "M-u" ) 'dired-up-directory)
     (deh-define-key dired-mode-map
       ( [return] . 'joc-dired-single-buffer)
       ( [mouse-1] . 'joc-dired-single-buffer-mouse)
       ( "^"    . '(lambda () (interactive) (joc-dired-single-buffer "..")))
       ( "\M-u"  . '(lambda () (interactive) (joc-dired-single-buffer "..")))
       ( "z"    . 'ywb-dired-compress-dir)
       ( "b"    . 'ywb-list-directory-recursive)
       ( "E"    . 'ywb-dired-w3m-visit)
       ( "j"    . 'ywb-dired-jump-to-file)
       ( "J"    . 'woman-dired-find-file)
       ( " "    . 'ywb-dired-count-dir-size)
       ( "r"    . 'wdired-change-to-wdired-mode)
       ( "W"    . 'ywb-dired-copy-fullname-as-kill)
       ( "a"    . 'ywb-add-description)
       ( "\C-q" . 'ywb-dired-quickview)
       ( "/r"   . 'ywb-dired-filter-regexp)
       ( "/."   . 'ywb-dired-filter-extension)
       )
     ;; Sort something, prefix key `s'
     (make-local-variable  'dired-sort-map)
     (setq dired-sort-map (make-sparse-keymap))
     (define-key dired-mode-map "s" dired-sort-map)
     (deh-define-key dired-sort-map
       ("s" . '(lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
       ("x" . '(lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
       ("t" . '(lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
       ("n" . '(lambda () "sort by Name" (interactive) (dired-sort-other (concat dired-listing-switches "")))))
     ))


  ;; Setting for dired
  (unless (eq system-type 'usg-unix-v)  ; solaris
    (setq dired-listing-switches "-alvh"))
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'top)
  (setq dired-dwim-target t)

  (deh-section "dired-omit"
    (add-hook 'dired-mode-hook
              (lambda ()
                (dired-omit-mode t)))
    (setq dired-omit-extensions
          '(".o" "~" ".bak" ".obj" ".lnk" ".a"
            ".ln" ".blg" ".bbl" ".drv" ".vxd" ".386" ".elc" ".lof"
            ".glo" ".lot" ".fmt" ".tfm" ".class" ".lib" ".mem" ".x86f"
            ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl"
            ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky"
            ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc"
            ".pyo" ".idx" ".lof" ".lot" ".glo" ".blg" ".bbl" ".cps"
            ".fn" ".fns" ".ky" ".kys" ".pgs" ".tp" ".tps" ".vr" ".vrs"
            ".pdb" ".ilk"))
    (setq dired-omit-files
          (concat "^[.#]\\|^" (regexp-opt '(".." "." "CVS" "_darcs" "TAGS" "GPATH" "GRTAGS" "GSYMS" "GTAGS") t) "$")))

  (deh-section "dired-assoc"
    (dolist (file `(("acroread" "pdf")
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
                    ("gthumb" "gif" "jpeg" "jpg" "tif" "png")
                    ("docview.pl" "doc")
                    ("ooffice -writer" "ods" "doc")
                    ("ooffice -calc"  "xls")
                    ("ooffice -impress" "odt" "ppt")
                    ("gnumeric" "xls")
                    ("7z x" "7z")
                    ("djview" "djvu")
                    ("perl" "pl")
                    ("firefox" "xml" "html" "htm" "mht")))
      (add-to-list 'dired-guess-shell-alist-default
                   (list (concat "\\." (regexp-opt (cdr file) t) "$")
                         (car file)))))
  ;; (dolist (suf (cdr file))
  ;;   (let ((prg (assoc suf dired-guess-shell-alist-default)))
  ;;     (if prg
  ;;         (setcdr prg (delete-dups (cons (car file) (cdr prg))))
  ;;       (add-to-list 'dired-guess-shell-alist-default (list suf (car file))))))))
  ;; sort directories first
  (defun my-dired-sort ()
    "Dired sort hook to list directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point)
                            (point-max))))
    (set-buffer-modified-p nil))
  (add-hook 'dired-after-readin-hook 'my-dired-sort)
  (defun my-dired-long-lines ()
    (setq truncate-lines t))
  (add-hook 'dired-after-readin-hook 'my-dired-long-lines)
  ;; Dired Association
  ;;This allows "X" in dired to open the file using the explorer settings.
  ;;From TBABIN(at)nortelnetworks.com
  ;;ToDo: adapt mswindows-shell-execute() for XEmacs or use tinyurl shell exec
  (if (eq system-type 'windows-nt)
      (progn
        (defun dired-custom-execute-file (&optional arg)
          (interactive "P")
          (mapcar #'(lambda (file)
                      (w32-shell-execute "open" (convert-standard-filename file)))
                  (dired-get-marked-files nil arg)))
        (defun dired-custom-dired-mode-hook ()
          (define-key dired-mode-map "X" 'dired-custom-execute-file))
        (add-hook 'dired-mode-hook 'dired-custom-dired-mode-hook))
    ;; Redefine of this function
    (defun dired-run-shell-command (command)
      (let ((handler
             (find-file-name-handler (directory-file-name default-directory)
                                     'shell-command)))
        (if handler
            (apply handler 'shell-command (list command))
          (start-process-shell-command "dired-run" nil command)))
      ;; Return nil for sake of nconc in dired-bunch-files.
      nil)))
;;}}}

;;{{{ ido
(deh-require 'ido
  ;; (ido-mode 1) ;; avoid recursive tramp load error, it's a reported bug
  (ido-everywhere t)
  (add-hook 'term-setup-hook 'ido-mode)

  (setq ido-enable-regexp t
        ido-enable-tramp-completion nil
        ido-use-faces t
        ido-use-filename-at-point 'guess
        ido-use-url-at-point t
        ido-auto-merge-work-directories-length -1)

  (setq ido-save-directory-list-file
        (expand-file-name "emacs.ido-last" my-temp-dir)
        org-id-locations-file
        (expand-file-name "emacs.ido-locations" my-temp-dir))
  (setq ido-ignore-buffers
        '("\\` " "^\\*.+" "_region_" "^TAGS$")
        ido-ignore-directories
        '("^auto/" "^CVS/" "^\\.")
        ido-ignore-files
        '("\\.\\(aux\\|nav\\|out\\|log\\|snm\\|toc\\|vrb\\)$"
          "^\\(CVS\\|TAGS\\|GPATH\\|GRTAGS\\|GSYMS\\|GTAGS\\)$"
          "_region_" "^[.#]"))
  ;; visit with dired also push the diretory to `ido-work-directory-list'
  (defadvice ido-file-internal (after ido-dired-add-work-directory)
    (when (eq ido-exit 'dired)
      (ido-record-work-directory (expand-file-name default-directory))))
  (ad-activate 'ido-file-internal)
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
;;}}}

;;{{{ Ibuffer
(deh-require 'ibuffer
  (require 'ibuf-ext nil t)
  (define-ibuffer-sorter file-name
    "Sort buffers by associated file name"
    (:description "file name")
    (apply 'string<
           (mapcar (lambda (buf)
                     (with-current-buffer (car buf)
                       (or buffer-file-name default-directory)))
                   (list a b))))

  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (define-key ibuffer-mode-map "sf" 'ibuffer-do-sort-by-file-name)
  (defun ywb-ibuffer-rename-buffer ()
    (interactive)
    (call-interactively 'ibuffer-update)
    (let* ((buf (ibuffer-current-buffer))
           (name (generate-new-buffer-name
                  (read-from-minibuffer "Rename buffer(to new name): "
                                        (buffer-name buf)))))
      (with-current-buffer buf
        (rename-buffer name)))
    (call-interactively 'ibuffer-update))
  (defun ywb-ibuffer-find-file ()
    (interactive)
    (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                               (if (buffer-live-p buf)
                                   (with-current-buffer buf
                                     default-directory)
                                 default-directory))))
      (call-interactively 'ido-find-file)))
  (define-key ibuffer-mode-map "r" 'ywb-ibuffer-rename-buffer)
  (define-key ibuffer-mode-map (kbd "C-x C-f") 'ywb-ibuffer-find-file)
  (define-key ibuffer-mode-map " " 'scroll-up)
  ;; group buffers
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("*buffer*" (name . "\\*.*\\*"))
           ("dired" (mode . dired-mode))
           ("prog" (or (mode . c++-mode)
                       (mode . c-mode)
                       (mode . java-mode)
                       (mode . python-mode)
                       (mode . makefile-mode)))
           ("web" (or  (mode . html-mode)
                       (mode . css-mode)
                       (mode . php-mode)
                       (mode . javascript-mode)
                       (mode . js2-mode)))
           ("elisp" (or (mode . emacs-lisp-mode)
                        (mode . lisp-interaction-mode)))
           ("tags" (name . "^G?TAGS"))
           ("erc" (mode . erc-mode)))))
  (set 'ibuffer-mode-hook
       (lambda ()
         (ibuffer-switch-to-saved-filter-groups "default")))
  (setq ibuffer-saved-filters
        '(("t" ((or (mode . latex-mode)
                    (mode . plain-tex-mode))))
          ("c" ((or (mode . c-mode)
                    (mode . c++-mode))))
          ("p" ((mode . cperl-mode)))
          ("e" ((or (mode . emacs-lisp-mode)
                    (mode . lisp-interaction-mode))))
          ("d" ((mode . dired-mode)))
          ("s" ((mode . shell-mode)))
          ("i" ((mode . image-mode)))
          ("h" ((mode . html-mode)))
          ("gnus" ((or (mode . message-mode)
                       (mode . mail-mode)
                       (mode . gnus-group-mode)
                       (mode . gnus-summary-mode)
                       (mode . gnus-article-mode))))
          ("pr" ((or (mode . emacs-lisp-mode)
                     (mode . cperl-mode)
                     (mode . c-mode)
                     (mode . c++-mode)
                     (mode . php-mode)
                     (mode . java-mode)
                     (mode . idl-mode)
                     (mode . lisp-interaction-mode))))
          ("w" ((or (mode . emacs-wiki-mode)
                    (mode . muse-mode)
                    (mode . rst-mode)
                    (mode . org-mode)
                    (mode . LaTeX-mode))))
          ("*" ((name . "*")))))
  )
;;}}}

;;{{{ session management
(deh-section "session-management"
  (deh-require 'desktop
    (setq desktop-globals-to-save
          (delq 'tags-table-list desktop-globals-to-save)
          ;; Do not save to desktop
          desktop-buffers-not-to-save
          (concat "\\(" "\\.log\\|\\.diary\\|\\.elc" "\\)$"))

    (setq desktop-base-file-name (concat "emacs.desktop-" (system-name))
          desktop-path (list my-temp-dir)
          history-length 100)

    (add-to-list 'desktop-globals-to-save 'file-name-history)

    (add-to-list 'desktop-modes-not-to-save 'dired-mode)
    (add-to-list 'desktop-modes-not-to-save 'Info-mode)
    (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
    (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
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
  (deh-require 'session
    (setq session-save-file (expand-file-name "emacs.session" my-temp-dir))
    (setq session-save-file-coding-system 'utf-8-unix)
    (add-to-list 'session-globals-exclude 'org-mark-ring)
    (add-hook 'after-init-hook 'session-initialize))
  (deh-require 'winsav
    ;; (winsav-save-mode 1)
    (setq winsav-dirname my-temp-dir
          winsav-base-file-name (expand-file-name "emacs.winsav" my-temp-dir)))
  )
;;}}}

;;{{{ pager
(deh-require 'pager
  (global-set-key (kbd "C-v") 'pager-page-down)
  (global-set-key (kbd "M-v") 'pager-page-up)
  (global-set-key (kbd "<up>") 'pager-row-up)
  (global-set-key (kbd "M-p") 'pager-row-up)
  (global-set-key (kbd "<down>") 'pager-row-down)
  (global-set-key (kbd "M-n") 'pager-row-down)
  )
;;}}}

;;{{{ standard libraries and settings
(deh-section "std-lib"
  (partial-completion-mode 1)
  (icomplete-mode 1)
  (winner-mode 1)
  ;; (auto-insert-mode 1)
  ;; view
  (setq view-mode-hook
        '((lambda ()
            (define-key view-mode-map "h" 'backward-char)
            (define-key view-mode-map "l" 'forward-char)
            (define-key view-mode-map "j" 'next-line)
            (define-key view-mode-map "k" 'previous-line))))

  ;; TimeStamp
  (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-active t)
  (setq time-stamp-warn-inactive t)

  (add-hook 'occur-mode-hook (lambda () (setq truncate-lines t)))

  ;; ffap
  (require 'ffap)
  ;; for windows path recognize
  (setq ffap-string-at-point-mode-alist
        '((file "--:\\\\$+<>@-Z_a-z~*?" "<@" "@>;.,!:")
          (url "--:=&?$+@-Z_a-z~#,%;*" "^A-Za-z0-9" ":;.,!?")
          (nocolon "--9$+<>@-Z_a-z~" "<@" "@>;.,!?")
          (machine "-a-zA-Z0-9." "" ".")
          (math-mode ",-:$+<>@-Z_a-z~`" "<" "@>;.,!?`:")))

  ;; tetris game
  ;; (setq tetris-update-speed-function (lambda (shapes rows) (/ 10.0 (+ 80.0 rows))))
  ;; woman
  (setq woman-use-own-frame nil)
  (setq woman-fontify t)                ; dump emacs need this
  (autoload 'woman-mode "woman")
  (add-hook 'woman-mode-hook 'view-mode)
  (autoload 'woman-decode-buffer "woman")

  ;; change-log
  (add-hook 'change-log-mode-hook
            (lambda ()
              (auto-fill-mode t)
              (add-to-list 'change-log-font-lock-keywords
                           '("^[0-9-]+:? +\\|^\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\) [A-z][a-z][a-z] [0-9:+ ]+"
                             (0 'change-log-date-face)
                             ("\\([^<(]+?\\)[   ]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]" nil nil
                              (1 'change-log-name)
                              (2 'change-log-email))))))

  ;; generic-x
  (require 'generic-x)

  ;; autosave bookmark into the diskete
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file (expand-file-name "emacs.bookmark" my-temp-dir))
  (add-hook 'bookmark-bmenu-mode-hook
            (lambda ()
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

  (add-hook 'Info-mode-hook
            (lambda ()
              (define-key Info-mode-map "j" 'next-line)
              (define-key Info-mode-map "k" 'previous-line)))
  ;; (filesets-init)
  (defalias 'default-generic-mode 'conf-mode)
  )

;; formats and timestamp
(deh-section "formats"
  (setq display-time-format "%m月%d日 星期%a %R")

  ;; emacs frame title
  (setq frame-title-format
        '((:eval
           (let ((login-name (getenv-internal "LOGNAME")))
             (if login-name (concat login-name "@") "")))
          (:eval (system-name))
          ":"
          (:eval (or (buffer-file-name) (buffer-name)))))

  ;; (setq time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S %:a by %u")
  (setq time-stamp-format "%U %:y-%02m-%02d %02H:%02M:%02S"))

;; uniquify
(deh-require 'uniquify
  (setq uniquify-buffer-name-style 'forward)
  ;; (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  )

(deh-section "hippie-expand"
  (setq hippie-expand-try-functions-list
        '(try-expand-line
          try-expand-dabbrev
          try-expand-line-all-buffers
          try-expand-list
          try-expand-list-all-buffers
          try-expand-dabbrev-visible
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name
          try-complete-file-name-partially
          try-complete-lisp-symbol
          try-complete-lisp-symbol-partially
          try-expand-whole-kill)))
(deh-section "doc-view"
  (add-hook 'doc-view-mode-hook
            (lambda ()
              (define-key doc-view-mode-map [remap move-beginning-of-line] 'image-bol)
              (define-key doc-view-mode-map [remap move-end-of-line] 'image-eol))))
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
;;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;; site-lisp library ;;;;;;;;;;;;;;;;;;

;;; recommend
;;{{{  w3m
(deh-require 'w3m-load
  (setq w3m-verbose t)                  ; log in *Messages*
  (add-hook 'w3m-mode-hook
            (lambda ()
              (defun ywb-w3m-goto-url (url)
                (if (and url (stringp url))
                    (w3m-goto-url url)))
              (local-unset-key "\C-xb")
              (local-unset-key (kbd "S-SPC"))
              (define-key w3m-mode-map "n" (lambda nil (interactive) (ywb-w3m-goto-url w3m-next-url)))
              (define-key w3m-mode-map "p" (lambda nil (interactive) (ywb-w3m-goto-url w3m-previous-url)))
              (define-key w3m-mode-map "t" (lambda nil (interactive) (ywb-w3m-goto-url w3m-contents-url)))
              ))

  (add-hook 'w3m-load-hook
            (lambda ()
              (add-to-list 'w3m-relationship-estimate-rules
                           `(w3m-relationship-simple-estimate
                             ""
                             ,(concat "<a\\s-+href=" w3m-html-string-regexp
                                      "\\s-*>.\\{,25\\}\\(?:next\\|后\\|下\\)")
                             ,(concat "<a\\s-+href=" w3m-html-string-regexp
                                      "\\s-*>.\\{,25\\}\\(?:prev\\|前\\|上\\)")
                             nil
                             ,(concat "<a\\s-+href=" w3m-html-string-regexp
                                      "\\s-*>.\\{,25\\}\\(?:index\\|目录\\)")
                             ))))
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
          (call-interactively 'w3m)))))
  )
;;}}}

;;{{{ autopair, like skeleton
(deh-require 'autopair
  (dolist (hook '(java-mode-hook
                  c-mode-common-hook
                  html-mode-hook))
    (add-hook hook
              #'(lambda () (autopair-mode))))

  ;; autopair work with paredit when editing emacs lisp
  (autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook))
    (add-hook hook
              #'(lambda () (paredit-mode +1)
                  (local-set-key "\C-cp" 'paredit-mode))))

  (defadvice paredit-mode (around disable-autopairs-around (arg))
    "Disable autopairs mode if paredit-mode is turned on"
    ad-do-it
    (if (null ad-return-value)
        (autopair-mode 1)
      (autopair-mode 0)))

  (ad-activate 'paredit-mode))
;;}}}

;;{{{ auto-complete
(deh-section "auto-complete"
  (require 'auto-complete-config)
  ;; specify a file stores data of candidate suggestion
  (setq ac-comphist-file (expand-file-name "ac-comphist.dat" my-temp-dir))

  (add-to-list 'ac-dictionary-directories
               (expand-file-name "ac-dict" my-startup-dir))
  (add-to-list 'ac-user-dictionary-files
               (expand-file-name "ac.dict" my-startup-dir))

  ;; enable auto-complete in comments
  (setq ac-disable-faces
        '(font-lock-string-face font-lock-doc-face))

  (ac-config-default)

  ;; keybind, `ac-complete-map' is deperecated, instead of `ac-menu-map'
  (setq ac-use-menu-map t)
  (define-key ac-menu-map (kbd "M-n") 'ac-next)
  (define-key ac-menu-map (kbd "M-p") 'ac-previous)
  ;; donot use RET for auto complete, only TAB
  (define-key ac-menu-map (kbd "<return>") nil)
  (define-key ac-menu-map (kbd "RET") nil)
  (define-key ac-menu-map (kbd "M-j") 'ac-complete)



  ;; c/c++
  ;; hack auto-complete.el (deperated)
  ;; add ac-prefix "->" to function `ac-prefix-c-dot'
  (defun ac-cc-mode-setup ()
    "customized setup for `c-mode-common-hook'"
    (dolist (command `(c-electric-backspace
                       c-electric-backspace-kill))
      (add-to-list 'ac-trigger-commands-on-completing command))
    (setq ac-sources (append '(ac-source-yasnippet
                               ac-source-gtags
                               ac-source-semantic
                               ac-source-imenu) ac-sources)))

  ;; python
  (defun ac-python-mode-setup ()
    (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))

  (add-hook 'python-mode-hook 'ac-python-mode-setup)
  )
;;}}}

;;{{{ Yet Another Snippet -  pluskid@newsmth
(deh-require 'yasnippet
  (setq yas/root-directory my-snippet-dir)
  (yas/load-directory yas/root-directory)
  (yas/initialize)     ;; enable yas/minor-mode globally
  (require 'dropdown-list)
  (setq yas/prompt-functions '(yas/dropdown-prompt
                               yas/ido-prompt
                               yas/completing-prompt))

  ;; FOR `hippie-try-expand' setting
  (add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)

  ;; FOR `auto-complete-mode', so disable default yasnippet expand action
  (if (fboundp 'auto-complete-mode)
      (progn
        (setq yas/trigger-key "M-/") ;; `M-/' works in terminal
        ;; (setq yas/trigger-key nil)
        (define-key yas/keymap (kbd "<right>") 'yas/next-field-or-maybe-expand)
        (define-key yas/keymap (kbd "<left>") 'yas/prev-field)))
)
;;}}}

;;{{{ Template
(deh-require 'template
  (template-initialize)
  (setq template-default-directories (list my-template-dir))
  ;; make custom prompts `ENDATE'
  (add-to-list 'template-default-expansion-alist
               '("ENDATE"
                 (let ((system-time-locale "C"))
                   (insert (format-time-string "%d %b %Y")))))
  ;; work with `ido-find-file'
  (dolist (cmd '(ido-select-text ido-magic-forward-char
                                 ido-exit-minibuffer))
    (add-to-list 'template-find-file-commands cmd))
  ;; WORKAROUND: avoid to auto update buffer `.ido-last'
  (setq template-header-lines 2)
  )
;;}}}

;;{{{ autoloads non-std libraries
(deh-section "non-std-lib"
  ;; wb-line
  ;; (autoload 'wb-line-number-toggle "wb-line-number" nil t)
  ;; htmlize
  (autoload 'htmlize-buffer "htmlize" "htmlize buffer" t)
  ;; moccur
  (autoload 'moccur-grep "moccur-edit" "Glob search file" t)
  (autoload 'moccur "moccur-edit" "moccur" t)
  ;; blank-mode
  (autoload 'blank-mode-on "blank-mode" "Turn on blank visualization."   t)
  (autoload 'blank-mode-off "blank-mode" "Turn off blank visualization."  t)
  (autoload 'blank-mode "blank-mode" "Toggle blank visualization."    t)
  (autoload 'blank-mode-customize "blank-mode" "Customize blank visualization." t)
  ;; hexl editor
  (autoload 'hexl-mode "hexl+" "Edit a file in a hex dump format" t)
  ;; A visual table editor, very cool
  (autoload 'table-insert "table" "WYGIWYS table editor")
  ;; ansit
  (autoload 'ansit-ansify-this "ansit"  "Ansi the region." t)
  ;; rst-mode
  (autoload 'rst-mode "rst" "" t)
  ;; minibuf-isearch
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
                               minibuffer-local-must-match-map))))

  ;; Add "CHARSET" for .po default charset
  (setq po-content-type-charset-alist
        '(("ASCII" . undecided)
          ("ANSI_X3.4-1968" . undecided)
          ("US-ASCII" . undecided)
          ("CHARSET" . undecided)))
  (autoload 'po-mode "po-mode"
    "Major mode for translators to edit PO files" t)

  (autoload 'yaml-mode "yaml-mode" "Simple mode to edit YAML." t)
  ;; (autoload 'muse-insert-list-item "muse-mode" t)
  ;; make cursor become a line
  ;; (require 'bar-cursor)
  ;; sdcv, dictionary search
  (autoload 'sdcv-search "sdcv-mode" "Search dictionary using sdcv" t)
  ;; smart mark, useful when edit markuped documents
  (require 'smart-mark)
  ;; visible-line
  (require 'visible-lines nil t)
  )

;; browse-kill-ring
(deh-require 'browse-kill-ring
  (browse-kill-ring-default-keybindings))

;; recent-jump
(deh-require 'recent-jump
  (global-set-key (kbd "M-o") 'recent-jump-jump-backward)
  (global-set-key (kbd "M-i") 'recent-jump-jump-forward))

;; recent opened files
(deh-require 'recentf
  ;; recent finded buffers
  (setq recentf-max-saved-items nil)
  (setq recentf-save-file (expand-file-name "emacs.recentf" my-temp-dir))
  (recentf-mode t)

  (defun recentf-open-files-compl ()
    (interactive)
    (let* ((all-files recentf-list)
           (tocpl (mapcar (function
                           (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
           (tokey (mapcar (function
                           (lambda (x) (file-name-nondirectory x))) all-files))
           ;; use `ido-completing-read' instead of `completing-read'
           (fname (ido-completing-read "Open file: " tokey nil t)))

      (find-file (cdr (assoc-string fname tocpl)))))
  (global-set-key (kbd "C-x C-o") 'recentf-open-files-compl)

  ;; Also store recent opened directories besides files
  (defun recentf-add-dir ()
    "Add directory name to recentf file list."
    (recentf-add-file dired-directory))
  (add-hook 'dired-mode-hook 'recentf-add-dir))

;; fold content
;; (deh-require 'fold
;;   (setq fold-mode-prefix-key "\C-c\C-o")
;;   (setq fold-autoclose-other-folds nil)
;;   (add-hook 'find-file-hook 'fold-find-file-hook t))

;; (deh-section "linum"
;;   (setq linum-format (concat (propertize "%6d " 'face 'default)
;;                              (propertize " " 'face 'fringe)))
;;   (autoload 'linum-mode "linum" "Display line number" t))

(deh-require 'bookmark+
  (setq bmkp-bmenu-commands-file
        (expand-file-name "emacs-bmk-bmenu-commands.el" my-temp-dir)
        bmkp-bmenu-state-file
        (expand-file-name "emacs-bmk-bmenu-state.el" my-temp-dir))
  (require 'bookmark+-lit)
  (setq bmkp-auto-light-when-jump (quote all-in-buffer)
        bmkp-auto-light-when-set (quote all-in-buffer))
	)

(deh-require 'auto-install
  ;; (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

(deh-section "anything"
  (autoload 'anything "anything" "" t)
  (eval-after-load "anything"
    '(progn
       (define-key anything-map "\C-n" 'anything-next-line)
       (define-key anything-map "\C-p" 'anything-previous-line)
       (define-key anything-map "\M-n" 'anything-next-source)
       (define-key anything-map "\M-p" 'anything-previous-source)))

  (try-require 'anything-config)

  (setq anything-c-adaptive-history-file
        (expand-file-name "anything-c-adaptive-history" my-temp-dir)
        anything-c-yaoddmuse-cache-file
        (expand-file-name "yaoddmuse-cache.el" my-temp-dir))

  (setq anything-sources
        '(
          ;; Buffer:
          anything-c-source-buffers
          anything-c-source-buffer-not-found
          anything-c-source-buffers+
          ;; File:
          anything-c-source-file-name-history
          anything-c-source-files-in-current-dir
          anything-c-source-files-in-current-dir+
          anything-c-source-file-cache
          anything-c-source-locate
          anything-c-source-recentf
          anything-c-source-ffap-guesser
          anything-c-source-ffap-line
          ;; Help:
          anything-c-source-man-pages
          anything-c-source-info-pages
          anything-c-source-info-elisp
          anything-c-source-info-cl
          ;; Command:
          anything-c-source-complex-command-history
          anything-c-source-extended-command-history
          anything-c-source-emacs-commands
          ;; Function:
          anything-c-source-emacs-functions
          anything-c-source-emacs-functions-with-abbrevs
          ;; Variable:
          anything-c-source-emacs-variables
          ;; Bookmark:
          anything-c-source-bookmarks
          anything-c-source-bookmark-set
          anything-c-source-bookmarks-ssh
          anything-c-source-bookmarks-su
          anything-c-source-bookmarks-local
          ;; Library:
          anything-c-source-elisp-library-scan
          ;; Programming:
          anything-c-source-imenu
          anything-c-source-ctags
          anything-c-source-semantic
          anything-c-source-simple-call-tree-functions-callers
          anything-c-source-simple-call-tree-callers-functions
          anything-c-source-commands-and-options-in-file
          ;; Color and Face:
          anything-c-source-customize-face
          anything-c-source-colors
          ;; Search Engine:
          anything-c-source-tracker-search
          anything-c-source-mac-spotlight
          ;; Kill ring:
          anything-c-source-kill-ring
          ;; Mark ring:
          anything-c-source-global-mark-ring
          ;; Register:
          anything-c-source-register
          ;; Headline Extraction:
          anything-c-source-fixme
          anything-c-source-rd-headline
          anything-c-source-oddmuse-headline
          anything-c-source-emacs-source-defun
          anything-c-source-emacs-lisp-expectations
          anything-c-source-emacs-lisp-toplevels
          anything-c-source-org-headline
          anything-c-source-eev-anchor
          ;; Misc:
          anything-c-source-evaluation-result
          anything-c-source-calculation-result
          anything-c-source-google-suggest
          anything-c-source-call-source
          anything-c-source-occur
          anything-c-source-create
          anything-c-source-minibuffer-history
          ;; System:
          anything-c-source-emacs-process))

  (unless (eq window-system 'w32)
    (add-to-list 'anything-sources 'anything-c-source-surfraw t))

  )
(deh-section "mode-line"
  (defun get-lines-4-mode-line ()
    (let ((lines (count-lines (point-min) (point-max))))
      (concat (propertize
               (concat "%l:" (format "%dL" lines))
               'mouse-face 'mode-line-highlight
               ;; make it colorful
               ;; 'face 'mode-line-lines-face
               'help-echo (format "%d lines" lines)) " ")))

  (defun get-size-indication-format ()
    (if (and transient-mark-mode mark-active)
        (format "%d chars" (abs (- (mark t) (point))))
      "%I"))

  (defun get-mode-line-region-face ()
    (and transient-mark-mode mark-active
         (if window-system 'region 'region-invert)))

  (size-indication-mode 1)
  (setq-default mode-line-buffer-identification (propertized-buffer-identification "%b"))

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
         (standard-mode-line-modes
          (list
           " "
           (propertize "%[" 'help-echo recursive-edit-help-echo)
           (propertize "(" 'help-echo help-echo)
           `(:propertize ("" mode-name)
                         help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                         mouse-face mode-line-highlight
                         local-map ,mode-line-major-mode-keymap)
           '("" mode-line-process)
           `(:propertize ("" minor-mode-alist)
                         mouse-face mode-line-highlight
                         help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
                         local-map ,mode-line-minor-mode-keymap)
           (propertize "%n" 'help-echo "mouse-2: Remove narrowing from the current buffer"
                       'mouse-face 'mode-line-highlight
                       'local-map (make-mode-line-mouse-map
                                   'mouse-1 #'mode-line-widen))
           (propertize ")" 'help-echo help-echo)
           (propertize "%]" 'help-echo recursive-edit-help-echo))))
    (setq-default mode-line-modes standard-mode-line-modes)
    (setq-default mode-line-format
                  `("%e%t"
                    mode-line-mule-info
                    mode-line-client
                    mode-line-modified
                    mode-line-remote
                    " "
                    mode-line-buffer-identification
                    ,(propertize " " 'help-echo help-echo)
                    mode-line-position
                    (which-func-mode (" " which-func-format))
                    (vc-mode vc-mode)
                    mode-line-modes
                    (working-mode-line-message (" " working-mode-line-message))
                    ,(propertize "-%-" 'help-echo help-echo))))

  (setq mode-line-format-bak mode-line-format)
  (setq mode-line t)
  (defun toggle-mode-line ()
    "Toggle mode-line."
    (interactive)
    (if mode-line
        (setq-default mode-line-format nil)
      (setq-default mode-line-format mode-line-format-bak))
    (setq mode-line (not mode-line)))
  )

;; sr-speedbar
(deh-require 'sr-speedbar
  ;; (global-set-key (kbd "M-9") 'sr-speedbar-select-window)
  (define-key speedbar-key-map (kbd "M-u") '(lambda () (interactive) (speedbar-up-directory)))

  ;; WORKAROUND: shortkey cofflict, disable view-mode in speedbar
  (setq speedbar-mode-hook '(lambda () (View-exit)))
  ;; add supported extensions
  (dolist (ext (list ".php" ".js" ".css" ".txt" "README" ".jpg" ".png"))
    (speedbar-add-supported-extension ext))

  (add-to-list 'speedbar-fetch-etags-parse-list
               '("\\.php" . speedbar-parse-c-or-c++tag))

  (setq sr-speedbar-skip-other-window-p t
        ;; sr-speedbar-delete-windows t
        sr-speedbar-width-x 22
        sr-speedbar-max-width 30))

;; highlight mode
(deh-require 'highlight-symbol
  (setq highlight-symbol-idle-delay 0.5)
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-interaction-mode-hook
                  java-mode-hook
                  c-mode-common-hook
                  text-mode-hook
                  html-mode-hook))
    (add-hook
     hook (lambda ()
            (highlight-symbol-mode 1)
            (local-set-key (kbd "C-c l l") 'highlight-symbol-at-point)
            (local-set-key (kbd "C-c l u") 'highlight-symbol-remove-all)
            (local-set-key (kbd "C-c l n") 'highlight-symbol-next)
            (local-set-key (kbd "C-c l p") 'highlight-symbol-prev)
            (local-set-key (kbd "C-c l q") 'highlight-symbol-query-replace)
            (local-set-key (kbd "C-c l N") 'highlight-symbol-next-in-defun)
            (local-set-key (kbd "C-c l P") 'highlight-symbol-prev-in-defun)
            ))))

;; shell
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
  (deh-require 'shell-completion
    (setq shell-completion-sudo-cmd "\\(?:sudo\\|which\\)")
    (defvar my-lftp-sites (if (file-exists-p "~/.lftp/bookmarks")
                              (shell-completion-get-file-column "~/.lftp/bookmarks" 0 "[ \t]+")))
    (add-to-list 'shell-completion-options-alist
                 '("lftp" my-lftp-sites))
    (add-to-list 'shell-completion-prog-cmdopt-alist
                 '("lftp" ("help" "open" "get" "mirror" "bookmark")
                   ("open" my-lftp-sites)
                   ("bookmark" "add")))))

;; erc
(deh-section "erc"
  (setq erc-log-channels-directory (expand-file-name "erc" my-temp-dir))
  (eval-after-load "erc"
    '(deh-require 'emoticons
       (add-hook 'erc-insert-modify-hook 'emoticons-fill-buffer)
       (add-hook 'erc-send-modify-hook 'emoticons-fill-buffer)
       (add-hook 'erc-mode-hook
                 (lambda ()
                   (eldoc-mode t)
                   (setq eldoc-documentation-function 'emoticons-help-echo))))))
;;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;; Extra library ;;;;;;;;;;;;;;;;;;
;; Tricks to load feature when needed
(setq
 deh-enable-list
 '(
   ("latex"
    (load "preview-latex.el" t t t)
    (load "auctex.el" t t t)
    (autoload 'CJK-insert-space "cjkspace"
      "Insert tildes appropriately in CJK document." t)
    (defun cjk-toggle-space-tilde (arg)
      (interactive "P")
      (setq CJK-space-after-space
            (if (null arg)
                (not CJK-space-after-space)
              (> (prefix-numeric-value arg) 0)))
      (message "Now SPC will insert %s" (if CJK-space-after-space "SPC" "~")))
    (setq TeX-electric-escape t)
    (add-hook
     'TeX-mode-hook
     (lambda ()
       (auto-fill-mode 1)
       (defun TeX-arg-input-file (optionel &optional prompt local)
         "Prompt for a tex or sty file.

First optional argument is the prompt, the second is a flag.
If the flag is set, only complete with local files."
         (unless (or TeX-global-input-files local)
           (message "Searching for files...")
           (setq TeX-global-input-files
                 (mapcar 'list (TeX-search-files (append TeX-macro-private
                                                         TeX-macro-global)
                                                 TeX-file-extensions t t))))
         (let ((file (if TeX-check-path
                         (completing-read
                          (TeX-argument-prompt optionel prompt "File")
                          (unless local
                            TeX-global-input-files))
                       (read-file-name
                        (TeX-argument-prompt optionel prompt "File")))))
           (if (null file)
               (setq file ""))
           (if (not (string-equal "" file))
               (TeX-run-style-hooks file))
           (TeX-argument-insert file optionel)))
       (my-turn-on-pair-insert '((?$ _ ?$)))
       (define-key LaTeX-mode-map " " 'CJK-insert-space)
       (define-key LaTeX-mode-map "\C-c\C-a" 'cjk-toggle-space-tilde)
       ))
    ;; for XeLaTeX
    (add-hook
     'LaTeX-mode-hook
     (lambda()
       (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
       (TeX-PDF-mode t)
       (setq TeX-command-default "XeLaTeX")
       (setq TeX-save-query nil )
       (setq TeX-show-compilation t)
       ))
    )
   ))

