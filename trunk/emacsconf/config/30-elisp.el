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
       ([return] . 'joc-dired-single-buffer)
       ([mouse-1] . 'joc-dired-single-buffer-mouse)
       ("^"    . '(lambda () (interactive) (joc-dired-single-buffer "..")))
       ("\M-u"  . '(lambda () (interactive) (joc-dired-single-buffer "..")))
       ("z"    . 'ywb-dired-compress-dir)
       ("b"    . 'ywb-list-directory-recursive)
       ("E"    . 'ywb-dired-w3m-visit)
       ("j"    . 'ywb-dired-jump-to-file)
       ("J"    . 'woman-dired-find-file)
       (" "    . 'ywb-dired-count-dir-size)
       ("r"    . 'wdired-change-to-wdired-mode) ; editable mode, 'C-c C-k' abort
       ("W"    . 'ywb-dired-copy-fullname-as-kill)
       ("a"    . 'ywb-add-description)
       ("\C-q" . 'ywb-dired-quickview)
       ("/r"   . 'ywb-dired-filter-regexp)
       ("/."   . 'ywb-dired-filter-extension)
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
        ;; ido-use-filename-at-point 'guess
        ;; ido-use-url-at-point t
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
;;}}}

;;{{{ Ibuffer
(deh-require 'ibuffer
  (require 'ibuf-ext nil t)
  ;; keybinds
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (define-key ibuffer-mode-map "sf" 'ibuffer-do-sort-by-file-name)
  (define-key ibuffer-mode-map "sr" 'ibuffer-do-sort-by-recency)
  (define-key ibuffer-mode-map "r" 'ywb-ibuffer-rename-buffer)
  (define-key ibuffer-mode-map (kbd "C-x C-f") 'ywb-ibuffer-find-file)
  (define-key ibuffer-mode-map " " 'scroll-up)

  (define-ibuffer-sorter file-name
    "Sort buffers by associated file name"
    (:description "file name")
    (apply 'string<
           (mapcar (lambda (buf)
                     (with-current-buffer (car buf)
                       (or buffer-file-name default-directory)))
                   (list a b))))
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
  ;; group buffers
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("*buffers*" (or (mode . term-mode)
                            (mode . twittering-mode)
                            (mode . dired-mode)
                            (mode . w3m-mode)
                            (mode . erc-mode)
                            (name . "^\\*gud")
                            (name . "^\\*scratch")))
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
           ("*others*" (name . "\\*.*\\*")))))
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

;;{{{ tramp
(deh-section "tramp"
  ;; (setq tramp-mode nil)                  ; disable tramp
  (setq tramp-auto-save-directory my-temp-dir
        tramp-persistency-file-name (expand-file-name "tramp" my-temp-dir)))
;;}}}

;;{{{ session management
(deh-require-if 'desktop
  (not (emacs-process-duplicated-p))
  (setq desktop-globals-to-save
        (delq 'tags-table-list desktop-globals-to-save)
        ;; Do not save to desktop
        desktop-buffers-not-to-save
        (concat "\\(" "\\.log\\|\\.diary\\|\\.elc" "\\)$"))

  (setq desktop-base-file-name (concat "emacs.desktop-" (system-name))
        desktop-path (list my-temp-dir)
        desktop-restore-eager 8        ; firstly restore 8 buffers
        history-length 100)

  (add-to-list 'desktop-globals-to-save 'file-name-history)
  (if (boundp 'windata-name-winconf)
      (add-to-list 'desktop-globals-to-save 'windata-named-winconf))

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

  ;;# persist desktop into file every 30 mins
  (run-with-idle-timer 1800 1800 'desktop-save-in-desktop-dir)

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

(deh-require 'bm
  (define-prefix-command 'bm-map-prefix nil "Bm prefix: C-c b")
  (global-set-key (kbd "C-c b") 'bm-map-prefix)
  (deh-define-key bm-map-prefix
    ("b" . 'bm-toggle)
    ("n" . 'bm-next)
    ("p" . 'bm-previous)
    ("s" . 'bm-show)
    ("a" . 'bm-show-all))

  (deh-define-key bm-show-mode-map
    ("n" . 'bm-show-next)
    ("p" . 'bm-show-prev)
    ("d" . 'bm-show-remove-bookmark))

  (setq-default bm-buffer-persistence t)
  (setq bm-repository-file
        (expand-file-name "emacs.bm-repository" my-temp-dir))
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
(deh-require 'recent-jump
  (deh-define-key global-map
    ((kbd "M-[") . 'recent-jump-jump-backward)
    ((kbd "M-]") . 'recent-jump-jump-forward)))

;; recent opened files
(deh-require 'recentf
  ;; recent finded buffers
  (setq recentf-max-saved-items nil)
  (setq recentf-save-file (expand-file-name "emacs.recentf" my-temp-dir))
  (recentf-mode t)

  (defun recentf-open-files-compl ()
    (interactive)
    (let* ((alist (remq nil (mapcar
                             '(lambda (el)
                                (unless (string-match "/$" el) ; skip dired
                                  (cons (file-name-nondirectory el) el)))
                             recentf-list)))
           ;; use `ido-completing-read' instead of `completing-read'
           (filename (ido-completing-read "Open file: "
                                          (mapcar 'car alist))))
      (find-file (cdr (assoc filename alist)))))
  ;; (global-set-key (kbd "C-x C-o") 'recentf-open-files-compl)

  ;; Also store recent opened directories besides files
  (add-hook 'dired-mode-hook
            (lambda () (recentf-add-file dired-directory))))
;;}}}

;;{{{ pager
(deh-require 'pager
  (global-set-key (kbd "C-v") 'pager-page-down)
  (global-set-key (kbd "M-v") 'pager-page-up)
  (global-set-key (kbd "<up>") 'pager-row-up)
  (global-set-key (kbd "M-p") 'pager-row-up)
  (global-set-key (kbd "<down>") 'pager-row-down)
  (global-set-key (kbd "M-n") 'pager-row-down)
  ;; Some individual keybinds
  (add-hook 'Man-mode-hook
            (lambda ()
              (define-key Man-mode-map (kbd "M-p") 'pager-row-up)
              (define-key Man-mode-map (kbd "M-n") 'pager-row-down)))
  (add-hook 'Info-mode-hook
            (lambda ()
              (define-key Info-mode-map (kbd "M-p") 'pager-row-up)
              (define-key Info-mode-map (kbd "M-n") 'pager-row-down)))
  )
;;}}}

;;{{{ ffap
(deh-section "ffap"
  (autoload 'ffap "ffap" "Alias of find-file-at-point")
  ;; for windows path recognize
  (setq ffap-string-at-point-mode-alist
        '((file "--{}:\\\\$+<>@-Z_a-z~*?\x100-\xffff" "<@" "@>;.,!:")
          (url "--:=&?$+@-Z_a-z~#,%;*" "^A-Za-z0-9" ":;.,!?")
          (nocolon "--9$+<>@-Z_a-z~" "<@" "@>;.,!?")
          (machine "-a-zA-Z0-9." "" ".")
          (math-mode ",-:$+<>@-Z_a-z~`" "<" "@>;.,!?`:"))))
;;}}}

;;{{{ View
(deh-section-after "view"
  (deh-define-key view-mode-map
    ("h" . 'backward-char)
    ("l" . 'forward-char)
    ("j" . 'View-scroll-line-forward)
    ("k" . 'View-scroll-page-backward)
    ("G" . 'View-goto-line-last)
    ("b" . 'View-scroll-page-backward)
    ("f" . 'View-scroll-page-forward)
    ("?" . 'View-search-regexp-backward)))
;;}}}

;;{{{ standard libraries and settings
(deh-section "std-lib"
  (partial-completion-mode 1)
  (icomplete-mode 1)
  (winner-mode 1)
  ;; (auto-insert-mode 1)

  ;; TimeStamp
  (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-active t)
  (setq time-stamp-warn-inactive t)

  (deh-add-hook occur-mode-hook (setq truncate-lines t))

  ;; tetris game
  ;; (setq tetris-update-speed-function (lambda (shapes rows) (/ 10.0 (+ 80.0 rows))))

  ;; woman
  (setq woman-use-own-frame nil)
  (setq woman-fontify t)                ; dump emacs need this
  (autoload 'woman-mode "woman")
  (add-hook 'woman-mode-hook 'view-mode)
  (autoload 'woman-decode-buffer "woman")

  ;; change-log
  (deh-add-hook change-log-mode-hook
    (auto-fill-mode t)
    (add-to-list 'change-log-font-lock-keywords
                 '("^[0-9-]+:? +\\|^\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\) [A-z][a-z][a-z] [0-9:+ ]+"
                   (0 'change-log-date-face)
                   ("\\([^<(]+?\\)[   ]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]" nil nil
                    (1 'change-log-name)
                    (2 'change-log-email)))))

  ;; generic-x
  (require 'generic-x)

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
       ("^D.*" . font-lock-type-face))))

  (deh-define-key Info-mode-map
    ("j" . 'next-line)
    ("k" . 'previous-line))
  ;; (filesets-init)
  (defalias 'default-generic-mode 'conf-mode)
  )

;; formats and timestamp
(deh-section "formats"
  (setq frame-title-format
        '((:eval
           (let ((login-name (getenv-internal "LOGNAME")))
             (if login-name (concat login-name "@") "")))
          (:eval (system-name))
          ":"
          (:eval (or (buffer-file-name) (buffer-name)))))

  ;; (setq display-time-format "%m月%d日 星期%a %R")
  ;; (setq time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S %:a by %u")
  (setq time-stamp-format "%U %:y-%02m-%02d %02H:%02M:%02S"))

;; unique buffers' name
(deh-require 'uniquify
  (setq uniquify-buffer-name-style 'forward)
  ;; (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
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
(deh-require-if 'w3m-load (executable-find "w3m")
  (setq w3m-verbose t)                  ; log in *Messages*
  (deh-add-hook w3m-mode-hook
    (local-unset-key "\C-xb")
    (local-unset-key (kbd "S-SPC"))
    (deh-define-key w3m-mode-map
      ("n" . (lambda nil (interactive) (ywb-w3m-goto-url w3m-next-url)))
      ("p" . (lambda nil (interactive) (ywb-w3m-goto-url w3m-previous-url)))
      ("t" . (lambda nil (interactive) (ywb-w3m-goto-url w3m-contents-url)))))
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
;;}}}

;;{{{ autopair, like skeleton
(deh-require 'autopair
  (deh-add-hooks (java-mode-hook
                  c-mode-common-hook
                  python-mode-hook
                  emacs-lisp-mode-hook
                  html-mode-hook)
    (autopair-mode))
  ;; some tricks
  (deh-add-hook c++-mode-hook
    (push ? (getf autopair-dont-pair :comment))
    ;; (push '(?< . ?>) (getf autopair-extra-pairs :code))
    )
  (deh-add-hook emacs-lisp-mode-hook
    (push '(?` . ?') (getf autopair-extra-pairs :comment))
    (push '(?` . ?') (getf autopair-extra-pairs :string)))
  )
;;}}}

;;{{{ auto-complete
(deh-section "auto-complete"
  (require 'auto-complete-config)
  ;; specify a file stores data of candidate suggestion
  (setq ac-comphist-file (expand-file-name "ac-comphist.dat" my-temp-dir))
  (setq ac-candidate-limit ac-menu-height ; improve drop menu performance
        ac-auto-start 3
        ac-auto-show-menu 1.5
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

  ;; for terminal, works well with `global-hl-line-mode'
  (if (null window-system)
      (set-face-background 'ac-completion-face "blue"))

  (add-to-list 'ac-dictionary-directories
               (expand-file-name "ac-dict" my-startup-dir))
  (add-to-list 'ac-user-dictionary-files
               (expand-file-name "ac.dict" my-startup-dir))

  (add-to-list 'ac-modes 'org-mode)

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
                               ac-source-semantic-raw
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

;;}}}

;;{{{ Yet Another Snippet -  pluskid@newsmth
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
        (yas/load-snippet-buffer))))
)
;;}}}

;;{{{ a simple template
(deh-require 'template-simple
  (setq template-directory-list (list my-template-dir)
        template-skip-directory-list (list my-temp-dir my-template-dir)))
;;}}}

;;{{{ isearch tweaks
(deh-section "isearch"
  (deh-define-key isearch-mode-map
    ("\t" . 'isearch-complete)
    ("\M-<" . 'isearch-beginning-of-buffer)
    ("\M->" . 'isearch-end-of-buffer)
    ("\M-i" . 'isearch-query-replace-current)
    ("\C-u" . 'isearch-clean)
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
  (defun isearch-yank-symbol ()
    "Put symbol at current point into search string."
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

  ;; Search word at point
  (defun isearch-word-at-point ()
    (interactive)
    (call-interactively 'isearch-forward-regexp))
  (defun isearch-yank-word-hook ()
    (when (equal this-command 'isearch-word-at-point)
      (let ((string (concat "\\<"
                            (buffer-substring-no-properties
                             (progn (skip-syntax-backward "w_") (point))
                             (progn (skip-syntax-forward "w_") (point)))
                            "\\>")))
        (if (and isearch-case-fold-search
                 (eq 'not-yanks search-upper-case))
            (setq string (downcase string)))
        (setq isearch-string string
              isearch-message
              (concat isearch-message
                      (mapconcat 'isearch-text-char-description
                                 string ""))
              isearch-yank-flag t)
        (isearch-search-and-update))))
  (add-hook 'isearch-mode-hook 'isearch-yank-word-hook)

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
  ;; info+
  (eval-after-load "info"
    '(require 'info+))
  ;; for normal term
  ;; (add-hook 'term-mode-hook 'kill-buffer-when-shell-command-exit)
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

;; Enhanced ansi-term
(deh-require 'multi-term
  ;; compatible with normal terminal keybinds
  (add-to-list 'term-bind-key-alist '("<M-backspace>" . term-send-backward-kill-word))
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

  (define-prefix-command 'multi-term-prefix nil "Multi-term prefix: C-c t")
  (global-set-key (kbd "C-c t") 'multi-term-prefix)
  (deh-define-key multi-term-prefix
    ("c" . 'multi-term)
    ("t" . 'multi-term-dedicated-open-select)
    ("q" . 'multi-term-dedicated-close)
    ("s" . 'multi-term-dedicated-select)
    ("g" . 'multi-term-dedicated-toggle))

  (defun multi-term-dedicated-open-select ()
    (interactive)
    (unless (multi-term-dedicated-exist-p)
      (multi-term-dedicated-open))
    (multi-term-dedicated-select)))

;; browse-kill-ring
(deh-require 'browse-kill-ring
  (browse-kill-ring-default-keybindings))

;; fold content
;; (deh-require 'fold
;;   (setq fold-mode-prefix-key "\C-c\C-o")
;;   (setq fold-autoclose-other-folds nil)
;;   (add-hook 'find-file-hook 'fold-find-file-hook t))

;; (deh-section "linum"
;;   (setq linum-format (concat (propertize "%6d " 'face 'default)
;;                              (propertize " " 'face 'fringe)))
;;   (autoload 'linum-mode "linum" "Display line number" t))

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

  (deh-require-if 'anything-config (executable-find "w3m")
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
      (add-to-list 'anything-sources 'anything-c-source-surfraw t)))
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


(deh-section "ediff"
  ;; (global-set-key "\C-cd" 'ediff-show-registry)
  (setq diff-switches "-ubB"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(deh-require 'highlight-parentheses
  (setq hl-paren-colors '("red" "yellow" "cyan" "magenta" "green" "red"))
  (deh-add-hooks (emacs-lisp-mode-hook
                  java-mode-hook
                  c-mode-common-hook)
    (highlight-parentheses-mode 1))
  )

;; Highlight current line
;; (global-hl-line-mode)
;; (set-face-background 'hl-line "white smoke") ; list-colors-display

(deh-require 'highlight-symbol
  (deh-add-hooks (emacs-lisp-mode-hook
                  java-mode-hook
                  c-mode-common-hook)
    (when window-system
      (highlight-symbol-mode 1)
      (setq highlight-symbol-idle-delay 0.5
            highlight-symbol-mode nil)))

  (deh-local-set-keys (emacs-lisp-mode-hook
                       java-mode-hook
                       c-mode-common-hook
                       text-mode-hook
                       html-mode-hook)
    ((kbd "C-c l l") . 'highlight-symbol-at-point)
    ((kbd "C-c l u") . 'highlight-symbol-remove-all)
    ((kbd "C-c l n") . 'highlight-symbol-next)
    ((kbd "C-c l p") . 'highlight-symbol-prev)
    ((kbd "C-c l q") . 'highlight-symbol-query-replace)
    ((kbd "C-c l N") . 'highlight-symbol-next-in-defun)
    ((kbd "C-c l P") . 'highlight-symbol-prev-in-defun)
    )
  )

(deh-section "sh-mode"
  (deh-add-hook sh-mode-hook
    (local-unset-key "\C-c\C-o")        ; trigger for `sh-while-getopts'
    ))

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
(deh-section-reserved "latex"
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
  (deh-add-hook LaTeX-mode-hook
    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
    (TeX-PDF-mode t)
    (setq TeX-command-default "XeLaTeX")
    (setq TeX-save-query nil )
    (setq TeX-show-compilation t)
    ))

(deh-require-if 'evernote-mode
  (executable-find "enclient.rb")
  ;; (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option
  (global-set-key "\C-cec" 'evernote-create-note)
  (global-set-key "\C-ceo" 'evernote-open-note)
  (global-set-key "\C-ces" 'evernote-search-notes)
  (global-set-key "\C-ceS" 'evernote-do-saved-search)
  (global-set-key "\C-cew" 'evernote-write-note)
  (global-set-key "\C-cep" 'evernote-post-region)
  (global-set-key "\C-ceb" 'evernote-browser)
  (setq evernote-mode-hook
        '(lambda () (outline-minor-mode t)))
  )
