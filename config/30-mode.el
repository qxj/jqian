;; -*- mode: Emacs-Lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end

;;; common mode hook
(deh-section mode-common
  (setq-default tab-width 4
                c-basic-offset tab-width
                indent-tabs-mode nil
                c-hungry-delete-key t)

  (defun my-mode-common-hook ()

    )

  (defun my-prog-mode-hook ()           ; programming hook
    (set (make-local-variable 'comment-style) 'indent)
    (set (make-local-variable 'tab-stop-list)
         (number-sequence tab-width 80 tab-width))

    (abbrev-mode 1)
    (subword-mode 1)
    ;; (when (fboundp 'whitespace-mode) (whitespace-mode t))
    (hs-minor-mode 1)
    (turn-on-eldoc-mode)
    (toggle-truncate-lines 1)
    (ignore-errors (imenu-add-menubar-index))

    ;; (local-set-key (kbd "RET")
    ;;                (lambda () (interactive)
    ;;                  (if (my-cursor-on-comment-p) (comment-indent-new-line)
    ;;                    (if (boundp 'autopair-newline) (autopair-newline)
    ;;                      (newline-and-indent)))))

    (deh-add-hook before-save-hook
      (when (> 3000 (count-lines (point-min) (point-max)))
        (delete-trailing-whitespace)        ; no trailing whitespace
        (if (reduce (lambda (a b) (or a b))
                    (mapcar (lambda (x) (eq major-mode x)) '(c-mode c++-mode python-mode php-mode)))
            (my-untabify)                       ; untabify source code
          )
        (my-update-header)                  ; update header
        (copyright-update)                  ; update copyright
        (time-stamp)                        ; update timestamp
        ))
    )

  ;; (add-hook 'prog-mode-hook 'my-prog-mode-hook)

  (defun my-text-mode-hook ()           ; literal hook
    (abbrev-mode 1)
    (outline-minor-mode 1)
    (toggle-truncate-lines 1)
    (turn-off-auto-fill)
    ;; (auto-fill-mode 1)
    )

  ;; comment new line and indent `M-j', as VIM acts.
  (defun my-cursor-on-comment-p (&optional point)
    (memq (get-text-property (or point (point)) 'face)
          '(font-lock-comment-face)))

  (defun my-untabify ()
    "My untabify function as discussed and described at
 http://www.jwz.org/doc/tabs-vs-spaces.html
 and improved by Claus Brunzema:
 - return nil to get `write-contents-hooks' to work correctly
   (see documentation there)
 - when instead of if
 Use some lines along the following for getting this to work in the
 modes you want it to:

 \(add-hook 'some-mode-hook
           '(lambda ()
                (add-hook 'write-contents-hooks 'my-untabify nil t)))"
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "\t" nil t)
        (untabify (1- (point)) (point-max)))
      nil))

  ;;# copy from template-simple.el
  (defun my-update-header ()
    (interactive)
    (when (and buffer-file-name
               (not (string-match (regexp-opt (list my-data-dir my-template-dir)) buffer-file-name)))
      (save-excursion
        (goto-char (point-min))
        (let ((end (progn (forward-line 3) (point))) ; check only first 3 lines
              (regexp "@(#)\\([^ \t\n]+\\)")
              (fn (file-name-sans-versions (file-name-nondirectory buffer-file-name))))
          (goto-char (point-min))
          (while (search-forward-regexp regexp end t)
            (and (not (string= (match-string 1) fn))
                 (y-or-n-p (format "Update file header %s to %s? "
                                   (match-string 1) fn))
                 (replace-match fn nil t nil 1)))))))
  )

;;; tags
(deh-package ebrowse
  :disabled
  :config
  (add-to-list 'auto-mode-alist '("BROWSE\\.*" . ebrowse-tree-mode))
  (setq ebrowse-global-prefix-key "\C-z"))

(deh-package etags
  :disabled
  :config
  (defun my-find-top-directory (file &optional dir)
    (or dir (setq dir (expand-file-name default-directory)))
    (let ((thefile (expand-file-name file dir)))
      (if (file-exists-p thefile)
          thefile
        (setq pdir (directory-file-name (file-name-directory dir)))
        (if (string= pdir dir)
            nil
          (my-find-top-directory file pdir)))))
  (setq tags-add-tables nil
        default-tags-table-function
        (lambda nil
          (my-find-top-directory "TAGS"))))

(deh-package gtags
  :disabled
  :commands (gtags-mode)
  :if (executable-find "global")
  :config
  (deh-add-hook (c-mode-common-hook)
    (when (derived-mode-p 'c-mode 'c++-mode)
      (gtags-mode t)))

  (setq gtags-mode-hook
        '(lambda ()
           (setq gtags-pop-delete t)
           (setq gtags-path-style 'absolute)))

  (setq gtags-select-mode-hook
        '(lambda ()
           (set (make-local-variable 'hl-line-face) 'underline)
           (hl-line-mode 1)))

  (defun gtags-root-dir ()
    "Returns GTAGS root directory or nil if doesn't exist."
    (with-temp-buffer
      (if (zerop (call-process "global" nil t nil "-pr"))
          (buffer-substring (point-min) (1- (point-max))) nil)))
  (defun gtags-update-single (filename)
    "Update Gtags database for changes in a single file"
    (interactive)
    (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))
  (defun gtags-update-current-file ()
    (interactive)
    (defvar filename)
    (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
    (gtags-update-single filename)
    (message "Gtags updated for %s" filename))
  (defun gtags-update-hook ()
    "Update GTAGS file incrementally upon saving a file"
    (when gtags-mode
      (when (gtags-root-dir)
        (gtags-update-current-file))))

  (add-hook 'after-save-hook 'gtags-update-hook)

  (defun gtags-append-tags ()
    (interactive)
    (if gtags-mode
        (progn
          (message "start to global -u")
          (start-process "gtags-name" "*gtags-var*" "global" "-u"))))

  (bind-keys
   :map gtags-mode-map
   ;; Instead of `find-tag' & `pop-tag-mark'
   ("M-." . gtags-find-tag)
   ("M-*" . gtags-pop-stack)
   ;; other key binds
   ("C-c g v"  . gtags-visit-rootdir)
   ("C-c g t"  . gtags-find-tag-from-here)
   ("C-c g o"  . gtags-find-tag-other-window)
   ("C-c g r"  . gtags-find-rtag)
   ("C-c g s"  . gtags-find-symbol)
   ("C-c g p"  . gtags-find-pattern)
   ("C-c g g"  . gtags-find-with-grep)
   ("C-c g i"  . gtags-find-with-idutils)
   ("C-c g f"  . gtags-find-file)
   ("C-c g a"  . gtags-parse-file)
   ("C-c g b"  . gtags-append-tags))
  )

(deh-package ggtags
  :commands (ggtags-mode)
  :if (executable-find "global")
  :config
  (deh-add-hook 'c-mode-common-hook
    (when (derived-mode-p 'c-mode 'c++-mode)
      (ggtags-mode 1))))

(deh-package xcscope
  :disabled
  :config
  (setq cscope-database-regexps
        '(
          ("^/home/jqian/nbusrc"
           (t)
           ("/home/jqian/tags/")
           ("/home/jqian/")
           t
           ("/net/code/srt/nb_sync/MAIN/cscope" ("-d")))
          ("^/home/jqian/projects"
           (t)
           ("/home/jqian/projects" ("-d" "-I/usr/local/include")))
          ))
  (setq cscope-do-not-update-database t
        cscope-adjust nil)
  ;; keybinds
  (setq cscope-minor-mode-hooks
        '(lambda ()
           ;; Instead of `find-tag' & `pop-tag-mark'
           (deh-define-key cscope:map
                           ((kbd "M-.") 'cscope-find-this-symbol)
                           ((kbd "M-*") 'cscope-pop-mark))
           ;; Key bind for cscope-minor-mode
           ))
  ;; hack `xcscope.el', remove hooks
  (deh-remove-hook (c-mode-hook c++-mode-hook dired-mode-hook)
    (function cscope:hook)))

(deh-package browse-el
  :config
  (bind-keys
   :map emacs-lisp-mode-map
   ("M-." . browse-el-find-funtion)
   ("M-*" . browse-el-go-back)))

(deh-package tags-view
  :disabled
  :config
  (bind-keys
   :map tags-history-mode-map
   ("q" . tv-view-history-quit))

  (defvar tv-previous-window-conf nil
    "Window configuration before switching to tv buffer.")
  (defun tv-view-history ()
    "The main entry point; pops open a buffer with the list of
locations on the tag stack that can then optionally be operated
on (eg, jumping to that location, deleting it from the list,
etc).  The following options will be available:

\\{tags-history-mode-map}"
    (interactive)
    ;; save windows configuration
    (setq gtags-previous-window-conf (current-window-configuration))
    (let* ((buf (get-buffer-create "*tags history*"))
           (backend (tv-determine-backend))
           (tag-items (tv-get-tags-list backend)))
      (pop-to-buffer buf nil t)
      (setq buffer-read-only nil)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (tags-history-mode)
      (set (make-local-variable 'tv-tags-backend) backend)
      (tv-insert-items tag-items)
      (setq buffer-read-only t)
      (goto-char 0)))
  (defun tv-jump-to-tag-and-quit (location)
    "Jump to tag location and quit tags view history."
    (interactive "d")
    (with-tag-info location (buf posn stack)
                   (switch-to-buffer buf)
                   (goto-char posn)
                   (if (window-configuration-p gtags-previous-window-conf)
                       (progn
                         (bury-buffer)
                         (set-window-configuration gtags-previous-window-conf)
                         (setq gtags-previous-window-conf nil)))))
  (defun tv-view-history-quit ()
    "Quit tags view history buffer."
    (interactive)
    (if (window-configuration-p gtags-previous-window-conf)
        (progn
          (bury-buffer)
          (set-window-configuration gtags-previous-window-conf)
          (setq gtags-previous-window-conf nil))))
  )


(deh-package woman
  :commands (woman woman-find-file)
  :config
  ;; (add-hook 'woman-mode-hook 'view-mode)

  (setq woman-cache-filename (expand-file-name "emacs.wmncach.el" my-data-dir)
        woman-manpath '("/usr/man"
                        "/usr/share/man"
                        "/usr/X11R6/man"
                        "/usr/local/man"
                        "/usr/share/man/zh_TW"
                        "/usr/share/man/zh_CN")
        woman-manpath-man-regexp (regexp-opt '("man2" "man3" "man7"))
        woman-imenu t
        woman-fontify t
        woman-use-own-frame nil))

(deh-package info
  :commands (Info-mode)
  :config
  (add-to-list 'Info-default-directory-list "~/info")

  (bind-keys
   :map Info-mode-map
   ;;# useful keybind reminds
   ;; ("i" . 'info-index)
   ;; ("T" . 'info-toc)
   ("j" 'next-line)
   ("k" 'previous-line))
  (deh-package info+
    :config
    (setq Info-fit-frame-flag nil))

  (define-mode-toggle "info" info
    (derived-mode-p 'Info-mode))
  )

(deh-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :config
  ;; flyspell-goto-next-error: `C-,'
  ;; (ispell-change-dictionary)

  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))

  ;; (deh-add-hook (text-mode-hook org-mode-hook) (flyspell-mode 1))
  ;; (deh-add-hook (change-log-mode-hook log-edit-mode-hook) (flyspell-mode -1))
  ;; (deh-add-hook (c-mode-common-hook python-mode-hook) (flyspell-prog-mode))
  )

(deh-package flymake
  :commands (flymake-mode)
  :config
  (setq flymake-gui-warnings-enabled nil
        flymake-allowed-file-name-masks '()
        flymake-log-level 0
        flymake-no-changes-timeout 5.0)

  ;; (deh-add-hook 'find-file-hook
  ;;   (condition-case nil (flymake-find-file-hook) (error nil)))

  (defvar flymake-mode-map (make-sparse-keymap))
  (bind-keys
   :map flymake-mode-map
   ("C-c <f4>"    . flymake-goto-next-error-disp)
   ("C-c <S-f4>"  . flymake-goto-prev-error-disp)
   ("C-c <C-f4>"  . flymake-display-err-menu-for-current-line))
  (or (assoc 'flymake-mode minor-mode-map-alist)
      (setq minor-mode-map-alist
            (cons (cons 'flymake-mode flymake-mode-map)
                  minor-mode-map-alist)))

  (defun flymake-display-current-error ()
    "Display errors/warnings under cursor."
    (interactive)
    (let ((ovs (overlays-in (point) (1+ (point)))))
      (catch 'found
        (dolist (ov ovs)
          (when (flymake-overlay-p ov)
            (message (overlay-get ov 'help-echo))
            (throw 'found t))))))
  (defun flymake-goto-next-error-disp ()
    "Go to next error in err ring, then display error/warning."
    (interactive)
    (flymake-goto-next-error)
    (flymake-display-current-error))
  (defun flymake-goto-prev-error-disp ()
    "Go to previous error in err ring, then display error/warning."
    (interactive)
    (flymake-goto-prev-error)
    (flymake-display-current-error))

  ;; Directly use gcc instead of Makefile
  (when (executable-find "texify")
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.tex\\'" flymake-simple-tex-init))
    (add-to-list 'flymake-allowed-file-name-masks
                 '("[0-9]+\\.tex\\'"
                   flymake-master-tex-init flymake-master-cleanup)))
  (when (executable-find "xml")
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.xml\\'" flymake-xml-init))
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.html?\\'" flymake-xml-init)))
  (when (executable-find "perl")
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.p[ml]\\'" flymake-perl-init)))
  (when (executable-find "php")
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.php[345]?\\'" flymake-php-init)))
  (when (executable-find "make")
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.idl\\'" flymake-simple-make-init))
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.java\\'"
                   flymake-simple-make-java-init flymake-simple-java-cleanup))
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.cs\\'" flymake-simple-make-init)))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.el$" flymake-elisp-init))
  ;; (add-hook 'write-file-functions (lambda nil
  ;;                                   (when (eq major-mode 'emacs-lisp-mode)
  ;;                                     (check-parens))))
  (defun flymake-elisp-init ()
    (if (string-match "^ " (buffer-name))
        nil
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list
         (expand-file-name invocation-name invocation-directory)
         (list
          "-Q" "--batch" "--eval"
          (prin1-to-string
           (quote
            (dolist (file command-line-args-left)
              (with-temp-buffer
                (insert-file-contents file)
                (emacs-lisp-mode)
                (condition-case data
                    (scan-sexps (point-min) (point-max))
                  (scan-error
                   (goto-char(nth 2 data))
                   (princ (format "%s:%s: error: Unmatched bracket or quote\n"
                                  file (line-number-at-pos)))))))))
          local-file)))))

;;;; flymake with make/gcc
  (defvar flymake-makefile-filenames '("Makefile" "makefile" "GNUmakefile")
    "File names for make.")
  (defun flymake-get-gcc-cmdline (source base-dir)
    (let ((cc (if (string= (file-name-extension source) "c") "gcc" "g++")))
      (list cc
            (list "-Wall"
                  "-Wextra"
                  "-pedantic"
                  "-fsyntax-only"
                  "-I.."
                  "-I../include"
                  "-I../inc"
                  "-I../common"
                  "-I../public"
                  "-I../.."
                  "-I../../include"
                  "-I../../inc"
                  "-I../../common"
                  "-I../../public"
                  source))))
  (defun flymake-init-find-makfile-dir (source-file-name)
    "Find Makefile, store its dir in buffer data and return its dir, if found."
    (let* ((source-dir (file-name-directory source-file-name))
           (buildfile-dir nil))
      (catch 'found

        (dolist (makefile flymake-makefile-filenames)
          (let ((found-dir (flymake-find-buildfile makefile source-dir)))
            (when found-dir
              (setq buildfile-dir found-dir)
              (setq flymake-base-dir buildfile-dir)
              (throw 'found t)))))
      buildfile-dir))
  (defun flymake-simple-make-gcc-init-impl (create-temp-f
                                            use-relative-base-dir
                                            use-relative-source)
    "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
    (let* ((args nil)
           (source-file-name buffer-file-name)
           (source-dir (file-name-directory source-file-name))
           (buildfile-dir
            (and (executable-find "make")
                 (flymake-init-find-makfile-dir source-file-name)))
           (cc (if (string= (file-name-extension source-file-name) "c")
                   "gcc"
                 "g++")))
      (if (or buildfile-dir (executable-find cc))
          (let* ((temp-source-file-name
                  (ignore-errors
                    (flymake-init-create-temp-buffer-copy create-temp-f))))
            (if temp-source-file-name
                (setq args
                      (flymake-get-syntax-check-program-args
                       temp-source-file-name
                       (if buildfile-dir buildfile-dir source-dir)
                       use-relative-base-dir
                       use-relative-source
                       (if buildfile-dir
                           'flymake-get-make-cmdline
                         'flymake-get-gcc-cmdline)))
              (flymake-report-fatal-status
               "TMPERR"
               (format "Can't create temp file for %s" source-file-name))))
        (flymake-report-fatal-status
         "NOMK" (format "No buildfile (%s) found for %s, or can't found %s"

                        "Makefile" source-file-name cc)))
      args))
  (defun flymake-simple-make-gcc-init ()
    (flymake-simple-make-gcc-init-impl 'flymake-create-temp-inplace t t))
  (defun flymake-master-make-gcc-init (get-incl-dirs-f
                                       master-file-masks
                                       include-regexp)
    "Create make command line for a source file
 checked via master file compilation."
    (let* ((args nil)
           (temp-master-file-name
            (ignore-errors

              (flymake-init-create-temp-source-and-master-buffer-copy
               get-incl-dirs-f
               'flymake-create-temp-inplace
               master-file-masks
               include-regexp)))
           (cc (if (string= (file-name-extension buffer-file-name) "c")
                   "gcc"
                 "g++")))
      (if temp-master-file-name
          (let* ((source-file-name buffer-file-name)
                 (source-dir (file-name-directory source-file-name))
                 (buildfile-dir
                  (and (executable-find "make")
                       (flymake-init-find-makfile-dir source-file-name))))
            (if (or buildfile-dir (executable-find cc))
                (setq args (flymake-get-syntax-check-program-args
                            temp-master-file-name
                            (if buildfile-dir buildfile-dir source-dir)
                            nil
                            nil
                            (if buildfile-dir
                                'flymake-get-make-cmdline
                              'flymake-get-gcc-cmdline)))
              (flymake-report-fatal-status
               "NOMK"
               (format "No buildfile (%s) found for %s, or can't found %s"
                       "Makefile" source-file-name cc))))
        (flymake-report-fatal-status
         "TMPERR" (format "Can't create temp file for %s" source-file-name)))
      args))
  (defun flymake-master-make-gcc-header-init ()
    (flymake-master-make-gcc-init
     'flymake-get-include-dirs
     '("\\.cpp\\'" "\\.c\\'")
     "[ \t]*#[ \t]*include[ \t]*\"\\([[:word:]0-9/\\_.]*%s\\)\""))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.\\(?:h\\(?:pp\\)?\\)\\'"
                 flymake-master-make-gcc-header-init flymake-master-cleanup))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'"
                 flymake-simple-make-gcc-init))
  )

(deh-package flycheck
  :commands (flycheck-mode global-flycheck-mode)
  :init
  (dolist (mode '(sh-mode-hook python-mode-hook c-mode-common-hook))
    (add-hook mode 'flycheck-mode))
  :config
  ;;# rebind flycheck prefix key
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)

  ;;# workaround to avoid eldoc override flycheck error message
  (setq flycheck-display-errors-delay 1.1)
  ;; (setq flycheck-indication-mode 'right-fringe)

  ;; python code style
  ;; flake8 works with git:
  ;;     http://flake8.readthedocs.org/en/latest/vcs.html#git-hook
  (setq flycheck-python-flake8-executable (executable-find "flake8"))

  (deh-package flycheck-google-cpplint
    :config
    ;; Add Google C++ Style checker.
    ;; In default, syntax checked by Clang and Cppcheck.
    (flycheck-add-next-checker 'c/c++-cppcheck
                               '(warning . c/c++-googlelint))
    (setq flycheck-c/c++-googlelint-executable
          (expand-file-name "misc/cpplint.py" my-startup-dir)
          flycheck-googlelint-linelength "100")))

(deh-section makefile
  (deh-add-hook makefile-mode-hook
    ;; (my-code-common-hook)
    ))

(deh-package change-log
  :config
  (deh-add-hook change-log-mode-hook
    (auto-fill-mode t)
    (add-to-list 'change-log-font-lock-keywords
                 '("^[0-9-]+:? +\\|^\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\) [A-z][a-z][a-z] [0-9:+ ]+"
                   (0 'change-log-date-face)
                   ("\\([^<(]+?\\)[   ]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]" nil nil
                    (1 'change-log-name)
                    (2 'change-log-email)))))
  )

(deh-package lisp-mode
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode ("Cask" . emacs-lisp-mode)
  :config
  (if (featurep 'ffap)
      (add-to-list 'ffap-alist '(lisp-interaction-mode . ffap-el-mode)))

  (deh-add-hook emacs-lisp-mode-hook
    (my-prog-mode-hook))

  (bind-keys
   :map emacs-lisp-mode-map ; lisp-mode-shared-map
   ("\M-."  . browse-el-find-funtion)
   ("\M-*"  . browse-el-go-back)
   ("C-)"   . my-auto-insert-paren))

  (defun my-auto-insert-paren ()
    "Auto close matched parentheses."
    (interactive)
    (condition-case nil
        (progn
          (scan-sexps (point) -1)
          (insert ")")
          (my-auto-insert-paren))
      (error (delete-char -1))))
  )

;;; scripts setting
(deh-package python
  :config
  ;; Start an inferior python process, wordaround for eldoc error
  (let ((python-cmd (if (executable-find python-shell-interpreter)
                        (python-shell-parse-command))))
    (if python-cmd (run-python python-cmd t nil)))

  (deh-add-hook python-mode-hook
    (my-prog-mode-hook)
    ;; Avoid mysterious "Arithmetric error", bug:
    ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=15975
    (setq python-indent-offset 4)
    (when (boundp 'rope-completions) (ac-ropemacs-initialize))
    (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p nil t)
    )

  (deh-package doctest-mode)

  (use-package flymake-python-pyflakes
    :if (executable-find "pyflakes")
    :config (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

  ;; # Preparation:
  ;; 1. $ sudo pip install jedi epc sexpdata
  ;; 2. emacs-epc, emacs-deferred, emacs-ctable (copy .el into `load-path`)
  ;; 3. download and uncompress emacs-jedi, then `make requirements` (depends on virtualenv)
  ;;
  ;; http://tkf.github.io/emacs-jedi/released/#install
  ;;
  (deh-package jedi
    :commands (jedi:setup jedi-direx:pop-to-buffer)
    :init
    (setq jedi:setup-keys t             ;set it before jedi loaded
          jedi:complete-on-dot t
          jedi:tooltip-method nil
          jedi:use-shortcuts t
          jedi:install-imenu t)
    (add-hook 'python-mode-hook 'jedi:setup))

  ;;# Preparation:
  ;;
  ;; 1. install pymacs, rope, ropemode, ropemacs one by one.
  ;; 2. put pymacs.el (pymacs) into `load-path'.
  ;; 3. check whether pymacs is working.
  ;;
  ;; http://pymacs.progiciels-bpi.ca/pymacs.html#check-if-pymacs-would-work
  ;;
  ;;# Useful features of ropemacs:
  ;;
  ;; 1. full code completion of modules/classes/methods (M-/)
  ;; 2. instant documentation for element under cursor (C-c d)
  ;; 3. jump to modules/classes/methods definition (C-c g)
  ;; 4. refactor (rename: C-c r r)
  ;; 5. list all occurences of a name in your entire project
  ;;
  ;; http://rope.sourceforge.net/ropemacs.html
  ;;
  (deh-package pymacs
    :disabled
    :commands (pymacs-apply pymacs-call pymacs-eval pymacs-exec pymacs-load)
    :config
    (pymacs-load "ropemacs" "rope-")
    (setq ropemacs-confirm-saving nil
          ropemacs-enable-autoimport t)
    (ropemacs-mode t)
    ;; (deh-package pycomplete)
    )
  )

(deh-package sh-script
  :defer
  :config
  (deh-add-hook sh-mode-hook
    ;; (local-unset-key "\C-c\C-o")        ; trigger for `sh-while-getopts'
    (my-prog-mode-hook)
    (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p nil t)
    ))

(deh-package text-mode
  :defer
  :config
  (deh-add-hook text-mode-hook my-text-mode-hook))

;;; tools
(deh-package gnuplot
  :commands (gnuplot-mode)
  :mode "\\.gp$"
  :config
  (deh-add-hook gnuplot-after-plot-hook
    (select-window (get-buffer-window gnuplot-comint-recent-buffer)))
  (deh-add-hook gnuplot-comint-setup-hook
    (define-key comint-mode-map "\C-d" 'comint-delchar-or-maybe-eof)))

(deh-package graphviz-dot
  :mode "\\.dot$"
  :config
  (setq graphviz-dot-auto-indent-on-semi nil
        graphviz-dot-auto-indent-on-newline nil
        graphviz-dot-toggle-completions t)

  (deh-add-hook graphviz-dot-mode-hook my-prog-mode-hook)

  (bind-keys
   :map graphviz-dot-mode-map
   ("C-c c" . nil)                     ; it's prefix key
   ("\t"  . graphviz-dot-tab-action))

  (defun graphviz-dot-tab-action ()
    "If cursor at one word end, try complete it. Otherwise, indent line."
    (interactive)
    (if (looking-at "\\>")
        (graphviz-dot-complete-word)
      (indent-for-tab-command))))

(deh-package protobuf
  :if (executable-find "protoc")
  :commands protobuf-mode)

;;; web related
(deh-package multi-web-mode
  :commands multi-web-mode
  :config
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                    (js-mode "<script[^>]*>" "</script>")
                    (css-mode "<style[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  (multi-web-global-mode 1))

;;# emacs -q --batch --eval '(byte-compile-file "js2.el")'
(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :config
  (deh-add-hook 'js2-mode-hook
    (setq forward-sexp-function nil
          js2-basic-offset 2)))

(deh-package php-mode
  :commands (php-mode)
  :config
  (deh-package php-doc
    :config
    (setq php-doc-directory "~/src/php_manual/html"
          php-doc-cachefile (expand-file-name "php-doc" my-data-dir))
    (bind-keys
     :map php-mode-map
     ("C-c \t" . php-doc-complete-function)
     ("C-c d"  . php-doc))
    (set (make-local-variable 'eldoc-documentation-function)
         'php-doc-eldoc-function)
    (eldoc-mode 1)
    ;; hack php-doc.el, in order to select php doc buffer automatically.
    (defun php-doc-w3m (url &rest ignore)
      (let ((buf (get-buffer-create "*php doc*")))
        (pop-to-buffer buf nil t)
        (w3m-goto-url url)))
    )

  (deh-add-hook php-mode-hook
    (my-prog-mode-hook)
    ;; (tempo-use-tag-list 'tempo-php-tags)
    (font-lock-add-keywords nil gtkdoc-font-lock-keywords)
    (setq php-beginning-of-defun-regexp "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(")
    (setq php-imenu-generic-expression
          '(
            ("Private Methods"
             "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?private\\s-+\\(?:static\\s-+\\)?function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
            ("Protected Methods"
             "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?protected\\s-+\\(?:static\\s-+\\)?function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
            ("Public Methods"
             "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?public\\s-+\\(?:static\\s-+\\)?function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
            ("Classes"
             "^\\s-*class\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*" 1)
            (nil
             "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
            ))
    )
;;;; ffap settings
  (defvar ffap-php-path
    (let ((include-path
           (shell-command-to-string "php -r 'echo get_include_path();'")))
      (split-string include-path ":"))
    "php include path")
  (defun my-php-ffap-locate (name)
    "Find php require or include files"
    (if (string-match "^[a-zA-Z0-9_]+$" name)
        (ffap-locate-file (replace-regexp-in-string "_" "/" name) '(".class.php" ".php") ffap-php-path)
      (ffap-locate-file name t ffap-php-path)))
  (if (featurep 'ffap)
      (add-to-list 'ffap-alist '(php-mode . my-php-ffap-locate))))

;;; edit
(deh-package markdown-mode
  :commands (markdown-mode)
  :config
  ;; override markdown's key binding
  (bind-keys*
   :map markdown-mode-map
   ("C-M-f"  . forward-sexp)
   ("C-M-b"  . backward-sexp)
   ("M-p"    . pager-row-up)
   ("M-n"    . pager-row-down)))

(deh-package auctex
  :disabled
  :config
  (load "preview-latex.el" nil t t)
  (set-default TeX-master nil)
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-electric-escape t
        TeX-save-query nil
        TeX-clean-confirm nil
        TeX-show-compilation nil)
  (deh-add-hook 'LaTeX-mode-hook
    (my-text-mode-hook)
    ;; (LaTeX-math-mode 1)

    (TeX-PDF-mode t)

    ;;# set evince as pdf viewer
    (add-to-list 'TeX-view-program-list '("Evince" "evince %o"))
    ;;# for XeLaTeX
    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
    (setq TeX-command-default "XeLaTeX"
          TeX-view-program-selection '((output-pdf "Evince"))
          TeX-global-PDF-mode t
          TeX-engine 'xetex)
    )

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
  )

(deh-package slime-helper
  :commands slime
  :init
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  :config
  ;;# download [hyperspec|ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz] to localhost, then use "C-c C-d h" to search symbols' hyperspec defines.
  (setq common-lisp-hyperspec-root (expand-file-name "~/src/HyperSpec/"))

  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-complete-symbol*-fancy t
        slime-fuzzy-completion-in-place t
        slime-enable-evaluate-in-emacs t
        slime-autodoc-use-multiline-p t)

  (deh-define-key slime-mode-map
                  ((kbd "TAB") 'slime-indent-and-complete-symbol)
                  ((kbd "C-c i") 'slime-inspect)
                  ((kbd "C-c s") 'slime-selector)))

(deh-package evernote-mode
  :load-path "~/local/emacs-evernote-mode"
  :bind
  ("C-c e c" . evernote-create-note)
  ("C-c e o" . evernote-open-note)
  ("C-c e s" . evernote-search-notes)
  ("C-c e S" . evernote-do-saved-search)
  ("C-c e w" . evernote-write-note)
  ("C-c e p" . evernote-post-region)
  ("C-c e b" . evernote-browser)
  :config
  ;; (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option
  (setq evernote-mode-hook
        '(lambda () (outline-minor-mode t)))
  )
