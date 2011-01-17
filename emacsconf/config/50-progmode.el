;; -*- mode: Emacs-Lisp -*-

;;{{{ Rebinding keys for hideshow
(deh-require 'hideshow
  (deh-define-key hs-minor-mode-map
    ("\C-chh" . 'hs-hide-block)
    ("\C-chs" . 'hs-show-block)
    ("\C-chH" . 'hs-hide-all)
    ("\C-chS" . 'hs-show-all)
    ("\C-cht" . 'hs-toggle-hiding)))
;;}}}

;;{{{ Outline minor mode
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
;;}}}

;;{{{ Etags
(deh-section "etags"
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
;;}}}

;;{{{ Gtags & Xcscope
(deh-section-if "gtags" (executable-find "global")
  (autoload 'gtags-mode "gtags" "" t)

  (deh-add-hooks (c-mode-common-hook)
    (gtags-mode t))

  (setq gtags-mode-hook
    '(lambda ()
       (setq gtags-pop-delete t)
       (setq gtags-path-style 'absolute)
  ))

  (setq gtags-select-mode-hook
    '(lambda ()
       (setq hl-line-face 'underline)
       (hl-line-mode 1)
  ))

  (eval-after-load "gtags"
    '(deh-define-key gtags-mode-map
       ;; Instead of `find-tag' & `pop-tag-mark'
       ((kbd "M-.") . 'gtags-find-tag)
       ((kbd "M-*") . 'gtags-pop-stack)
       ;; other key binds
       ("\C-cgv" . 'gtags-visit-rootdir)
       ("\C-cgt" . 'gtags-find-tag-from-here)
       ("\C-cgo" . 'gtags-find-tag-other-window)
       ("\C-cgr" . 'gtags-find-rtag)
       ("\C-cgs" . 'gtags-find-symbol)
       ("\C-cgp" . 'gtags-find-pattern)
       ("\C-cgg" . 'gtags-find-with-grep)
       ("\C-cgi" . 'gtags-find-with-idutils)
       ("\C-cgf" . 'gtags-find-file)
       ("\C-cga" . 'gtags-parse-file)
       ("\C-cgb" . 'gtags-append-tags)
       ("\C-cgd" . 'gtags-display-tag)
       ("\C-cgq" . 'gtags-display-tag-quit)
       ("q"      . 'gtags-display-tag-quit)))
  (defun gtags-append-tags ()
    (interactive)
    (if gtags-mode
        (progn
          (message "start to global -u")
          (start-process "gtags-name" "*gtags-var*" "global" "-u"))))

  ;; Only display tags in another window, hacked by julian
  (defvar gtags-previous-window-conf nil
    "Window configuration before switching to sdcv buffer.")
  (defun gtags-display-tag ()
    "Input tag name and move to the definition."
    (interactive)
    (let (tagname prompt input)
      (setq tagname (gtags-current-token))
      (if tagname
          (setq prompt (concat "Find tag: (default " tagname ") "))
        (setq prompt "Find tag: "))
      (setq input (completing-read prompt 'gtags-completing-gtags
                                   nil nil nil gtags-history-list))
      (if (not (equal "" input)) (setq tagname input))
      (if (and (boundp 'gtags-select-buffer-single) ; >= v5.9.0
               gtags-select-buffer-single)
          (progn
            (let ((now-buffer-list (buffer-list)) now-buffer)
              (while now-buffer-list
                (setq now-buffer (car now-buffer-list))
                (if (string-match "*GTAGS SELECT*" (buffer-name now-buffer))
                    (kill-buffer now-buffer))
                (setq now-buffer-list (cdr now-buffer-list))))))

      ;; save windows configuration
      (setq gtags-previous-window-conf (current-window-configuration))

      (let* ((option "-x")
             (save (current-buffer))
             (prefix "(D)")
             (buffer (generate-new-buffer (generate-new-buffer-name (concat "*GTAGS SELECT* " prefix tagname))))
             context
             lines)
        ;; (set-buffer buffer)
        (pop-to-buffer buffer nil t)    ; keep pop buffer in the same window

        (cond
         ((equal gtags-path-style 'absolute)
          (setq option (concat option "a")))
         ((equal gtags-path-style 'root)
          (let (rootdir)
            (if gtags-rootdir
                (setq rootdir gtags-rootdir)
              (setq rootdir (gtags-get-rootpath)))
            (if rootdir (cd rootdir)))))
        (message "Searching %s ..." tagname)
        (if (not (= 0 (call-process "global" nil t nil option tagname)))
        ;; (if (not (= 0 (call-process "global" nil t nil option "--encode-path=\" \t\"" tagname)))
            (message (buffer-substring (point-min)(1- (point-max))))
          ;; else goto line
          (goto-char (point-min))
          (setq lines (count-lines (point-min) (point-max)))
          (cond
           ((= 0 lines)
            (message "%s: tag not found" tagname)
            (kill-buffer buffer)
            ;; restore window config?
            )
           ((= 1 lines)
            (message "Searching %s ... Done" tagname)
            (gtags-select-it t nil)
            (recenter))
           (t
            (switch-to-buffer buffer)
            (gtags-select-mode)))))))
  (defun gtags-display-tag-quit ()
    "Quit gtags display buffer."
    (interactive)
    (if (window-configuration-p gtags-previous-window-conf)
        (progn
          (bury-buffer)
          (set-window-configuration gtags-previous-window-conf)
          (setq gtags-previous-window-conf nil))
      (self-insert-command 1)))
  )

(deh-require-reserved 'xcscope
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
             ((kbd "M-.") . 'cscope-find-this-symbol)
             ((kbd "M-*") 'cscope-pop-mark))
           ;; Key bind for cscope-minor-mode
           ))
  ;; hack `xcscope.el', remove hooks
  (deh-remove-hooks (c-mode-hook c++-mode-hook dired-mode-hook)
    (function cscope:hook)))
;;}}}

;;{{{ tag view history
(deh-require 'tags-view
  (deh-define-key tags-history-mode-map
    ("q" . 'tv-view-history-quit))

  (defvar tv-previous-window-conf nil
    "Window configuration before switching to sdcv buffer.")
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
;;}}}

;;{{{ svn settins
(deh-require 'psvn
  (defsubst svn-status-interprete-state-mode-color (stat)
    "Interpret vc-svn-state symbol to mode line color"
    (case stat
      ('up-to-date "GreenYellow")
      ('edited     "tomato")
      ('unknown    "gray")
      ('added      "blue")
      ('deleted    "red")
      ('unmerged   "purple")
      (t           "black")))
  )
;;}}}

;;{{{ flymake & flyspell
(deh-section-reserved "flyspell"
  ;; flyspell-goto-next-error: `C-,'
  ;; (ispell-change-dictionary)
  (deh-add-hooks (text-mode-hook org-mode-hook) (flyspell-mode 1))
  (deh-add-hooks (change-log-mode-hook log-edit-mode-hook) (flyspell-mode -1))
  (deh-add-hooks (c-mode-common-hook python-mode-hook) (flyspell-prog-mode)))

(deh-section-reserved "flymake"
  (eval-after-load "flymake"
    '(progn
       (autoload 'flymake-find-file-hook "flymake" "" t)
       (defun my-flymake-find-file-hook ()
         (condition-case nil
             (flymake-find-file-hook)
           (error nil)))
       (add-hook 'find-file-hooks 'my-flymake-find-file-hook t)

       ;; (flymake-mode t)

       (setq flymake-gui-warnings-enabled nil
             flymake-log-level 0)

       ;; (deh-add-hooks (c-mode-common-hook makefile-mode-hook)
       ;;      ((kbd "C-c C-v") . 'flymake-goto-next-error))

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
       (defvar flymake-mode-map (make-sparse-keymap))
       (deh-define-key flymake-mode-map
         ((kbd "C-c <f4>")   . 'flymake-goto-next-error-disp)
         ((kbd "C-c <S-f4>") . 'flymake-goto-prev-error-disp)
         ((kbd "C-c <C-f4>") . 'flymake-display-err-menu-for-current-line))
       (or (assoc 'flymake-mode minor-mode-map-alist)
           (setq minor-mode-map-alist
                 (cons (cons 'flymake-mode flymake-mode-map)
                       minor-mode-map-alist)))

       ;; Directly use gcc instead of Makefile
       (setq flymake-allowed-file-name-masks '())
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
       (when (or (executable-find "make")
                 (executable-find "gcc")
                 (executable-find "g++"))
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
                        flymake-simple-make-gcc-init)))
       )))
;;}}}

;; Setting for common hook
(defun my-mode-common-hook ()
  (setq tab-width 4)
  (set (make-local-variable 'tab-stop-list) (number-sequence tab-width 80 tab-width))
  ;; (abbrev-mode t)
  (set (make-local-variable 'comment-style) 'indent)
  (setq c-basic-offset tab-width)
  ;; comment new line and indent `M-j', as VIM acts.
  (defun my-cursor-on-comment-p (&optional point)
    (memq (get-text-property (or point (point)) 'face)
          '(font-lock-comment-face)))
  ;; (local-set-key (kbd "RET")
  ;;                (lambda () (interactive)
  ;;                  (if (my-cursor-on-comment-p) (comment-indent-new-line)
  ;;                    (if (boundp 'autopair-newline) (autopair-newline)
  ;;                      (newline-and-indent)))))
  )

;;{{{ elisp
(deh-section "elisp"
  (deh-require 'browse-el
    (define-key lisp-mode-shared-map (kbd "M-.") 'browse-el-find-funtion)
    (define-key lisp-mode-shared-map (kbd "M-*") 'browse-el-go-back)
    )
  (if (featurep 'ffap)
      (add-to-list 'ffap-alist '(lisp-interaction-mode . ffap-el-mode)))
  (defun my-emacs-lisp-mode-hook ()
    (my-mode-common-hook)
    (define-key lisp-mode-shared-map (kbd "C-)") 'ywb-insert-paren)
    (hs-minor-mode 1)
    (turn-on-eldoc-mode))
  (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook))
;;}}}

;;=============================================================
;; Not use offen language
;;=============================================================

;; HTML
(deh-section "html"
  (setq sgml-xml-mode t)
  (add-hook 'sgml-mode-hook 'my-mode-common-hook)
  (set 'html-mode-hook
       (lambda ()
         (define-key html-mode-map (kbd "<C-return>") 'ywb-html-insert-newline)
         ;; (tempo-use-tag-list 'tempo-html-tags)
         (let ((str '(""))
               (align '(("align" ("left") ("center") ("right")))))
           (setq sgml-tag-alist `(("style"
                                   ("href" ,str)
                                   ("type" "text/css"))
                                  ("meta"
                                   t
                                   ("http-equiv" ("Content-Type"))
                                   ("content" ("text/html; charset=utf-8" "text/plain") ("Copyright &#169;"))
                                   ("name" ,str))
                                  ("script" (nil "//<![CDATA[" \n _ \n "//]]>")
                                   ("src" ,str)
                                   ("type" "text/javascript"))
                                  ("div" n
                                   ("class" ,str)
                                   ("src", str))
                                  ("object" ("id" ,str))
                                  ("code")
                                  ,@sgml-tag-alist))))))

;; nxhtml: javascript + php + html + css
(deh-section-path "nxhtml"
  "~/src/nxhtml/autostart.el"
  (load-file deh-this-path)
  (deh-add-hook nxhtml-mode-hook
    (local-unset-key (kbd "C-c C-o"))))

;; gnuplot
(deh-section "gnuplot"
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
  (deh-add-hook gnuplot-after-plot-hook
    (select-window (get-buffer-window gnuplot-comint-recent-buffer)))
  (deh-add-hook gnuplot-comint-setup-hook
    (deh-define-key comint-mode-map
      ("\C-d" . 'comint-delchar-or-maybe-eof))))

;; graphviz
(deh-section "graphviz"
  (autoload 'graphviz-dot-mode "graphviz-dot-mode" "graphviz mode" t)
  (deh-add-hook graphviz-dot-mode-hook
    (local-unset-key "\C-cc") ; it's prefix key
    (define-key graphviz-dot-mode-map "\t" 'graphviz-dot-tab-action))
  (defun graphviz-dot-tab-action ()
    "If cursor at one word end, try complete it. Otherwise, indent line."
    (interactive)
    (if (looking-at "\\>")
        (graphviz-dot-complete-word)
      (indent-for-tab-command))))

;; java
(deh-section "java"
  (add-to-list 'load-path "/usr/local/jdee/lisp/")
  (defun jde-init ()
    (interactive)
    (require 'cedet)
    (require 'jde)
    (jde-mode))
  (deh-add-hook java-mode-hook
    (c-set-style "java")
    (setq c-basic-offset 4)))

;;; emacs --batch --eval '(byte-compile-file "js2.el")'
(deh-section "js2"
  (deh-add-hook js2-mode-hook
    (setq forward-sexp-function nil)))

(deh-section "autoloads"
  ;; (autoload 'gtags-mode "gtags" "Global Tags Mode from GNU." t)
  (autoload 'svn-status "psvn" nil t)
  (autoload 'js2-mode "js2" "" t)
  (autoload 'git-status "git" "" t)
  (autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
  (autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
  (autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
  (autoload 'yaml-mode "yaml-mode" "YAML major mode" t)
  (autoload 'bat-mode "bat-mode" "Bat mode for Windows batch file" t)
  (autoload 'javascript-mode "javascript-mode" "JavaScript mode" t)
  (autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
  (autoload 'python-mode "python" "Python editing mode." t)
  (autoload 'php-mode "php-mode" "php mode" t)
  (autoload 'visual-basic-mode "vb-mode" "Visual Basic Mode" t)
  ;; (autoload 'pod-mode "pod-mode" "A major mode to edit pod" t)
  (autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)
  (autoload 'whitespace-toggle-options "whitespace" "Toggle local `whitespace-mode' options." t)
  (autoload 'sourcepair-load "sourcepair" nil t)
  (autoload 'compile-dwim-compile "compile-dwim" nil t)
  (autoload 'compile-dwim-run "compile-dwim" nil t)
  ;; emacs lock
  (autoload 'toggle-emacs-lock "emacs-lock" "Emacs lock" t)
  ;; iimage
  (autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
  (autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
  )

(deh-section "auto-mode"
  (add-to-list 'auto-mode-alist '("\\.proc?$" . sql-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(ya?ml\\|fb\\)$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))
  (add-to-list 'auto-mode-alist '("\\.cls$" . LaTeX-mode))
  (add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(php[345]?\\|module\\|phtml\\|inc\\)$" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(frm\\|bas\\)$" . visual-basic-mode))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-to-list 'auto-mode-alist '("apache2?/access" . apache-log-generic-mode))
  (add-to-list 'auto-mode-alist '("\\(Makefile\\|Build\\)" . makefile-mode))
  (add-to-list 'auto-mode-alist '("\.schemas" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(p6\\|tdy\\|cgi\\|t\\)$" . perl-mode))
  (add-to-list 'auto-mode-alist '("\\.xs$" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))
  )

(deh-section-reserved "php"
  (add-to-list 'magic-mode-alist '("\\`<\\?php" . php-mode))
  (add-to-list 'interpreter-mode-alist '("php" . php-mode))
  (deh-try-require 'php-doc
    (setq php-doc-directory "~/src/php_manual/html"
          php-doc-cachefile (expand-file-name "php-doc" my-temp-dir))
    (deh-local-set-key php-mode-hook
      ("\t"       . 'php-doc-complete-function)
      ("\C-cd" . 'php-doc))
    (set (make-local-variable 'eldoc-documentation-function)
         'php-doc-eldoc-function)
    (eldoc-mode 1)
    ;; hack php-doc.el, in order to select php doc buffer automatically.
    (defun php-doc-w3m (url &rest ignore)
      (let ((buf (get-buffer-create "*php doc*")))
        (pop-to-buffer buf nil t)
        (w3m-goto-url url)))
    )
  (deh-try-require 'geben
    (defun my-geben-open-file (file)
      (interactive
       (list
        (let ((source-file
               (replace-regexp-in-string
                "^file://" ""
                (geben-session-source-fileuri geben-current-session
                                              (buffer-file-name)))))
          (read-file-name "Open file: " (file-name-directory source-file)))))
      (geben-open-file (concat "file://" file)))
    (deh-define-key geben-mode-map
      ("f" . 'my-geben-open-file)))
  (deh-try-require 'simpletest
    (simpletest-mode 1)
    (setq simpletest-create-test-function 'simpletest-create-test-template)
    (deh-define-key simpletest-mode-map
      ("\C-ctb" . 'simpletest-switch)
      ("\C-ctc" . 'simpletest-create-test)
      ("\C-ctr" . 'simpletest-run-test)))
  (deh-add-hook php-mode-hook
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
    (local-set-key (kbd "C-M-a") 'beginning-of-defun)
    (local-set-key (kbd "C-M-e") 'end-of-defun)
    )
  ;; ffap settings
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


