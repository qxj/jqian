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
  (setq outline-minor-mode-prefix (kbd "C-c C-o"))
  (deh-define-key outline-minor-mode-map
    ("\C-c\C-os" . 'show-subtree)
    ("\C-c\C-oS" . 'show-all)
    ("\C-c\C-oh" . 'hide-subtree)
    ("\C-c\C-oH" . 'hide-body)
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
    ("\C-c\C-on" . 'xwl-narrow-to-outline-level)
    ("\C-c\C-ou" . 'xwl-outline-toggle-enter-exit)
    ("\C-c\C-oq" . 'xwl-outline-toggle-show-hide))

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
(deh-section "gtags"
  (autoload 'gtags-mode "gtags" "" t)
  (eval-after-load "gtags"
    '(deh-define-key gtags-mode-map
       ;; Instead of `find-tag' & `pop-tag-mark'
       ((kbd "M-.")     . 'gtags-find-tag)
       ((kbd "M-*")     . 'gtags-pop-stack)
       ;; other key binds
       ((kbd "C-c g v") . 'gtags-visit-rootdir)
       ((kbd "C-c g t") . 'gtags-find-tag-from-here)
       ((kbd "C-c g o") . 'gtags-find-tag-other-window)
       ((kbd "C-c g r") . 'gtags-find-rtag)
       ((kbd "C-c g s") . 'gtags-find-symbol)
       ((kbd "C-c g p") . 'gtags-find-pattern)
       ((kbd "C-c g g") . 'gtags-find-with-grep)
       ((kbd "C-c g i") . 'gtags-find-with-idutils)
       ((kbd "C-c g f") . 'gtags-find-file)
       ((kbd "C-c g a") . 'gtags-parse-file)
       ((kbd "C-c g b") . 'gtags-append-tags)
       ((kbd "C-c g d") . 'gtags-display-tag)
       ((kbd "C-c g q") . 'gtags-display-tag-quit)
       ("q"             . 'gtags-display-tag-quit)
       ))
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
        (pop-to-buffer buffer)

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

(deh-section "xcscope"
  (eval-after-load "xcscope"
    '(progn
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
         (function cscope:hook)))))

(defcustom my-toggle-gtags-or-xcscope-option t
  "No-nil to enable gtags, or enable xcscope.")

(defun my-toggle-gtags-and-xcscope ()
  (interactive)
  (if my-toggle-gtags-or-xcscope-option
      (progn
        (deh-add-hooks (c-mode-hook c++-mode-hook) (gtags-mode 1))
        (deh-remove-hooks (c-mode-hook c++-mode-hook) (function cscope:hook)))
    (require 'xcscope)
    (deh-remove-hooks (c-mode-hook c++-mode-hook) (gtags-mode 1))
    (deh-add-hooks (c-mode-hook c++-mode-hook) (function cscope:hook))))

(my-toggle-gtags-and-xcscope)
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

(deh-section "flymake"
  (eval-after-load "flymake"
    '(progn
       (setq flymake-gui-warnings-enabled nil)
       (deh-add-hooks (c-mode-common-hook makefile-mode-hook)
            ((kbd "C-c C-v") . 'flymake-goto-next-error))

       ;; (flymake-mode t)
       ;; (setq flymake-log-level 1)

       (defun my-flymake-find-file-hook ()
         (condition-case nil
             (flymake-find-file-hook)
           (error nil)))
       ;; (add-hook 'find-file-hooks 'my-flymake-find-file-hook t)
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
(deh-section-if "nxhtml" "~/src/nxhtml/autostart.el" (load-file deh-this-path))

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
  )

(deh-section "auto-mode"
  (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.proc?$" . sql-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(ya?ml\\|fb\\)$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.acd$" . acd-mode))
  (add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode))
  (add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))
  (add-to-list 'auto-mode-alist '("\\.cls$" . LaTeX-mode))
  (add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(php[345]?\\|module\\|phtml\\|inc\\)$" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(frm\\|bas\\)$" . visual-basic-mode))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-to-list 'auto-mode-alist '("apache2?/access" . apache-log-generic-mode))
  (add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))
  (add-to-list 'auto-mode-alist '("\.schemas" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(p6\\|tdy\\|cgi\\|t\\)$" . perl-mode))
  (add-to-list 'auto-mode-alist '("\\.xs$" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))
  (add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))
  )

(deh-section "php"
  ;; (add-to-list 'magic-mode-alist '("\\`<\\?php" . php-mode))
  ;; (add-to-list 'interpreter-mode-alist '("php" . php-mode))
  (autoload 'geben "geben" "" t)
  (defun my-geben-open-file (file)
    (interactive
     (list
      (let ((source-file
             (replace-regexp-in-string "^file://" ""
                                       (geben-session-source-fileuri geben-current-session (buffer-file-name)))))
        (read-file-name "Open file: " (file-name-directory source-file)))))
    (geben-open-file (concat "file://" file)))
  (add-hook 'geben-context-mode-hook
            (lambda ()
              (define-key geben-mode-map "f" 'my-geben-open-file)))
;;  (deh-require 'php-doc)
  (deh-require 'simpletest)
  (setq simpletest-create-test-function 'simpletest-create-test-template)
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
  (defun my-php-mode-hook ()
    ;; (tempo-use-tag-list 'tempo-php-tags)
    (font-lock-add-keywords nil gtkdoc-font-lock-keywords)
    (setq php-beginning-of-defun-regexp "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(")
    (when (featurep 'php-doc)
      (local-set-key "\t" 'php-doc-complete-function)
      (set (make-local-variable 'eldoc-documentation-function)
           'php-doc-eldoc-function)
      (eldoc-mode 1))
    (when (featurep 'simpletest)
      (simpletest-mode 1)
      (define-key simpletest-mode-map "\C-ctb" 'simpletest-switch)
      (define-key simpletest-mode-map "\C-ctc" 'simpletest-create-test)
      (define-key simpletest-mode-map "\C-ctr" 'simpletest-run-test))
    (local-set-key (kbd "C-M-a") 'beginning-of-defun)
    (local-set-key (kbd "C-M-e") 'end-of-defun)
    )
  (add-hook 'php-mode-hook 'my-php-mode-hook)
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
      (add-to-list 'ffap-alist '(php-mode . my-php-ffap-locate)))
)


