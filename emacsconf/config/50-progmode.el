;; -*- mode: Emacs-Lisp -*-
;;{{{ etags, hideshow, tree-imenu, smart-compile
;; etags
(deh-section "etags"
  (setq tags-add-tables nil
        default-tags-table-function
        (lambda nil
          (ywb-find-top-directory "TAGS"))))

(deh-require 'help-dwim
  (help-dwim-register
   '(clibpc . ["a-zA-Z0-9_" clibpc-obarray nil
               (lambda (sym)
                 (clibpc-describe-function (symbol-name sym)))])
   t
   '((setq clibpc-cache-file (expand-file-name "clibpc-symbols.el" my-temp-dir))
     (require 'clibpc nil t)))
  )

(deh-require 'imenu-tree
  (add-hook 'tree-mode-hook
            (lambda ()
              (toggle-truncate-lines t))))

;; Rebinding keys for hideshow
(deh-require 'hideshow
  (define-key hs-minor-mode-map "\C-ch"
    (let ((map (lookup-key hs-minor-mode-map "\C-c@")))
      ;; C-h is help to remind me key binding
      (define-key map "\C-h" 'describe-prefix-bindings)
      (define-key map "\C-q" 'hs-toggle-hiding)
      ;; compatible with outline
      (define-key map "h" 'hs-hide-block)
      (define-key map "s" 'hs-show-block)
      (define-key map "H" 'hs-hide-all)
      (define-key map "S" 'hs-show-all)
      map)))
;;}}}

;;{{{ Etags, Gtags
(deh-section "gtags"
  (autoload 'gtags-mode "gtags" "" t)
  (dolist (map (list c-mode-hook c++-mode-hook java-mode-hook))
    (add-hook
     'map 
     '(lambda () 
        (gtags-mode 1)
        ;; Instead of `find-tag' & `pop-tag-mark'
        (define-key gtags-mode-map (kbd "M-.") 'gtags-find-tag-from-here)
        (define-key gtags-mode-map (kbd "M-*") 'gtags-pop-stack)
        ;; Key bind for gtags-mode
        (define-prefix-command 'my-gtags-map)
        (global-set-key (kbd "C-c g") 'my-gtags-map)
        (define-key gtags-mode-map (kbd "C-c g v") 'gtags-visit-rootdir)
        (define-key gtags-mode-map (kbd "C-c g t") 'gtags-find-tag)
        (define-key gtags-mode-map (kbd "C-c g o") 'gtags-find-tag-other-window)
        (define-key gtags-mode-map (kbd "C-c g r") 'gtags-find-rtag)
        (define-key gtags-mode-map (kbd "C-c g s") 'gtags-find-symbol)
        (define-key gtags-mode-map (kbd "C-c g p") 'gtags-find-pattern)
        (define-key gtags-mode-map (kbd "C-c g g") 'gtags-find-with-grep)
        (define-key gtags-mode-map (kbd "C-c g i") 'gtags-find-with-idutils)
        (define-key gtags-mode-map (kbd "C-c g f") 'gtags-find-file)
        (define-key gtags-mode-map (kbd "C-c g a") 'gtags-parse-file)
        (define-key gtags-mode-map (kbd "C-c g b") 'yp-gtags-append)
        )))

  (defun yp-gtags-append ()
    (interactive)
    (if gtags-mode
        (progn
          (message "start to global -u")
          (start-process "gtags-name" "*gtags-var*" "global" "-u"))))
  )
;;}}}
;; Setting for common hook
(defun my-mode-common-hook ()
  (setq tab-width 4)
  (set (make-local-variable 'tab-stop-list) (number-sequence tab-width 80 tab-width))
  (abbrev-mode t)
  (set (make-local-variable 'comment-style) 'indent)
  ;; (local-set-key "\t" 'hippie-expand)
  (setq c-basic-offset tab-width))

;;{{{ elisp
(deh-section "elisp"
  (deh-require 'browse-el
    (define-key lisp-mode-shared-map (kbd "M-.") 'browse-el-find-funtion)
    (define-key lisp-mode-shared-map (kbd "M-*") 'browse-el-go-back)
    ;; (define-key lisp-mode-shared-map (kbd "<f6>") 'find-tag)
    ;; (define-key lisp-mode-shared-map (kbd "<f5>") 'pop-tag-mark)
    )

  (add-to-list 'ffap-alist '(lisp-interaction-mode . ffap-el-mode))
  (defun my-emacs-lisp-mode-hook ()
    (my-mode-common-hook)
    (define-key lisp-mode-shared-map (kbd "C-)") 'ywb-insert-paren)
    (local-set-key "\t" 'PC-lisp-complete-symbol)
    ;; (tempo-install "(?\\([^\\b]+\\)\\=" 'tempo-elisp-tags)
    (tempo-use-tag-list 'tempo-elisp-tags)
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
         (tempo-use-tag-list 'tempo-html-tags)
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
(deh-section "nxhtml"
  ;;; nxhtml mode
  (let ((nxhtml-init-file "~/src/nxhtml/autostart.el"))
    (if (file-exists-p nxhtml-init-file)
        (load-file nxhtml-init-file)))
  )

;; sh-mode
(deh-section "sh-mode"
  (add-hook 'sh-mode-hook
            (lambda ()
              ;; (when buffer-file-name
              ;;   (executable-set-magic "bash" nil t t))
              (deh-require 'inf-sh-mode)))
  (add-hook 'sh-set-inferior-hook
            (lambda ()
              (keep-end-watch-this
               (buffer-name sh-inferior-buffer)))))

;; gnuplot
(deh-section "gnuplot"
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
  (add-hook 'gnuplot-after-plot-hook
            (lambda ()
              (select-window (get-buffer-window gnuplot-comint-recent-buffer))))
  (add-hook 'gnuplot-comint-setup-hook
            (lambda ()
              (define-key comint-mode-map "\C-d" 'comint-delchar-or-maybe-eof))))

(deh-section "java"
  (add-to-list 'load-path "/usr/local/jdee/lisp/")
  (defun jde-init ()
    (interactive)
    (require 'cedet)
    (require 'jde)
    (jde-mode)
    )
  (defun my-java-mode-hook ()
    (c-set-style "java");
    (setq c-basic-offset 4))
  (add-hook 'java-mode-hook 'my-java-mode-hook))

(deh-section "js2"
  (defun my-js2-mode-hook ()
    (setq forward-sexp-function nil)
    )
  (add-hook 'js2-mode-hook 'my-js2-mode-hook))

(deh-section "autoloads"
  ;; (autoload 'gtags-mode "gtags" "Global Tags Mode from GNU." t)
  (autoload 'svn-status "psvn" nil t)
  (autoload 'js2-mode "js2-mode" "" t)
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
  (autoload 'pod-mode "pod-mode" "A major mode to edit pod" t)
  (autoload 'sourcepair-load "sourcepair" nil t))

(deh-section "auto-mode"
  (add-to-list 'auto-mode-alist '("\\.proc?$" . sql-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(ya?ml\\|fb\\)$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.acd$" . acd-mode))
  (add-to-list 'auto-mode-alist '("\\.po\\'\\|\\.po\\." . po-mode))
  (add-to-list 'auto-mode-alist '("\\.i\\'" . swig-mode))
  (add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))
  (add-to-list 'auto-mode-alist '("\\.cls$" . LaTeX-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.tt2?$" . html-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.\\(php[345]?\\|module\\|phtml\\|inc\\)$" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(hla\\|hhf\\)$" . hla-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(frm\\|bas\\)$" . visual-basic-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(imc\\|pir\\)\\'" . pir-mode))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-to-list 'auto-mode-alist '("apache2?/access" . apache-log-generic-mode))
  (add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))
  (add-to-list 'auto-mode-alist '("\\.fa\\|\\.gb\\|\\.embl$" . dna-mode))
  (add-to-list 'auto-mode-alist '("\.schemas" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(p6\\|tdy\\|cgi\\|t\\)$" . perl-mode))
  (add-to-list 'auto-mode-alist '("\\.xs$" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))
  (add-to-list 'auto-mode-alist '("\\.pir$" . pir-mode))
  (add-to-list 'auto-mode-alist '("\\.xs$" . xs-mode))
  (add-to-list 'auto-mode-alist '("\\.muse$" . muse-mode))
  (add-to-list 'auto-mode-alist '("\\.twiki$" . oddmuse-mode)))

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
    (tempo-use-tag-list 'tempo-php-tags)
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
  (add-to-list 'ffap-alist '(php-mode . my-php-ffap-locate)))


