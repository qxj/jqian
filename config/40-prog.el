;; -*- mode: Emacs-Lisp -*-

;; some programming setting

(deh-section makefile
  (deh-add-hook makefile-mode-hook
    ;; (my/code-common-hook)
    ))

(deh-package lisp-mode
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode ("Cask" . emacs-lisp-mode)
  :config
  (if (featurep 'ffap)
      (add-to-list 'ffap-alist '(lisp-interaction-mode . ffap-el-mode)))

  (deh-add-hook emacs-lisp-mode-hook
    (my/prog-mode-hook))

  (bind-keys
   :map emacs-lisp-mode-map ; lisp-mode-shared-map
   ("\M-."  . browse-el-find-funtion)
   ("\M-*"  . browse-el-go-back)
   ("C-)"   . my/auto-insert-paren))

  (defun my/auto-insert-paren ()
    "Auto close matched parentheses."
    (interactive)
    (condition-case nil
        (progn
          (scan-sexps (point) -1)
          (insert ")")
          (my/auto-insert-paren))
      (error (delete-char -1))))
  )


(deh-package sh-script
  :defer
  :config
  (deh-add-hook sh-mode-hook
    ;; (local-unset-key "\C-c\C-o")        ; trigger for `sh-while-getopts'
    (my/prog-mode-hook)
    (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p nil t)
    ))

(deh-package text-mode
  :defer
  :config
  (deh-add-hook text-mode-hook my/text-mode-hook))

;;; tools
(deh-package gnuplot
  :commands (gnuplot-mode)
  :mode "\\.gp$"
  :config
  (deh-add-hook gnuplot-after-plot-hook
    (select-window (get-buffer-window gnuplot-comint-recent-buffer)))
  (deh-add-hook gnuplot-comint-setup-hook
    (define-key comint-mode-map "\C-d" 'comint-delchar-or-maybe-eof)))

;; workaround
(unless (boundp 'org-src-lang-modes)
  (setq org-src-lang-modes nil))

(deh-package graphviz-dot-mode
  :mode "\\.dot$"
  :config
  (setq graphviz-dot-auto-indent-on-semi nil
        graphviz-dot-auto-indent-on-newline nil
        graphviz-dot-toggle-completions t)

  (deh-add-hook graphviz-dot-mode-hook my/prog-mode-hook)

  (bind-keys
   :map graphviz-dot-mode-map
   ("C-c c" . nil)                     ; it's prefix key
   ("\t"  . graphviz-dot-tab-action))

  (defun graphviz-dot-tab-action ()
    "If cursor at one word end, try complete it. Otherwise, indent line."
    (interactive)
    (if (looking-at "\\>")
        (graphviz-dot-complete-word)
      (indent-for-tab-command)))
  )

(deh-package protobuf
  :if (executable-find "protoc")
  :commands protobuf-mode)

;;; web related
(deh-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  )

;;# emacs -q --batch --eval '(byte-compile-file "js2.el")'
(deh-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :config
  (deh-add-hook 'js2-mode-hook
    (setq forward-sexp-function nil
          js2-basic-offset 2)))

(deh-package php-mode
  :mode ("\\.php$" . php-mode)
  :commands (php-mode)
  :config
  (deh-package php-doc
    :config
    (setq php-doc-directory "~/src/php_manual/html"
          php-doc-cachefile (expand-file-name "php-doc" my/data-dir))
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
    (my/prog-mode-hook)
    ;; (tempo-use-tag-list 'tempo-php-tags)
    ;; (font-lock-add-keywords nil gtkdoc-font-lock-keywords)
    ;; (setq php-beginning-of-defun-regexp "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(")
    ;; (setq php-imenu-generic-expression
    ;;       '(
    ;;         ("Private Methods"
    ;;          "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?private\\s-+\\(?:static\\s-+\\)?function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
    ;;         ("Protected Methods"
    ;;          "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?protected\\s-+\\(?:static\\s-+\\)?function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
    ;;         ("Public Methods"
    ;;          "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?public\\s-+\\(?:static\\s-+\\)?function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
    ;;         ("Classes"
    ;;          "^\\s-*class\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*" 1)
    ;;         (nil
    ;;          "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)
    ;;         ))
    )
  ;; ffap settings
  (defvar ffap-php-path
    (let ((include-path
           (shell-command-to-string "php -r 'echo get_include_path();'")))
      (split-string include-path ":"))
    "php include path")
  (defun my/php-ffap-locate (name)
    "Find php require or include files"
    (if (string-match "^[a-zA-Z0-9_]+$" name)
        (ffap-locate-file (replace-regexp-in-string "_" "/" name) '(".class.php" ".php") ffap-php-path)
      (ffap-locate-file name t ffap-php-path)))
  (if (featurep 'ffap)
      (add-to-list 'ffap-alist '(php-mode . my/php-ffap-locate))))


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

(deh-package sql
  :config
  (setq sql-product 'mysql)
  (eval-after-load "sql"
    '(load-library "sql-indent")))
