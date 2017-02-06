;; -*- emacs-lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end

(use-package desktop
  :config
  (setq desktop-base-file-name (concat "emacs.desktop-" (system-name))
        desktop-restore-eager 8)        ; firstly restore 8 buffers

  ;;# not to save
  (setq desktop-globals-to-save
        (delq nil (mapcar (lambda (x) (if (memq x '(tags-table-list
                                                    file-name-history)) nil x))
                          desktop-globals-to-save)))
  ;; (setq desktop-buffers-not-to-save "\\(\\.log\\|\\.diary\\|\\.elc\\)$")
  (dolist (mode '(dired-mode info-lookup-mode fundamental-mode))
    (add-to-list 'desktop-modes-not-to-save mode))

  ;;# to save
  ;; (add-to-list 'desktop-globals-to-save 'kill-ring)
  (if (boundp 'windata-name-winconf)
      (add-to-list 'desktop-globals-to-save 'windata-named-winconf))
  (if (boundp 'smex-history)
      (add-to-list 'desktop-globals-to-save 'smex-history))

  ;; if error occurred, no matter it!
  ;; (condition-case nil
  ;;     (desktop-read)
  ;;   (error nil))
  (unless (emacs-process-duplicated-p)
    (desktop-save-mode 1)
    ;;# persist desktop into file every 10 mins
    (run-with-idle-timer 600 600 'desktop-save-in-desktop-dir))

  ;; desktop-menu.el can store many desktops, it works besides
  ;; desktop.el and its settings don't cofflict with desktop.el, so
  ;; please don't mix up `desktop-base-file-name' and
  ;; `desktop-menu-base-filename'.
  ;;
  (use-package desktop-menu
    :config
    (setq desktop-menu-base-filename (concat "emacs.desktops-" (system-name))
          desktop-menu-list-file "emacs.desktops"
          desktop-menu-clear 'ask)
    ;; customize some standard `desktop' variables
    (setq desktop-load-locked-desktop t)

    ;; desktop-menu-autosave can NOT be customized by setq
    (customize-set-variable 'desktop-menu-autosave 500)

    ;;# save individual bm repository with different desktops
    (with-eval-after-load 'bm
      (defun bm-repository-file-of-desktop-menu ()
        (let ((current-desktop (cdr desktop-menu--current-desktop)))
          (if current-desktop
              (setq bm-repository-file
                    (concat current-desktop ".bm-repo")))))
      (my/add-hook 'desktop-after-read-hook
        (if (bm-repository-file-of-desktop-menu) (bm-repository-load)))
      (my/add-hook 'desktop-save-hook
        (when (bm-repository-file-of-desktop-menu)
          (bm-buffer-save-all)
          (bm-repository-save))))
    )
  )

(use-package bm
  :commands (bm-toggle bm-next bm-previous bm-show bm-show-all
                       bm-toggle-cycle-all-buffers)
  :bind*
  ("C-c b" . bm-prefix-map)
  ;; mouse setting
  ("<left-margin> <mouse-2>" . bm-toggle-mouse)
  ("<left-margin> <mouse-3>" . bm-next-mouse)
  :init
  (bind-keys
   :prefix-map bm-prefix-map
   :prefix "C-c b"
   ("b" . bm-toggle)
   ("n" . bm-next)
   ("p" . bm-previous)
   ("s" . bm-show)
   ("l" . bm-show)
   ("a" . bm-show-all)
   ("t" . bm-toggle-cycle-all-buffers)
   ("h" . helm-bm))
  :config
  (setq bm-cycle-all-buffers nil
        bm-highlight-style (if window-system
                               'bm-highlight-line-and-fringe
                             'bm-highlight-line)
        bm-restore-repository-on-load t)
  (setq-default bm-buffer-persistence t)

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
  (bind-keys
   :map bm-show-mode-map
   ("n"  . bm-show-next)
   ("p"  . bm-show-prev))

  (use-package helm-bm
    :after bm)
  )

(use-package ivy
  :disabled
  :ensure t
  :diminish ivy-mode
  :bind
  (("C-x b" . ivy-switch-buffer)
   ("C-c C-r" . ivy-resume)
   :map ivy-minibuffer-map
   ("C-w" . ivy-backward-kill-word)
   ("C-l" . ivy-backward-kill-word)
   ("<return>" . ivy-alt-done)
   ("C-c o" . ivy-occur))
  :config
  (ivy-mode 1)

  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))

  (ivy-set-actions
   t '(("I" insert "insert")))

  (use-package swiper
    :ensure
    :bind
    ("C-s" . swiper)
    ("C-r" . swiper)
    ("C-M-s" . swiper-all))

  (use-package counsel
    :ensure
    :bind
    ("M-x" . counsel-M-x)
    ("M-y" . counsel-yank-pop)

    ("C-x d" . counsel-dired-jump)
    ("C-x C-f" . counsel-find-file)

    ("C-c i" . counsel-imenu)
    ("C-c c o" . counsel-recentf)

    ("C-h f" . counsel-describe-function)
    ("C-h v" . counsel-describe-variable)
    ("C-h l" . counsel-find-library)
    ("C-h i" . counsel-info-lookup-symbol)
    ("C-h u" . counsel-unicode-char)

    ("C-x c g"  . counsel-git)
    ("C-x c j"  . counsel-git-grep)
    ("C-x c a"  . counsel-ag)
    ("C-x c l"  . counsel-locate)

    :bind
    (:map help-map
          ("f" . counsel-describe-function)
          ("v" . counsel-describe-variable)
          ("l" . counsel-info-lookup-symbol))
    :bind
    (:map read-expression-map
          ("C-r" . counsel-expression-history))

    :config
    (when (eq 'system-type 'darwin)
      (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind))

    (setq counsel-find-file-at-point t
          ivy-use-virtual-buffers t
          ivy-display-style 'fancy
          ivy-initial-inputs-alist nil)
    )

  (use-package counsel-gtags
    :ensure t
    :defer 3
    :if (executable-find "gtags")
    :diminish (counsel-gtags-mode . "cG")
    :bind (:map counsel-gtags-mode-map
                ("M-." . counsel-gtags-find-definition)
                ("M-," . counsel-gtags-pop-stack)
                ("M-s d" . counsel-gtags-dwim)
                ("M-s r" . counsel-gtags-find-reference)
                ("M-s s" . counsel-gtags-find-symbol))
    :config
    (add-hook 'c-mode-hook 'counsel-gtags-mode)
    (add-hook 'c++-mode-hook 'counsel-gtags-mode)
    )

  (use-package counsel-projectile
    :ensure t
    :defer 3
    :config
    (counsel-projectile-on))

  (use-package counsel-dash
    :commands counsel-dash
    :bind
    ("C-x c d" . counsel-dash))
  )

(use-package mark-more-like-this
  :bind
  ("C-<" . mark-previous-like-this)
  ("C->" . mark-next-like-this)
  ("C-M-m" . mark-more-like-this)
  ("C-*" . mark-all-like-this))

(use-package hl-line
  :config
  ;; (global-hl-line-mode 1)
  (setq hl-line-face 'underline)
  ;; (set-face-background 'hl-line "white smoke") ; list-colors-display
  )

(use-package hi-lock
  :diminish hi-lock-mode
  :config
  (setq hi-lock-file-patterns-range 5000
        hi-lock-file-patterns-policy '(lambda (dummy) t)))

(use-package rainbow-mode
  :commands rainbow-mode
  ;; (add-hook 'prog-mode-hook 'rainbow-mode)
  )

;; diff-hl-mode highlights uncommitted changes on the left side of the
;; window, allows you to jump between and revert them selectively.
(use-package diff-hl
  :config
  (global-diff-hl-mode 1)
  (add-hook 'magit-refresh-file-buffer-hook
            (lambda ()
              (with-current-buffer (current-buffer) (diff-hl-update)))))

(use-package sdcv
  :bind ("M-1" . sdcv-search-input))

(use-package markdown-mode
  :commands (markdown-mode)
  :config
  ;; override markdown's key binding
  (bind-keys*
   :map markdown-mode-map
   ("C-M-f"  . forward-sexp)
   ("C-M-b"  . backward-sexp))
  (unbind-key "M-n" markdown-mode-map)
  (unbind-key "M-p" markdown-mode-map)

  (add-hook 'markdown-mode-hook 'outline-minor-mode)

  (setq markdown-xhtml-header-content
        "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\">
MathJax.Hub.Config({
        tex2jax: {
            skipTags: ['script', 'noscript', 'style', 'textarea', 'pre'],
            ignoreClass: 'tex2jax_ignore|ignore_php',
            // The default math delimiters are $$...$$ and \[...\] for displayed mathematics, and \(...\) for in-line mathematics.
            inlineMath: [ ['$','$'], ['\\(','\\)'] ],
            displayMath: [ ['$$','$$'], ['\\[','\\]'] ],
            processEscapes: true
        },
        'HTML-CSS': { availableFonts: ['TeX'], linebreaks: {automatic: true}},
        TeX: {
            // equationNumbers: { autoNumber: ['AMS'], useLabelIds: true },
            extensions: ['AMSmath.js','AMSsymbols.js','noErrors.js','noUndefined.js'],
            Macros: {
                argmax: ['\\operatorname*{arg\\,max}'],
                braket: ['{\\langle #1 \\rangle}', 1],
                Abs: ['\\left\\lvert #2 \\right\\rvert_{\\text{#1}}', 2, ''],
            }
        },
        extensions: ['jsMath2jax.js', 'tex2jax.js'],
        messageStyle: 'none'
    });
</script>")

  (use-package pandoc-mode
    :config
    (add-hook 'markdown-mode-hook 'pandoc-mode)
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))
  )
(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode))

(use-package howdoi)

(use-package manage-minor-mode)

(use-package xkcd)

(use-package golden-ratio
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale t)

  (defun pl/helm-alive-p ()
    (if (boundp 'helm-alive-p)
        (symbol-value 'helm-alive-p)))
  (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p))

(use-package dumb-jump
  :diminish dumb-jump-mode
  :bind (("C-M-g" . dumb-jump-go)
         ("C-M-p" . dumb-jump-back)
         ("C-M-q" . dumb-jump-quick-look)))

(use-package fasd
  :bind
  ("C-h C-/" . fasd-find-file)
  :config
  (global-fasd-mode 1))
