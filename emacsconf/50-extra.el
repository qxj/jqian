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

(use-package helm                       ; http://tuhdo.github.io/helm-intro.html
  :disabled
  :ensure t
  :diminish helm-mode
  ;; :bind-keymap* ("C-c h" . helm-command-prefix)
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-c i" . helm-semantic-or-imenu)
   ("C-x b" . helm-mini)
   ("C-c c o" . helm-recentf)
   ("C-c C-r" . helm-resume)
   ("C-x C-f" . helm-find-files)
   ("C-x M-f" . helm-for-files)
   ("C-h SPC" . helm-all-mark-rings)
   :map helm-command-map
   ("i" . helm-semantic-or-imenu)
   ("s" . helm-swoop)                   ;like occur
   ("<tab>" . helm-lisp-completion-at-point)
   ("x" . helm-register)
   ("p" . helm-projectile)
   ("a" . helm-do-grep-ag)
   ("j" . helm-grep-do-git-grep)
   :map minibuffer-local-map
   ("C-c C-l" . helm-minibuffer-history)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
   ("C-i"   . helm-execute-persistent-action) ; make TAB works in terminal
   ("C-z"   . helm-select-action)             ; list actions using C-z
   ("C-w"   . backward-kill-word)
   :map helm-find-files-map
   ("M-u" . helm-find-files-up-one-level))
  :init
  (require 'helm-config)
  ;; (global-unset-key (kbd "C-x c"))
  (helm-mode t)
  (helm-adaptive-mode t)
  (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
  :config
  ;; (bind-keys
  ;;  :map minibuffer-local-map
  ;;  ("C-c C-l" . helm-minibuffer-history))

  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))

  (when (eq system-type 'darwin)
    (setq helm-locate-command "mdfind -name %s %s"))

  ;; enable man page at point
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

  ;; enable fuzzy matching
  (setq helm-buffers-fuzzy-matching t   ; helm-mini
        helm-recentf-fuzzy-match    t   ; helm-mini
        helm-semantic-fuzzy-match t     ; helm-semantic-or-imenu
        helm-imenu-fuzzy-match    t     ; helm-semantic-or-imenu
        helm-locate-fuzzy-match nil ; helm-locate
        helm-lisp-fuzzy-completion t    ; helm-lisp-completion-at-point
        helm-ff-guess-ffap-filenames t  ; helm-find-files
        )

  (setq helm-candidate-number-limit 100)
  ;; From https://gist.github.com/antifuchs/9238468
  (setq helm-input-idle-delay 0.01  ; this actually updates things reeeelatively quickly.
        helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        ;; helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-ff-skip-boring-files t
        helm-ff-file-name-history-use-recentf t)

  (helm-autoresize-mode 1)

  (use-package helm-swoop
    :ensure t
    :defer
    :bind
    (("C-s"     . helm-swoop)
     ("C-c M-i" . helm-multi-swoop)
     ("C-x M-i" . helm-multi-swoop-all)
     :map helm-swoop-map
     ("C-s" . undefined)
     ("C-u" . my/helm-swoop-clean))
    :config
    (bind-key "M-i" 'helm-swoop-from-isearch isearch-mode-map)
    (bind-key "M-i" 'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)
    (defun my/helm-swoop-clean ()
      (interactive)
      (delete-region
       (save-excursion (move-beginning-of-line 1) (point))
       (save-excursion (move-end-of-line 1) (point))))
    )

  (use-package helm-gtags
    :ensure t
    :after cc-mode
    :if (executable-find "gtags")
    :diminish (helm-gtags-mode . "hG")
    :bind (:map helm-gtags-mode-map
                ("M-." . helm-gtags-find-tag)
                ("M-," . helm-gtags-pop-stack)
                ("M-*" . helm-gtags-pop-stack)
                ("M-s d" . helm-gtags-dwim)
                ("M-s r" . helm-gtags-find-rtag)
                ("M-s s" . helm-gtags-find-symbol)
                ("C-c i" . helm-gtags-parse-file)  ;replace imenu
                ("C-c <" . helm-gtags-previous-history)
                ("C-c >" . helm-gtags-next-history))
    :config
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t)
    (add-hook 'c-mode-hook #'helm-gtags-mode)
    (add-hook 'c++-mode-hook #'helm-gtags-mode))

  (use-package helm-c-yasnippet
    :after yasnippet
    :bind
    ("C-c y" . helm-yas-complete)
    :config
    (setq helm-yas-space-match-any-greedy t
          helm-yas-display-key-on-candidate t))

  (use-package helm-ls-git
    :ensure
    :config
    :bind (:map helm-command-map
                ("g" . helm-ls-git-ls)))

  (use-package helm-projectile
    :ensure t
    :after projectile
    :config
    (setq projectile-completion-system 'helm
          projectile-switch-project-action 'helm-projectile)
    (helm-projectile-on))

  (use-package helm-dash
    :bind ("C-x c d" . helm-dash))
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
