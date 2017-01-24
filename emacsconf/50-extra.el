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
   ("h" . helm-bm)
   ("t" . bm-toggle-cycle-all-buffers))
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
   ("p"  . bm-show-prev)))

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
  :bind ("M-1" . sdcv-search))

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
