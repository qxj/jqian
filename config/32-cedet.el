;; -*- mode: Emacs-Lisp -*-

;; (load "cedet")

;; internal cedet setting for emacs23.2+
(deh-package semantic
  :disabled
  :config
  (load-library "semantic/loaddefs")
  (setq semantic-default-submodes '(global-semanticdb-minor-mode
                                    global-semantic-idle-summary-mode
                                    global-semantic-idle-completions-mode
                                    global-semantic-idle-scheduler-mode
                                    global-semantic-highlight-func-mode
                                    global-semantic-decoration-mode
                                    ;; global-semantic-stickyfunc-mode
                                    global-semantic-mru-bookmark-mode))

  (semantic-mode 1)

  (global-semantic-highlight-edits-mode (if window-system 1 -1))
  (global-semantic-show-unmatched-syntax-mode 1)
  (global-semantic-show-parser-state-mode 1)
  ;; (global-semantic-idle-breadcrumbs-mode (if window-system 1 -1))
  (global-semantic-idle-local-symbol-highlight-mode 1)

  ;; semantic cache directory
  (setq semanticdb-default-save-directory my-data-dir
        ;; semanticdb-persistent-path '(project)
        ;; semanticdb-project-predicate-functions nil ; TODO: add predicates
        ;; semanticdb-find-default-throttle '(local project unloaded system recursive)
        )

  ;; CEDET tweaks: http://stackoverflow.com/questions/3807345/cedet-scalability-tips/3808830#3808830
  (setq semantic-idle-scheduler-idle-time 5
        semantic-idle-scheduler-work-idle-time 60
        semantic-idle-scheduler-max-buffer-size 100000
        semantic-idle-work-parse-neighboring-files-flag t
        semantic-idle-work-update-headers-flag t) ; if slow, disable it

  (setq semantic-which-function-use-color t)

  (add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb)))

  (require 'semantic/decorate/include nil 'noerror)
  (semantic-toggle-decoration-style "semantic-tag-boundary" -1)

  ;;# imenu, expand all functions
  (setq semantic-imenu-bucketize-file nil
        semantic-imenu-buckets-to-submenu nil
        semantic-imenu-sort-bucket-function 'semantic-sort-tags-by-type-increasing)
  (deh-add-hook c-mode-common-hook
    (setq imenu-create-index-function 'semantic-create-imenu-index))

  ;; WORKAROUND: it should be unnecessary, maybe it's a bug of internal
  ;; semantic
  (require 'semantic/analyze/refs)

  ;;# include path
  ;; (add-to-list 'semantic-c-dependency-system-include-path "/usr/local/include")

  (bind-keys
   :map semantic-mode-map
    ("C-c , c" . semantic-ia-complete-symbol)
    ("C-c , =" . semantic-decoration-include-visit)
    ("C-c , q" . semantic-ia-show-doc)
    ("C-c , s" . semantic-ia-show-summary)
    ("C-c , t" . semantic-analyze-proto-impl-toggle)
    ("C-c , b" . semantic-ia-fast-jump-or-back)
    ("C-c , B" . semantic-ia-fast-jump-back))

  ;;# wrap `semantic-ia-fast-jump'
  (defun semantic-ia-fast-jump-back ()
    (interactive)
    (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
        (error "Semantic Bookmark ring is currently empty"))
    (let* ((ring (oref semantic-mru-bookmark-ring ring))
           (alist (semantic-mrub-ring-to-assoc-list ring))
           (first (cdr (car alist))))
      ;; (if (semantic-equivalent-tag-p (oref first tag) (semantic-current-tag))
      ;;     (setq first (cdr (car (cdr alist)))))
      (semantic-mrub-visit first)
      (ring-remove ring 0)))
  (defun semantic-ia-fast-jump-or-back (&optional back)
    (interactive "P")
    (if back
        (semantic-ia-fast-jump-back)
      (semantic-ia-fast-jump (point))))
  ;; fix `semantic-mrub-switch-tags' to jump back
  (defadvice push-mark (around semantic-mru-bookmark activate)
    "Push a mark at LOCATION with NOMSG and ACTIVATE passed to `push-mark'.
If `semantic-mru-bookmark-mode' is active, also push a tag onto
the mru bookmark stack."
    (semantic-mrub-push semantic-mru-bookmark-ring
                        (point) 'mark) ad-do-it)

  ;; integrated with external tools
  (unless (eq system-type 'windows-nt)
    (when (executable-find "gcc") (semantic-gcc-setup))
    (when (executable-find "global")
      (semanticdb-enable-gnu-global-databases 'c-mode)
      (semanticdb-enable-gnu-global-databases 'c++-mode)))

  ;;# Semantic search scope of header files
  (mapc (lambda (dir)
          (dolist (mode '(c-mode c++-mode))
            (semantic-add-system-include dir mode)))
        my-include-dirs))

(deh-package senator
  :disabled
  :commands (senator-try-expand-semantic)
  ;; hippie-try-expand setting
  (dolist (hook '(c-mode-common-hook
                  emacs-lisp-mode-hook))
    (add-to-list 'hippie-expand-try-functions-list
                 'senator-try-expand-semantic)))

;; contrib/semantic-tag-folding.el
(deh-package semantic-tag-folding
  :disabled
  :bind
  ("C-?" . global-semantic-tag-folding-mode)
  :config
  (global-semantic-tag-folding-mode 1)
  (bind-keys
   :map semantic-tag-folding-mode-map
    ("C-c , -" . semantic-tag-folding-fold-block)
    ("C-c , +" . semantic-tag-folding-show-block)
    ("C-_"     . semantic-tag-folding-fold-all)
    ("C-+"     . semantic-tag-folding-show-all)))

(deh-package linemark
  :disabled
  :bind
  ("<f2>"     . viss-bookmark-toggle)
  ("<C-f2>"   . viss-bookmark-next-buffer)
  ("<S-f2>"   . viss-bookmark-prev-buffer)
  ("<C-S-f2>" . viss-bookmark-clear-all-buffer)
  :config
  (enable-visual-studio-bookmarks) ; vss is useful
  )

(deh-package eassist
  :disabled
  :config
  (bind-keys
   :map semantic-mode-map
    ("C-c A" . eassist-switch-h-cpp)
    ("C-c L" . eassist-list-methods)))

(deh-package ede
  :config
  ;; (setq semantic-c-obey-conditional-section-parsing-flag nil) ; ignore #if
  (setq ede-project-placeholder-cache-file
        (expand-file-name "ede-project.el" my-data-dir)
        ede-locate-setup-options
        '(ede-locate-global ede-locate-locate ede-locate-base))

  (global-ede-mode 1)

  ;; Ede project support
  ;; M-x `ede-new' to generate Project.ede to project root directory.
  (defun my-ede-new (projname
                     filename
                     &optional incdir
                     &optional cmd)
    "Generate a Project.ede for current project, instead of `ede-new'.

refer: http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html#sec10"
    (interactive
     (list
      (read-string "Project name: ")
      (read-file-name "Choose a file in the project root directory (only as an anchor): ")
      (if (y-or-n-p "Do you have extra include path? ")
          (read-directory-name "Extra include path: "))
      (if (y-or-n-p "Will you customize compile command? ")
          (read-string "Default compile command is `make': " "make"))))
    (let ((current-dir (file-name-directory
                        (or filename
                            (buffer-file-name (current-buffer))
                            default-directory))))
      (find-file (expand-file-name "Project.ede" current-dir))
      (lisp-interaction-mode)
      (insert (format "(ede-cpp-root-project \"%s\"
                      :name \"%s\"
                      :file \"%s\"
                      :targets 'nil
                      :include-path '(\"/\")" projname projname filename))
      (if incdir
          (insert (format "\n
                      :system-include-path '(\"%s\")" incdir)))
      (if cmd
          (insert (format "\n
                      :local-variables (list
                                        (cons 'compile-command '(my-gen-compile-string \"%s\")))" cmd)))
      (insert ")")))
  (defun my-gen-compile-string (&optional cmd)
    "Generates compile string for compiling project"
    (let* ((current-dir (file-name-directory
                         (or (buffer-file-name (current-buffer))
                             default-directory)))
           (prj (ede-current-project current-dir))
           (root-dir (ede-project-root-directory prj))
           (compile-cmd (if (null cmd) "make" cmd))
           )
      (concat "cd " root-dir ";" compile-cmd)))
  ) ;- ede end

(deh-package pulse
  :config
  (setq pulse-command-advice-flag (if window-system 1 nil)
        pulse-delay 0.08)
  (add-hook 'next-error-hook 'pulse-line-hook-function)
  ;; pre defines in cedet trunk
  (defmacro pulse-defadvice (func-name)
    `(defadvice ,func-name (after pulse-advice activate)
       (when (and pulse-command-advice-flag (called-interactively-p))
         (pulse-momentary-highlight-one-line (point)))))
  (dolist (func '(goto-line exchange-dot-and-mark exchange-point-and-mark-nomark find-tag
                            tags-search tags-loop-continue pop-tag-mark imenu-default-goto-function
                            my-switch-recent-buffer ibuffer-visit-buffer joc-dired-single-buffer
                            switch-to-buffer ido-switch-buffer previous-buffer next-buffer
                            beginning-of-buffer end-of-buffer viss-bookmark-next-buffer
                            viss-bookmark-prev-buffer ff-find-other-file pager-page-up
                            pager-page-down recenter-top-bottom))
    (pulse-defadvice func)))
