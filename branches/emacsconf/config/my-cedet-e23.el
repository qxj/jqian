;; -*- mode: Emacs-Lisp -*-

;; (load "cedet")

;; try to load external cedet
(deh-section-path "cedet"
  "~/src/cedet-1.0/common/cedet.el"
  (load deh-this-path t)
  ;;# Enable cedet modes
  ;; (semantic-load-enable-minimum-features)
  (semantic-load-enable-code-helpers)
  ;; (semantic-load-enable-gaudy-code-helpers)
  ;; (semantic-load-enable-excessive-code-helpers)
  ;; (semantic-load-enable-semantic-debugging-helpers)
  (if window-system
      (global-semantic-highlight-edits-mode 1))
  (global-semantic-show-parser-state-mode 1)

  (require 'semantic-decorate-include nil 'noerror)
  (semantic-toggle-decoration-style "semantic-tag-boundary" -1)
  (global-semantic-decoration-mode 1)

  ;; semantic cache directory
  (setq semanticdb-default-save-directory (expand-file-name "semanticdb" my-data-dir))

  ;; CEDET tweaks: http://stackoverflow.com/questions/3807345/cedet-scalability-tips/3808830#3808830
  (setq semantic-idle-scheduler-idle-time 5
        semantic-idle-scheduler-work-idle-time 60
        semantic-idle-scheduler-max-buffer-size 100000
        semantic-idle-work-parse-neighboring-files-flag t
        semantic-idle-work-update-headers-flag t) ; if slow, disable it

  (deh-define-key senator-mode-map
    ((kbd "C-c , RET") 'semantic-ia-complete-symbol-menu)
    ("\C-c,c"  'semantic-ia-complete-symbol)
    ("\C-c,G"  'semantic-symref)
    ("\C-c,="  'semantic-decoration-include-visit)
    ("\C-c,j"  'semantic-ia-fast-jump)
    ("\C-c,J"  'semantic-complete-jump)
    ("\C-c,q"  'semantic-ia-show-doc)
    ("\C-c,s"  'semantic-ia-show-summary)
    ("\C-c,t"  'semantic-analyze-proto-impl-toggle)
    ("\C-c,b"  'semantic-ia-fast-jump-or-back)
    ("\C-c,B"  'semantic-ia-fast-jump-back))
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

  ;; integrated with external tools
  (when (executable-find "gcc") (semantic-gcc-setup))
  (when (executable-find "global")
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode))
  (ignore-errors (semantic-load-enable-primary-exuberent-ctags-support))

  ;;# Semantic search scope of header files
  (mapc (lambda (dir)
          (dolist (mode '(c-mode c++-mode))
            (semantic-add-system-include dir mode)))
        my-include-dirs)


  (deh-section "imenu-semantic"
    ;;# imenu, expand all functions but not in submenus
    (setq-default semantic-imenu-bucketize-file nil
                  semantic-imenu-buckets-to-submenu t
                  semantic-imenu-bucketize-type-members t
                  semantic-imenu-expand-type-members t
                  semantic-imenu-sort-bucket-function 'semantic-sort-tags-by-type-increasing
                  ;; semantic-imenu-auto-rebuild-directory-indexes t
                  ;; semantic-imenu-index-directory t
                  semantic-which-function-use-color t)
    ;; (deh-add-hook 'c-mode-common-hook
    ;;   (setq imenu-create-index-function 'semantic-create-imenu-index))
    )

  (deh-section "hippie-semantic"
    (autoload 'senator-try-expand-semantic "senator")
    ;; hippie-try-expand setting
    (deh-add-hook '(c-mode-common-hook emacs-lisp-mode-hook)
      (add-to-list 'hippie-expand-try-functions-list
                   'senator-try-expand-semantic
                   'semantic-ia-complete-symbol)))

  (deh-require 'eassist
    (deh-define-key senator-mode-map
      ((kbd "C-c A")  'eassist-switch-h-cpp)
      ((kbd "C-c L")  'eassist-list-methods))
    ;; donot miss minus "-"
    (eassist-key-itself eassist-mode-map (string-to-char "-")))

  (deh-require-reserved 'linemark
    ;;# vss is useful
    (enable-visual-studio-bookmarks)
    (deh-define-key global-map
      ((kbd "<f2>")      'viss-bookmark-toggle)
      ((kbd "<C-f2>")    'viss-bookmark-next-buffer)
      ((kbd "<S-f2>")    'viss-bookmark-prev-buffer)
      ((kbd "<C-S-f2>")  'viss-bookmark-clear-all-buffer)))

  (deh-require-if 'semantic-tag-folding
    window-system
    (global-semantic-tag-folding-mode 1)
    (global-set-key (kbd "C-?") 'global-semantic-tag-folding-mode)
    (deh-define-key semantic-tag-folding-mode-map
      ((kbd "C-c , -") 'semantic-tag-folding-fold-block)
      ((kbd "C-c , +") 'semantic-tag-folding-show-block)
      ((kbd "C-_")     'semantic-tag-folding-fold-all)
      ((kbd "C-+")     'semantic-tag-folding-show-all)))

  (deh-require 'ede
    ;; (setq semantic-c-obey-conditional-section-parsing-flag nil) ; ignore #if
    (setq ede-locate-setup-options
          '(ede-locate-global ede-locate-locate ede-locate-base)
          ede-project-placeholder-cache-file
          (expand-file-name "ede-project.el" my-data-dir))

    ;; (global-ede-mode t)

    ;; Ede project support
    ;; M-x `ede-new' to generate Project.ede to project root directory.
    (defun my-ede-new (projname
                       filename
                       &optional incdir
                       &optional cmd)
      "Generate a Project.ede for current project, instead of `ede-new'."
      (interactive
       (list
        (read-string "Project name: ")
        (read-file-name "Choose a project file, such as Makefile: ")
        (if (y-or-n-p "Do you have extra include path? ")
            (read-directory-name "Extra include path: "))
        (if (y-or-n-p "Will you customize compile command? ")
            (read-string "Default compile command is `make': " "make"))))
      (let ((current-dir (file-name-directory
                          (or (buffer-file-name (current-buffer))
                              default-directory))))
        (find-file (expand-file-name "Project.ede" current-dir))
        (lisp-interaction-mode)
        (insert (format "(ede-proj-project \"%s\"
                      :name \"%s\"
                      :file \"%s\"
                      :targets 'nil" projname projname filename))
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

  (deh-require 'pulse
    (pulse-toggle-integration-advice (if window-system 1 -1))
    ;; fix `semantic-mrub-switch-tags' to jump back
    (defadvice push-mark (around semantic-mru-bookmark activate)
      "Push a mark at LOCATION with NOMSG and ACTIVATE passed to `push-mark'.
If `semantic-mru-bookmark-mode' is active, also push a tag onto
the mru bookmark stack."
      (semantic-mrub-push semantic-mru-bookmark-ring
                          (point)
                          'mark)
      ad-do-it)
    ;; my defadvices
    (defadvice my-switch-recent-buffer (after pulse-advice activate)
      (when (and pulse-command-advice-flag (interactive-p))
        (pulse-momentary-highlight-one-line (point))))
    (defadvice ibuffer-visit-buffer (after pulse-advice activate)
      (when (and pulse-command-advice-flag (interactive-p))
        (pulse-momentary-highlight-one-line (point))))
    (defadvice joc-dired-single-buffer (after pulse-advice activate)
      (when (and pulse-command-advice-flag (interactive-p))
        (pulse-momentary-highlight-one-line (point))))
    (defadvice exchange-point-and-mark-nomark (after pulse-advice activate)
      "Cause the line that is `goto'd to pulse when the cursor gets there."
      (when (and pulse-command-advice-flag (interactive-p)
                 (> (abs (- (point) (mark))) 400))
        (pulse-momentary-highlight-one-line (point))))
    (defadvice cua-exchange-point-and-mark (after pulse-advice activate)
      "Cause the line that is `goto'd to pulse when the cursor gets there."
      (when (and pulse-command-advice-flag (interactive-p)
                 (> (abs (- (point) (mark))) 400))
        (pulse-momentary-highlight-one-line (point))))
    (defadvice switch-to-buffer (after pulse-advice activate)
      "After switch-to-buffer, pulse the line the cursor lands on."
      (when (and pulse-command-advice-flag (interactive-p))
        (pulse-momentary-highlight-one-line (point))))
    (defadvice previous-buffer (after pulse-advice activate)
      "After previous-buffer, pulse the line the cursor lands on."
      (when (and pulse-command-advice-flag (interactive-p))
        (pulse-momentary-highlight-one-line (point))))
    (defadvice next-buffer (after pulse-advice activate)
      "After next-buffer, pulse the line the cursor lands on."
      (when (and pulse-command-advice-flag (interactive-p))
        (pulse-momentary-highlight-one-line (point))))
    (defadvice ido-switch-buffer (after pulse-advice activate)
      "After ido-switch-buffer, pulse the line the cursor lands on."
      (when (and pulse-command-advice-flag (interactive-p))
        (pulse-momentary-highlight-one-line (point))))
    (defadvice beginning-of-buffer (after pulse-advice activate)
      "After beginning-of-buffer, pulse the line the cursor lands on."
      (when (and pulse-command-advice-flag (interactive-p))
        (pulse-momentary-highlight-one-line (point))))
    (defadvice viss-bookmark-next-buffer (after pulse-advice activate)
      "After viss-bookmark-next-buffer, pulse the line the cursor lands on."
      (when (and pulse-command-advice-flag (interactive-p))
        (pulse-momentary-highlight-one-line (point))))
    (defadvice viss-bookmark-prev-buffer (after pulse-advice activate)
      "After viss-bookmark-prev-buffer, pulse the line the cursor lands on."
      (when (and pulse-command-advice-flag (interactive-p))
        (pulse-momentary-highlight-one-line (point))))
    (defadvice sourcepair-load (after pulse-advice activate)
      "After sourcepair-load, pulse the line the cursor lands on."
      (when (and pulse-command-advice-flag (interactive-p))
        (pulse-momentary-highlight-one-line (point))))
    ;; pulse for bm
    (defadvice bm-next (after pulse-advice activate)
      "After bm-next, pulse the line the cursor lands on."
      (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
                 (interactive-p))
        (pulse-momentary-highlight-one-line (point))))
    (defadvice bm-previous (after pulse-advice activate)
      "After bm-previous, pulse the line the cursor lands on."
      (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
                 (interactive-p))
        (pulse-momentary-highlight-one-line (point))))
    (defadvice bm-next-mouse (after pulse-advice activate)
      "After bm-next-mouse, pulse the line the cursor lands on."
      (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
                 (interactive-p))
        (pulse-momentary-highlight-one-line (point))))
    (defadvice bm-previous-mouse (after pulse-advice activate)
      "After bm-previous-mouse, pulse the line the cursor lands on."
      (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag
                 (interactive-p))
        (pulse-momentary-highlight-one-line (point))))
    ;; END pulse for bm
    ))
