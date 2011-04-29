;; (load "cedet")

(defvar cedet-enable nil)

;; try to load external cedet
(let ((path "~/src/cedet-1.0/common/cedet.el"))
  (when (file-exists-p path)
    (load-file path)
    (setq cedet-enable t)

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

    ;;# vss is useful
    (deh-section "viss-bookmark"
      (enable-visual-studio-bookmarks)
      (deh-define-key global-map
        ((kbd "<f2>") . 'viss-bookmark-toggle)
        ((kbd "<C-f2>") . 'viss-bookmark-next-buffer)
        ((kbd "<S-f2>") . 'viss-bookmark-prev-buffer)
        ((kbd "<C-S-f2>") . 'viss-bookmark-clear-all-buffer))
      )

    ;;# turn on pulse
    (pulse-toggle-integration-advice (if window-system 1 -1))))

;; try to load internal offical cedet
(when (and (boundp 'semantic-mode) (null cedet-enable))
  (locate-library "semantic-ctxt")
  (require 'cedet nil 'noerror)
  (setq cedet-enable t)

  (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                    global-semanticdb-minor-mode
                                    global-semantic-idle-summary-mode
                                    global-semantic-mru-bookmark-mode
                                    global-semantic-highlight-edits-mode
                                    global-semantic-show-parser-state-mode))
  (semantic-mode 1)

  ;;# pulse
  (setq pulse-command-advice-flag (if window-system 1 nil))
  (defadvice goto-line (after pulse-advice activate)
    "Cause the line that is `goto'd to pulse when the cursor gets there."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice exchange-point-and-mark (after pulse-advice activate)
    "Cause the line that is `goto'd to pulse when the cursor gets there."
    (when (and pulse-command-advice-flag (interactive-p)
               (> (abs (- (point) (mark))) 400))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice find-tag (after pulse-advice activate)
    "After going to a tag, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice tags-search (after pulse-advice activate)
    "After going to a hit, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice tags-loop-continue (after pulse-advice activate)
    "After going to a hit, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice pop-tag-mark (after pulse-advice activate)
    "After going to a hit, pulse the line the cursor lands on."
    (when (and pulse-command-advice-flag (interactive-p))
      (pulse-momentary-highlight-one-line (point))))
  (defadvice imenu-default-goto-function (after pulse-advice activate)
    "After going to a tag, pulse the line the cursor lands on."
    (when pulse-command-advice-flag
      (pulse-momentary-highlight-one-line (point))))

  ;; fix `semantic-mrub-switch-tags' to jump back
  (defadvice push-mark (around semantic-mru-bookmark activate)
    "Push a mark at LOCATION with NOMSG and ACTIVATE passed to `push-mark'.
If `semantic-mru-bookmark-mode' is active, also push a tag onto
the mru bookmark stack."
    (semantic-mrub-push semantic-mru-bookmark-ring
                        (point)
                        'mark)
    ad-do-it))

(deh-section-if "cedet"
  cedet-enable

  ;; semantic cache directory
  (setq semanticdb-default-save-directory my-temp-dir)

  ;; CEDET tweaks: http://stackoverflow.com/questions/3807345/cedet-scalability-tips/3808830#3808830
  (setq semantic-idle-scheduler-idle-time 5
        semantic-idle-scheduler-work-idle-time 60
        semantic-idle-scheduler-max-buffer-size 100000
        semantic-idle-work-parse-neighboring-files-flag nil
        semantic-idle-work-update-headers-flag t) ; if slow, disable it

  (deh-section "semantic-tags"
    ;; exuberent ctags
    (ignore-errors (semantic-load-enable-primary-exuberent-ctags-support))

    ;; global gtags
    (when (executable-find "global")
      (semanticdb-enable-gnu-global-databases 'c-mode)
      (semanticdb-enable-gnu-global-databases 'c++-mode)))

  (deh-section "cedet-hippie"
    ;; hippie-try-expand setting
    (autoload 'senator-try-expand-semantic "senator")
    (dolist (hook (list
                   c-mode-common-hook
                   emacs-lisp-mode-hook))
      (add-to-list 'hippie-expand-try-functions-list
                   'senator-try-expand-semantic
                   'semantic-ia-complete-symbol)))

  (deh-section "semantic-imenu"
    ;; imenu, expand all functions
    (setq semantic-imenu-buckets-to-submenu nil
          semantic-imenu-sort-bucket-function 'semantic-sort-tags-by-type-increasing
          semantic-which-function-use-color t)

    (defvar ywb-semantic-imenu-function (symbol-function 'semantic-create-imenu-index))
    (defun ywb-toggle-semantic-imenu ()
      (interactive)
      (if (eq (symbol-function 'semantic-create-imenu-index)
              (symbol-function 'imenu-default-create-index-function))
          (progn
            (fset 'semantic-create-imenu-index ywb-semantic-imenu-function)
            (message "Using semantic-create-imenu-index"))
        (fset 'semantic-create-imenu-index
              (symbol-function 'imenu-default-create-index-function))
        (message "Using imemu default"))))

  (deh-require 'ede
    ;; (setq semantic-c-obey-conditional-section-parsing-flag nil) ; ignore #if
    (setq ede-locate-setup-options
          '(ede-locate-global ede-locate-locate ede-locate-base)
          ede-project-placeholder-cache-file
          (expand-file-name "ede-project.el" my-temp-dir))

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

  (deh-require 'semantic-c
    ;;# Semantic search scope of header files
    (defconst cedet-user-include-dirs
      (list ".." "../include" "../inc" "../common" "../public" "../hdr"
            "../.." "../../include" "../../inc" "../../common" "../../public"
            "../../hdr"))
    (defconst cedet-win32-include-dirs
      (list "C:/Cygwin/include"
            "C:/Cygwin/include/c++/3.4.5"
            "C:/Cygwin/include/c++/3.4.5/mingw32"
            "C:/Cygwin/include/c++/3.4.5/backward"
            "C:/Cygwin/lib/gcc/mingw32/3.4.5/include"
            "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"))
    (let ((include-dirs cedet-user-include-dirs))
      (when (eq system-type 'windows-nt)
        (setq include-dirs (append include-dirs cedet-win32-include-dirs)))
      (mapc (lambda (dir)
              (dolist (mode '(c-mode c++-mode))
                (semantic-add-system-include dir mode)))
            include-dirs))
    ;; (setq semanticdb-project-roots (list (expand-file-name "/")))

    (when (executable-find "gcc")
      (semantic-gcc-setup)))

  (deh-require-if 'semantic-tag-folding
    window-system
    (global-semantic-tag-folding-mode 1)
    (global-set-key (kbd "C-?") 'global-semantic-tag-folding-mode)
    (deh-define-key semantic-tag-folding-mode-map
      ((kbd "C-c , -") . 'semantic-tag-folding-fold-block)
      ((kbd "C-c , +") . 'semantic-tag-folding-show-block)
      ((kbd "C-_")     . 'semantic-tag-folding-fold-all)
      ((kbd "C-+")     . 'semantic-tag-folding-show-all)))

  (deh-require 'senator
    ;; senator-prefix-key: "C-c ,"
    (deh-define-key senator-prefix-map
      ("G" . 'semantic-symref)
      ((kbd "RET") . 'semantic-ia-complete-symbol-menu)
      ("c" . 'semantic-ia-complete-symbol)
      ("=" . 'semantic-decoration-include-visit)
      ("j" . 'semantic-ia-fast-jump)
      ("J" . 'semantic-complete-jump)
      ("q" . 'semantic-ia-show-doc)
      ("s" . 'semantic-ia-show-summary)
      ("p" . 'semantic-analyze-proto-impl-toggle)
      ("b" . 'semantic-ia-fast-jump-or-back)
      ("B" . 'semantic-ia-fast-jump-back))

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
        (semantic-ia-fast-jump (point)))))

  (deh-require 'eassist
    (deh-define-key c-mode-base-map
      ((kbd "C-c A")   . 'eassist-switch-h-cpp))
    (deh-local-set-keys (c-mode-common-hook
                         python-mode-hook
                         emacs-lisp-mode-hook)
      ((kbd "C-c , l") . 'eassist-list-methods)
      ((kbd "C-c L") . 'eassist-list-methods)))

  (deh-require 'pulse
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
        (pulse-momentary-highlight-one-line (point))))))
