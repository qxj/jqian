;; (load "cedet")

;; Q: How to use ede for a project?
;; A: Create a file named "Project.ede" under the root of this project.
;; Sample:
;; (ede-cpp-root-project "cpp-tests"
;;                       :file "~/projects/lang-exp/cpp/CMakeLists.txt"
;;                       :system-include-path '("/home/ott/exp/include"
;;                                              boost-base-directory)
;;                       :local-variables (list
;;                                         (cons 'compile-command 'my-compile)
;;                                         )
;;                       )

(deh-section "cedet"
  (let ((cedet-path "~/src/cedet-1.0/common/cedet.el"))
    (when (file-exists-p cedet-path)
      (load-file cedet-path)
      (setq semantic-load-turn-useful-things-on t)

      (semantic-load-enable-code-helpers)
      ;; (semantic-load-enable-minimum-features)

      (require 'semantic-decorate-include)

      ;; gcc setup
      (require 'semantic-gcc)

      ;; smart complitions
      (require 'semantic-ia)

      (setq-mode-local c-mode semanticdb-find-default-throttle
                       '(project unloaded system recursive))
      (setq-mode-local c++-mode semanticdb-find-default-throttle
                       '(project unloaded system recursive))


      ;; keybind
      (global-set-key (kbd "M-?") 'semantic-ia-complete-symbol-menu)

      ;; eassit
      (require 'eassist)

      (dolist (hook '(c-mode-common-hook
                      python-mode-hook
                      emacs-lisp-mode-hook))
        (add-hook
         hook
         '(lambda ()
            (local-set-key (kbd "C-c , l") 'eassist-list-methods)
            (local-set-key (kbd "C-c , G") 'semantic-symref)
            (local-set-key (kbd "<C-return>") 'semantic-ia-complete-symbol-menu)
            (local-set-key (kbd "C-c , c") 'semantic-ia-complete-symbol)
            (local-set-key (kbd "C-c , =") 'semantic-decoration-include-visit)
            (local-set-key (kbd "C-c , j") 'semantic-ia-fast-jump)
            (local-set-key (kbd "C-c , q") 'semantic-ia-show-doc)
            (local-set-key (kbd "C-c , s") 'semantic-ia-show-summary)
            (local-set-key (kbd "C-c , p") 'semantic-analyze-proto-impl-toggle)
            (local-set-key (kbd "C-c , h") 'senator-fold-tag-toggle)
            ))
        )

      ;; customization
      (custom-set-variables
       '(semantic-self-insert-show-completion-function
         (lambda nil (semantic-ia-complete-symbol-menu (point)))))

      ;; (when window-system
      ;;   (global-semantic-folding-mode 1)
      ;;   (global-semantic-tag-folding-mode 1))

      ;; (global-semantic-idle-tag-highlight-mode 1)

      ;; enable support for gnu global
      (unless (eq system-type 'windows-nt)
        (require 'semanticdb-global)
        (semanticdb-enable-gnu-global-databases 'c-mode)
        (semanticdb-enable-gnu-global-databases 'c++-mode))

      ;; enable support for exuberent ctags
      (when (and (fboundp 'semantic-ectag-version)
                 (semantic-ectag-version))
        (require 'semanticdb-ectag)
        (semantic-load-enable-primary-exuberent-ctags-support))

      ;; Semantic search scope
      ;; (setq semanticdb-project-roots (list (expand-file-name "/")))
      (defconst cedet-user-include-dirs
        (list ".." "../include" "../inc" "../common" "../public"
              "../.." "../../include" "../../inc" "../../common" "../../public"))
      (defconst cedet-win32-include-dirs
        (list "C:/Cygwin/include"
              "C:/Cygwin/include/c++/3.4.5"
              "C:/Cygwin/include/c++/3.4.5/mingw32"
              "C:/Cygwin/include/c++/3.4.5/backward"
              "C:/Cygwin/lib/gcc/mingw32/3.4.5/include"
              "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"))

      (require 'semantic-c nil 'noerror)
      (let ((include-dirs cedet-user-include-dirs))
        (when (eq system-type 'windows-nt)
          (setq include-dirs (append include-dirs cedet-win32-include-dirs)))
        (mapc (lambda (dir)
                (dolist (mode '(c-mode c++-mode))
                  (semantic-add-system-include dir mode)))
              include-dirs))

      (deh-section "ede"
        ;; Ede project support
        ;; M-x `ede-new' to generate Project.ede to project root directory.
        (global-ede-mode t)
        (setq ede-project-placeholder-cache-file
              (expand-file-name "ede-project.el" my-temp-dir))

        (defun my-ede-new-cpp (projname
                               filename
                               &optional incdir
                               &optional cmd)
          "Generate a Project.ede for current cpp project, instead of `ede-new'."
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
            (insert (format "(ede-cpp-root-project \"%s\"
                      :file \"%s\"" projname filename))
            (if incdir
                (insert (format "\n                      :system-include-path '(\"%s\")" incdir)))
            (if cmd
                (insert (format "\n                      :local-variables (list
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

      ;; Enable visual bookmarks, similar to native bookmarks and bm.el
      (enable-visual-studio-bookmarks)

      ;; semantic cache directory
      (setq semanticdb-default-save-directory my-temp-dir)

      (autoload 'senator-try-expand-semantic "senator")
      ;; Time in seconds of idle before scheduling events
      (setq semantic-idle-scheduler-idle-time 5)
      ;; Time in seconds of idle before scheduling big work.
      (setq semantic-idle-scheduler-work-idle-time 10)
      ;; Maximum size in bytes of buffers automatically reparsed
      (setq semantic-idle-scheduler-max-buffer-size 100000)

      ;; hippie-try-expand setting
      (add-to-list 'hippie-expand-try-functions-list 'semantic-ia-complete-symbol)

      (global-srecode-minor-mode 1)

      (if (<= 23 emacs-major-version)
          (cogre-uml-enable-unicode))

      ;; restore imenu original setting rather than semantic-create-imenu-index
      (dolist (hook (list
                     c-mode-common-hook
                     lisp-mode-hook
                     emacs-lisp-mode-hook
                     python-mode-hook
                     java-mode-hook))
        (add-hook 'hook '(lambda ()
                           (setq imenu-create-index-function
                                 'imenu-default-create-index-function))))

      ;; enable ctags for some languages:
      ;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
      ;; (when (semantic-ectag-version)
      ;;   (semantic-load-enable-primary-exuberent-ctags-support))

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
          (message "Using imemu default")))
      )))