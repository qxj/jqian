;; Load CEDET

(load "cedet")
;; Load CEDET
(setq semantic-load-turn-useful-things-on t)

;;{{{ decide to load which helper support?
;; * This enables the database and idle reparse engines
;; (semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as the nascent intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-guady-code-helpers)

;; * Most fully featured helper support
;; (semantic-load-enable-excessive-code-helpers)
;;}}}

;; (semantic-load-enable-semantic-debugging-helpers)

(require 'semantic-decorate-include)

;; gcc setup
(require 'semantic-gcc)

;; smart complitions
(require 'semantic-ia)

(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))


;; eassit
(require 'eassist)

(dolist (map (list
              c-mode-base-map
              python-mode-map
              emacs-lisp-mode-map))
  (apply-define-key
   map
   `(
     ("C-c , s" eassist-switch-h-cpp)
     ("C-c , l" eassist-list-methods)
     ("C-c , r" semantic-symref)
     ;; ("<C-return>" semantic-ia-complete-symbol-menu)
     ;; ("C-c , c" semantic-ia-complete-symbol)
     ;; ("C-c , i" semantic-complete-analyze-inline)
     ;; ("C-c , =" semantic-decoration-include-visit)
     ;; ("C-c , j" semantic-ia-fast-jump)
     ;; ("C-c , q" semantic-ia-show-doc)
     ;; ("C-c , s" semantic-ia-show-summary)
     ;; ("C-c , p" semantic-analyze-proto-impl-toggle)
     ("C-c , h" senator-fold-tag-toggle))))

;; customization
(custom-set-variables
 '(semantic-idle-scheduler-idle-time 3)
 '(semantic-self-insert-show-completion-function
   (lambda nil (semantic-ia-complete-symbol-menu (point)))))

(when window-system
  (global-semantic-folding-mode 1)
  (global-semantic-tag-folding-mode 1))

(global-semantic-idle-tag-highlight-mode 1)

;; enable support for gnu global
(unless mswin
  (require 'semanticdb-global)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode))

;; enable support for exuberent ctags
(when (and (fboundp 'semantic-ectag-version)
           (semantic-ectag-version))
  (require 'semanticdb-ectag)
  (semantic-load-enable-primary-exuberent-ctags-support))

;; Semantic search scope
;; (setq semanticdb-project-roots
;;    (list
;;     (expand-file-name "~/src")))

;; semantic cache directory
(setq semanticdb-default-save-directory my-temp-dir)

;; keybind
(global-set-key (kbd "M-?") 'semantic-ia-complete-symbol-menu)

(autoload 'senator-try-expand-semantic "senator")
;; Time in seconds of idle before scheduling events
(setq semantic-idle-scheduler-idle-time 5)
;; Time in seconds of idle before scheduling big work.
(setq semantic-idle-scheduler-work-idle-time 10)
;; Maximum size in bytes of buffers automatically reparsed
(setq semantic-idle-scheduler-max-buffer-size 100000)


(global-srecode-minor-mode 1)

(if is-after-emacs-23
    (cogre-uml-enable-unicode))

;; if windows system, add header file as far as possible
(if (or mswin cygwin)
    (dolist (mode '(c-mode c++-mode))
      (semantic-add-system-include (concat my-cygwin-dir "usr/include/") mode)))

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
