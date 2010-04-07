;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; highlight-cfg.el --- keybind settings
;; Time-stamp: <2010-03-10 13:48:44 Wednesday by julian>

(require 'highlight-parentheses)

;; TODO: 最后一项不知道为啥不起作用
(setq hl-paren-colors '("red" "yellow" "cyan" "magenta" "green" "red"))

(dolist (hook (list 'find-file-hook 'help-mode-hook 'Man-mode-hook 'log-view-mode-hook
                    'compilation-mode-hook 'gdb-mode-hook 'lisp-interaction-mode-hook
                    'browse-kill-ring-mode-hook 'completion-list-mode-hook 'hs-hide-hook
                    'custom-mode-hook 'Info-mode-hook 'svn-log-edit-mode-hook
                    'package-menu-mode-hook 'dired-mode-hook 'apropos-mode-hook))
  (add-hook hook
            (lambda()
              (highlight-parentheses-mode t)) t))


(require 'highlight-symbol)
(require 'sgml-mode)

(when window-system
  (setq highlight-symbol-idle-delay 0.5)
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-interaction-mode-hook
                  java-mode-hook
                  c-mode-common-hook
                  text-mode-hook
                  html-mode-hook))
    (add-hook hook 'highlight-symbol-mode-on)))

(defun highlight-symbol-mode-on ()
  "Turn on function `highlight-symbol-mode'."
  (highlight-symbol-mode 1))

(defun highlight-symbol-mode-off ()
  "Turn off function `highlight-symbol-mode'."
  (highlight-symbol-mode -1))

;;;###autoload
(define-globalized-minor-mode global-highlight-symbol-mode highlight-symbol-mode highlight-symbol-mode-on)

;; I bind "C-x w" to `copy-sexp'
(apply-define-key
 hi-lock-map
 `(("C-x w" nil)))

(define-prefix-command 'highlight-symbol-map)
(global-set-key (kbd "C-c l") 'highlight-symbol-map)
(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map java-mode-map
                   c-mode-base-map text-mode-map html-mode-map))
  (apply-define-key
   map
   `(("C-c l l" highlight-symbol-at-point)
     ("C-c l u" highlight-symbol-remove-all)
     ("C-c l n" highlight-symbol-next)
     ("C-c l p" highlight-symbol-prev)
     ("C-c l q" highlight-symbol-query-replace)
     ("C-c l N" highlight-symbol-next-in-defun)
     ("C-c l P" highlight-symbol-prev-in-defun))))
