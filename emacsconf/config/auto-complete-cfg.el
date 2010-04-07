;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; auto-complete-cfg.el --- auto complete settings
;; Time-stamp: <2010-03-16 06:27:29 Tuesday by tty0>


(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-yasnippet nil t)
(require 'auto-complete-c)
(require 'auto-complete-etags)
(require 'auto-complete-extension)
(require 'auto-complete-octave)
(require 'ac-anything)
(require 'rcodetools)
(require 'auto-complete+)

;; After do this, isearch any string, M-: (match-data) always
;; return the list whose elements is integer
(global-auto-complete-mode 1)

(apply-define-key
 ac-complete-mode-map
 `(("<return>"   nil)
   ("RET"        nil)
   ("M-j"        ac-complete)
   ("TAB"        ac-complete)
   ("M-n"        ac-next)
   ("M-p"        ac-previous)))

(setq ac-auto-start 3)           ;; auto complete from third word.
(setq ac-dwim t)                 ;; do what i mean
(setq ac-candidate-menu-height 20)
(setq ac-candidate-max ac-candidate-menu-height)
(setq ac-override-local-map nil) ;; don't override local map

(set-default 'ac-sources
             '(ac-source-semantic
               ac-source-yasnippet
               ac-source-abbrev
               ac-source-words-in-buffer
               ac-source-words-in-all-buffer
               ac-source-imenu
               ac-source-files-in-current-dir
               ac-source-filename))
(setq ac-modes ac+-modes)


;;; Lisp mode
(dolist (hook (list
               'emacs-lisp-mode-hook
               'lisp-interaction-mode
               ))
  (add-hook hook '(lambda ()
                    (add-to-list 'ac-sources 'ac-source-symbols))))

(setq
 ac-trigger-commands
 '(self-insert-command
   autopair-insert-or-skip-quote
   autopair-backspace
   autopair-extra-skip-close-maybe
   c-electric-backspace
   c-electric-backspace-kill))

(defun ac-settings-4-lisp ()
  "Auto complete settings for lisp mode."
  (setq ac-omni-completion-sources '(("\\<require\s+'" ac+-source-elisp-features)
                                     ("\\<load\s+\"" ac-source-emacs-lisp-features)))
  (ac+-apply-source-elisp-faces)
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-symbols
          ac-source-semantic
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-imenu
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-java ()
  (setq ac-omni-completion-sources (list (cons "\\." '(ac-source-semantic))
                                         (cons "->" '(ac-source-semantic))))
  (setq ac-sources
        '(;;ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-c ()
  (setq ac-omni-completion-sources (list (cons "\\." '(ac-source-semantic))
                                         (cons "->" '(ac-source-semantic))))
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-c-keywords
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-cpp ()
  (setq ac-omni-completion-sources
        (list (cons "\\." '(ac-source-semantic))
              (cons "->" '(ac-source-semantic))))
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-c++-keywords
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-text ()
  (setq ac-sources
        '(;;ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-imenu)))

(defun ac-settings-4-eshell ()
  (setq ac-sources
        '(;;ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename
          ac-source-symbols
          ac-source-imenu)))

(defun ac-settings-4-ruby ()
  (setq ac-omni-completion-sources
        (list (cons "\\." '(ac-source-rcodetools))
              (cons "::" '(ac-source-rcodetools)))))

(defun ac-settings-4-html ()
  (setq ac-sources
        '(;;ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename)))

(dolist (hook (list 'lisp-mode-hook 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook
                    'svn-log-edit-mode-hook 'change-log-mode-hook))
  (add-hook hook 'ac-settings-4-lisp))

(apply-args-list-to-fun
 'add-hook
 `(('java-mode-hook   'ac-settings-4-java)
   ('c-mode-hook      'ac-settings-4-c)
   ('c++-mode-hook    'ac-settings-4-cpp)
   ('text-mode-hook   'ac-settings-4-text)
   ('eshell-mode-hook 'ac-settings-4-eshell)
   ;; ('ruby-mode-hook   'ac-settings-4-ruby)
   ('html-mode-hook   'ac-settings-4-html)))

(dolist (mode ac-modes)
  (let ((mode-name (symbol-name mode)))
    (when (and (intern-soft mode-name) (intern-soft (concat mode-name "-map")))
      (define-key (symbol-value (concat-name mode-name "-map")) (kbd "C-c a") 'ac-start))))

(provide 'auto-complete-cfg)
