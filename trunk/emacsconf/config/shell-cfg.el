;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; shell-cfg.el ---
;; Time-stamp: <2010-04-21 19:42:08 Wednesday by jqian>
;; Created: 2010 Julian Qian
;; Version: $Id: shell-cfg.el,v 0.0 2010/04/21 11:35:34 jqian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))


;; Make terminal colorful
(setq ansi-color-for-comint-mode t)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; auto kill shell buffer, when exit
(defun wcy-shell-mode-hook-func  ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'wcy-shell-mode-kill-buffer-on-exit))
(defun wcy-shell-mode-kill-buffer-on-exit (process state)
  (kill-buffer (current-buffer)))

;; auto rename shell name, to open more shell window
(defun wcy-shell-mode-auto-rename-buffer (text)
  (if (eq major-mode 'shell-mode)
      (rename-buffer  (concat "shell: " default-directory) t)))
;; add-hook
(add-hook 'comint-output-filter-functions 'wcy-shell-mode-auto-rename-buffer)
(add-hook 'shell-mode-hook 'wcy-shell-mode-hook-func)
;; (setq-default shell-cd-regexp nil)
;; (setq-default shell-pushd-regexp nil)
;; (setq-default shell-popd-regexp nil)

;;{{{ multi term
(require 'multi-term)
;;}}}

(provide 'shell-cfg)

;;; shell-cfg.el ends here
