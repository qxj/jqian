;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; ilisp-cfg.el --- ilisp settings
;; Time-stamp: <2010-01-28 16:22:26 julian>

(set-variable 'inferior-lisp-program (concat my-cygwin-dir "usr/local/bin/gcl.exe"))
(autoload 'fi:common-lisp "fi-site-init" "" t)

; Edit the directory path below for your directory
; Put this file directly into C:\
; When you first start GCL, enter:       (load "C:/loadf.lsp")
; Then you can use loadf to load files:  (loadf "mydir/myfile.lsp")

(defun loadf (filestring)
  (load (concatenate 'string
		     "D:/Data/"  ; edit this!
		     filestring)))


;; ILISP/Inferior Lisp hook customizations
(add-hook 'lisp-mode-hook
	  (lambda ()
	    (imenu-add-to-menubar "Symbols")
	    (outline-minor-mode)
	    (make-local-variable 'outline-regexp)
	    (setq outline-regexp "^(.*")
	    (ignore-errors (semantic-default-elisp-setup))
	    (set (make-local-variable lisp-indent-function)
		 'common-lisp-indent-function)))

(add-hook 'ilisp-load-hook
	  '(lambda ()
	     (defkey-ilisp [(control c) (e)] 'eval-in-lisp)
	     (defkey-ilisp [(control c) (\;)] 'insert-balanced-comments)
	     (defkey-ilisp [(control c) (:)] 'remove-balanced-comments)
	     (defkey-ilisp [(control c) (x)] 'copy-eval-dwim-lisp)
	     ;; Set the inferior Lisp directory to the directory of
	     ;; the buffer that spawned it on the first prompt.
	     (add-hook 'ilisp-init-hook
		       '(lambda ()
			  (default-directory-lisp ilisp-last-buffer)))))

(defun my-setup-ilisp ()
  "Set up common variables used by ilisp."
  (interactive)
  (setq ilisp-*use-fsf-compliant-keybindings* t
	ilisp-*arglist-message-lisp-space-p* t
	ilisp-print-info-message-command t
	lisp-no-popper t)
  (require 'completer)
  (require 'ilisp)
  ;; Fix clisp interaction buffer (Windows)
  (modify-coding-system-alist 'process "lisp" 'unix)
  ;; All the *.d and *.lisp sources are in UTF-8 encoding.
  (modify-coding-system-alist 'file "\\.\\(d\\|lisp\\)\\'" 'utf-8))