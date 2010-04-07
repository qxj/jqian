;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; gud-cfg.el ---
;; Time-stamp: <2010-03-11 11:40:49 Thursday by julian>
;; Created: 2010 Julian Qian
;; Version: $Id: gud-cfg.el,v 0.0 2010/01/28 10:04:57 julian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))

(require 'gud)

(apply-define-key
 c-mode-base-map
 `(("C-c g" gdb)
   ("C-c b" gud-break)
   ("C-c B" gud-remove)))

(apply-define-key
 gud-mode-map
 `(("C-c B" gud-remove)
   ("M-s"   view)
   ("M-m"   comint-previous-matching-input)
   ("M-M"   comint-next-matching-input)
   ("C-c r" gud-run)
   ("C-c f" gud-finish)
   ("M-j"   gud-next)
   ("M-k"   gud-step)
   ("M-c"   gud-cont)
   ("M-C"   capitalize-word)
   ("C-c m" make)))

;; kill according buffers when exit gdb
(add-hook 'gdb-mode-hook 'kill-buffer-when-shell-command-exit)

;; show gdb mouse promption
(gud-tooltip-mode 1)

;; debug
(setq gdb-many-windows t)
(setq gdb-use-inferior-io-buffer t)

(add-hook 'gdb-mode-hook
          (lambda ()
            (gud-def my-watch "watch %e"
                     "\C-w" "Watch my variables.")))


(provide 'gud-cfg)

;;; gud-cfg.el ends here
