;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; hideshow-cfg.el ---
;; Time-stamp: <2010-04-08 10:35:45 Thursday by jqian>
;; Created: 2010 Julian Qian
;; Version: $Id: hideshow-cfg.el,v 0.0 2010/01/29 09:34:19 julian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))


;;{{{ Hide selected region by yourself
(require 'hide-region)
(setq hide-region-before-string "@[Hide ")
(setq hide-region-after-string "Region]@\n")
;;}}}

;;{{{ Hide function block during programming
(require 'hideshow)

(defvar hs-headline-max-len 30 "*Maximum length of `hs-headline' to display.")

(setq hs-isearch-open t)

(defun hs-display-headline ()
  (let* ((len (length hs-headline))
         (headline hs-headline)
         (postfix ""))
    (when (>= len hs-headline-max-len)
      (setq postfix "...")
      (setq headline (substring hs-headline 0 hs-headline-max-len)))
    (if hs-headline (concat headline postfix " ") "")))

(setq-default mode-line-format
              (append '((:eval (hs-display-headline))) mode-line-format))

(setq hs-set-up-overlay 'hs-abstract-overlay)

(defun hs-abstract-overlay (ov)
  (let ((str (format "[This block hide %d lines]" (count-lines (overlay-start ov) (overlay-end ov)))) text)
    (if window-system
        (setq text (propertize str 'face 'yellow-forestgreen-face))
      (setq text (propertize str 'face 'white-red-face)))
    (overlay-put ov 'display text)))

;;; mode setting
(dolist (hook (list
               'c-mode-common-hook
               'lisp-mode-hook
               'emacs-lisp-mode-hook
               'python-mode-hook
               'java-mode-hook))
  (add-hook hook 'hs-minor-mode))

(define-prefix-command 'hs-mode-map)
(global-set-key (kbd "C-c h") 'hs-mode-map)
(dolist (map (list
              c-mode-base-map
              python-mode-map
              emacs-lisp-mode-map))
  (apply-define-key
   map
   `(("C-c h h" hs-hide-block)
     ("C-c h H" hs-hide-all)
     ("C-c h s" hs-show-block)
     ("C-c h S" hs-show-all)
     ("C-c h t" hs-toggle-hiding)
     ;; hide-region-mode
     ("C-c h r" hide-region-hide)
     ("C-c h g" hide-region-toggle)
     ("C-c h u" hide-region-unhide)
     ("C-c h U" hide-region-unhide-all))))
;;}}}

(provide 'hideshow-cfg)

;;; hideshow-cfg.el ends here
