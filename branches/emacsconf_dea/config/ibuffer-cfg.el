;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; ibuffer-cfg.el ---
;; Time-stamp: <2010-05-31 14:08:30 Monday by jqian>
;; Created: 2010 Julian Qian
;; Version: $Id: ibuffer-cfg.el,v 0.0 2010/03/11 02:57:52 julian Exp $

;;

;;; Code:
;; (eval-when-compile (require 'cl))

(require 'ibuffer)
(require 'ibuf-ext nil t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(define-ibuffer-sorter file-name
  "Sort buffers by associated file name"
  (:description "file name")
  (apply 'string<
         (mapcar (lambda (buf)
                   (with-current-buffer (car buf)
                     (or buffer-file-name default-directory)))
                 (list a b))))
(define-key ibuffer-mode-map "sf" 'ibuffer-do-sort-by-file-name)

(defun ywb-ibuffer-rename-buffer ()
  (interactive)
  (call-interactively 'ibuffer-update)
  (let* ((buf (ibuffer-current-buffer))
         (name (generate-new-buffer-name
                (read-from-minibuffer "Rename buffer(to new name): "
                                      (buffer-name buf)))))
    (with-current-buffer buf
      (rename-buffer name)))
  (call-interactively 'ibuffer-update))
(defun ywb-ibuffer-find-file ()
  (interactive)
  (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                             (if (buffer-live-p buf)
                                 (with-current-buffer buf
                                   default-directory)
                               default-directory))))
    (call-interactively 'ido-find-file)))
(define-key ibuffer-mode-map "r" 'ywb-ibuffer-rename-buffer)
(define-key ibuffer-mode-map (kbd "C-x C-f") 'ywb-ibuffer-find-file)
(define-key ibuffer-mode-map " " 'scroll-up)
;; group buffers
(setq ibuffer-saved-filter-groups
      '(("default"
         ("*buffer*" (name . "\\*.*\\*"))
         ("dired" (mode . dired-mode))
         ("perl" (or (mode . cperl-mode)
                     (mode . sepia-mode)
                     (mode . perl-mode)))
         ("elisp" (or (mode . emacs-lisp-mode)
                      (mode . lisp-interaction-mode)))
         ("prog" (or (mode . c++-mode)
                     (mode . c-mode)
                     (mode . java-mode)))
         ("tags" (name . "^TAGS"))
         ("erc" (mode . erc-mode)))))
(set 'ibuffer-mode-hook
     (lambda ()
       (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-saved-filters
      '(("t" ((or (mode . latex-mode)
                  (mode . plain-tex-mode))))
        ("c" ((or (mode . c-mode)
                  (mode . c++-mode))))
        ("p" ((mode . cperl-mode)))
        ("e" ((or (mode . emacs-lisp-mode)
                  (mode . lisp-interaction-mode))))
        ("d" ((mode . dired-mode)))
        ("s" ((mode . shell-mode)))
        ("i" ((mode . image-mode)))
        ("h" ((mode . html-mode)))
        ("gnus" ((or (mode . message-mode)
                     (mode . mail-mode)
                     (mode . gnus-group-mode)
                     (mode . gnus-summary-mode)
                     (mode . gnus-article-mode))))
        ("pr" ((or (mode . emacs-lisp-mode)
                   (mode . cperl-mode)
                   (mode . c-mode)
                   (mode . c++-mode)
                   (mode . php-mode)
                   (mode . java-mode)
                   (mode . idl-mode)
                   (mode . lisp-interaction-mode))))
        ("m" ((mode . muse-mode)))
        ("w" ((or (mode . emacs-wiki-mode)
                  (mode . muse-mode))))
        ("*" ((name . "*")))))

(provide 'ibuffer-cfg)

;;; ibuffer-cfg.el ends here
