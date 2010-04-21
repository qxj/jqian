;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; edit-cfg.el --- editor settings
;; Time-stamp: <2010-04-21 19:22:37 Wednesday by jqian>


;;{{{ unicad to distinguish charset
(require 'unicad)
;;}}}

;;{{{ more highlight colors
(require 'generic-x)
;;}}}

;;{{{ make cursor become a line
(require 'bar-cursor)
;;}}}

;; kill a whole line
(setq-default kill-whole-line t)
(setq kill-ring-max 50)


;;{{{ similar to dd & yy in VIM
(defadvice kill-ring-save (before slickcopy activate compile)
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
                   (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line inste
ad."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
                   (line-beginning-position 2)))))
;;}}}

;;{{{ mark something smartly
(defun wcy-mark-some-thing-at-point()
  (interactive)
  (let* ((from (point))
         (a (mouse-start-end from from 1))
         (start (car a))
         (end (cadr a))
         (goto-point (if (= from start) end start)))
    (if (eq last-command 'wcy-mark-some-thing-at-point)
        (progn
          ;; exchange mark and point
          (goto-char (mark-marker))
          (set-marker (mark-marker) from))
      (push-mark (if (= goto-point start) end start) nil t)
      (when (and (interactive-p) (null transient-mark-mode))
        (goto-char (mark-marker))
        (sit-for 0 500 nil))
      (goto-char goto-point))))
;;}}}

;;{{{
(defun my-move-line-up (p)
  "move current line up"
  (interactive "p")
  (let ((c (current-column)))
    (beginning-of-line)
    (kill-line 1)
    (previous-line p)
    (beginning-of-line)
    (yank)
    (previous-line 1)
    (move-to-column c)))

(defun my-move-line-down (p)
  "move current line down"
  (interactive "p")
  (let ((c (current-column)))
    (beginning-of-line)
    (kill-line 1)
    (next-line p)
    (beginning-of-line)
    (yank)
    (previous-line 1)
    (move-to-column c)))

(global-set-key [C-M-up]   'my-move-line-up)
(global-set-key [C-M-down] 'my-move-line-down)
;;}}}

;;{{{ sexp related
(defun mark-whole-sexp ()
  "Mark whole sexp."
  (interactive)
  (let ((region (bounds-of-thing-at-point 'sexp)))
    (if (not region)
        (message "Can not found sexp.")
      (goto-char (car region))
      (call-interactively 'set-mark-command)
      (forward-sexp))))
(defun kill-sexp ()
  "Kill a sexp"
  (interactive)
  (mark-whole-sexp)
  (backward-kill-word-or-kill-region))
(defun copy-sexp ()
  "Copy a sexp"
  (interactive)
  (save-excursion
    (mark-whole-sexp)
    (if mark-active
        (copy-region (region-beginning) (region-end)))))
(defun my-kill-word-1 ()
  "Kill selected region when `mark-active', otherwise kill word backword, just as bash."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'backward-kill-word)))
(defun my-kill-word ()
  "Kill a whole word, even cursor lies between the word, which acts different from original `kill-word'."
  (interactive)
  (wcy-mark-some-thing-at-point)
  (my-kill-word-1))
(defun del-to-begin (&optional arg)
  "Delete words from cursor to line begin."
  (interactive "P")
  (if (not arg)
      (kill-line 0)
    (copy-region-as-kill-nomark (1+ (line-beginning-position)) (point))))
(defun lisp-mark-function (&optional allow-extend)
  (interactive "p")
  (mark-defun allow-extend)
  (let (next-is-fun)
    (save-excursion (forward-line) (setq next-is-fun (looking-at "[ \t]*(defun")))
    (if (or (looking-at "$") (and next-is-fun (not (looking-at "[ \t]*(defun"))))
      (forward-line))))
(defun mark-function ()
  "Mark function."
  (interactive)
  (cond
   ((or (equal major-mode 'c-mode) (equal major-mode 'c++-mode))
    (c-mark-function))
   ((or (equal major-mode 'emacs-lisp-mode) (equal major-mode 'lisp-mode) (equal major-mode 'lisp-interaction-mode))
    (lisp-mark-function))))
(defun comment-function (&optional arg)
  "Comment function."
  (interactive "P")
  (save-excursion
    (mark-function)
    (comment-region (region-beginning) (region-end) arg)))
;;}}}

;;{{{ some simple functions
(defmacro def-action-on-area-command (fun-name action mark-area doc)
  `(defun ,fun-name ()
     ,doc
     (interactive)
     (save-excursion
       (funcall ,mark-area)
       (call-interactively ,action))))
(apply-args-list-to-fun
 'def-action-on-area-command
  `((copy-function       'kill-ring-save 'mark-function     "Copy function.")
    (kill-function       'kill-region    'mark-function     "Kill function.")
    (indent-function     'indent-region  'mark-function     "Indent function.")
    (indent-paragraph    'indent-region  'mark-paragraph    "Indent paragraph.")
    (copy-whole-buffer   'kill-ring-save 'mark-whole-buffer "Copy whole buffer.")
    (kill-whole-buffer   'kill-region    'mark-whole-buffer "Kill whole buffer.")
    (indent-whole-buffer 'indent-region  'mark-whole-buffer "Indent whole buffer.")))
;;}}}

;;{{{ paragraph
(defun kill-whole-paragraph (&optional arg)
  "Kill whole paragraph."
  (interactive "P")
  (if arg
      (kill-paragraph nil)
    (call-interactively 'mark-paragraph)
    (call-interactively 'kill-region)))

(defun copy-whole-paragraph (&optional arg)
  "Copy whole paragraph."
  (interactive "P")
  (save-excursion
    (if arg
        (progn
          (mark-command t)
          (forward-paragraph))
      (call-interactively 'mark-paragraph))
    (call-interactively 'copy-region)))
;;}}}

;;{{{ redo
(defmacro def-redo-command (fun-name redo undo)
  "Make redo command."
  `(defun ,fun-name ()
     (interactive)
     (if (equal last-command ,redo)
         (setq last-command 'undo)
       (setq last-command nil))
     (call-interactively ,undo)
     (setq this-command ,redo)))
(def-redo-command redo 'redo 'undo)

(global-set-key (kbd "C-'") 'redo)
;;}}}

;;{{{ keybinds
(apply-define-key
 global-map
 `(("M-k" kill-whole-paragraph)
   ("M-C-k" kill-paragraph)
   ("M-C" copy-whole-paragraph)
   ("C-M-w" copy-sentence)
   ("M-U" del-to-begin)
   ("C-1" mark-whole-sexp)
   ("C-2" mark-function)
   ;; ("C-M-d" kill-sexp)
   ("M-D" my-kill-word)
   ;; ,(if window-system '("C-z" undo))
   ("C-'" redo)))
;;}}}

;;; edit-cfg.el ends here
