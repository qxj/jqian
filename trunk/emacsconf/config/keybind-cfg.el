;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; keybind-cfg.el --- keybind settings
;; Time-stamp: <2010-05-12 14:54:42 Wednesday by jqian>

;;; region action
(global-set-key [f4] 'indent-region)
(global-set-key [(shift f4)] 'fill-region)
;;; like windows action in Win32 system
(global-set-key [(meta f4)] 'delete-frame)
;;; reload from the disk
(global-set-key [f5] 'revert-buffer)
;;; winner
(global-set-key [(shift f9)] 'winner-undo)
(global-set-key [(shift f10)] 'winner-redo)
;;; go to previous or next buffer
(global-set-key [(f11)] 'bury-buffer)
(global-set-key [(shift f11)]
                (lambda()
                  (interactive)
                  (switch-to-buffer (car (reverse (buffer-list))))))
(global-set-key [f12]
                (lambda ()
                  (interactive)
                  (switch-to-buffer (other-buffer))))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-n") 'pager-row-down)
(global-set-key (kbd "M-p") 'pager-row-up)

(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "C-c j") 'ffap)
(global-set-key (kbd "C-c i") 'imenu)

(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-2") 'set-mark-command)

;; shell
;;; (define-key shell-mode-map '[up] 'comint-previous-input)
;;; (define-key shell-mode-map '[down] 'comint-next-input)
;; default
;;; (global-set-key [(control f)] 'forward-char)
;; when isearch-mode, donot delete charactors in the buffer.
(define-key isearch-mode-map '[backspace] 'isearch-delete-char)

(define-key global-map (kbd "<escape> SPC") 'just-one-space)

;;; some useful keybinding
(defun display-buffer-name ()
  (interactive)
  (message (buffer-file-name (current-buffer))))
(defun my-kill-other-buffers (&optional list)
  "Kill other buffers except the current one."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
           (name (buffer-name buffer)))
      (if (not (string-equal name (buffer-name (current-buffer))))
          (and name ; Can be nil for an indirect buffer, if we killed the base buffer.
               (not (string-equal name ""))
               (/= (aref name 0) ?\s)
               (kill-buffer buffer))))
    (setq list (cdr list))))
(defun my-kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
;; begin setting
;; (global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'my-kill-other-buffers)
(global-set-key (kbd "M-3") 'delete-other-frames)
(global-set-key (kbd "M-4") 'my-kill-current-buffer)
(global-set-key (kbd "M-5") 'display-buffer-name)
(global-set-key (kbd "M-0") 'other-window)
(global-set-key (kbd "<M-left>") 'backward-sexp)
(global-set-key (kbd "<M-right>") 'forward-sexp)

;; C-x C-j open the directory of current buffer
(global-set-key (kbd "C-x C-j")
                (lambda ()
                  (interactive)
                  (if (buffer-file-name)
                      (dired default-directory))))

(global-set-key (kbd "C-x C-k")
                (lambda()
                  (interactive)
                  (kill-buffer (current-buffer))))


;;{{{ C-a, C-e rebind
(defun my-end-of-line ()
  (interactive)
  (if (eq (point) (line-end-position))
      (skip-chars-backward " \t")
    (move-end-of-line 1)))

(defun my-beginning-of-line ()
  (interactive)
  (if (eq (point) (line-beginning-position))
      (skip-chars-forward " \t")
    (beginning-of-line)))

;; bind it!
(global-set-key (kbd "C-a") 'my-beginning-of-line)
(global-set-key (kbd "C-e") 'my-end-of-line)
;;}}}

;;{{{ my-comment-or-uncomment-region: by Julian
(defun my-comment-or-uncomment-region (&optional line)
  "This function is to comment or uncomment a line or a region"
  (interactive "P")
  (unless (or line (and mark-active (not (equal (mark) (point)))))
    (setq line 1))
  (if line
      (save-excursion
        (comment-or-uncomment-region
         (progn
           (beginning-of-line)
           (point))
         (progn
           (end-of-line)
           (point))))
    (call-interactively 'comment-or-uncomment-region)))
;; bind it
(global-set-key [(control ?\\)] 'my-comment-or-uncomment-region)
;;}}}

;; Support by zslevin (LinuxForum GNU Emacs/XEmacs)
;; C-. find tag at point in another window -- "C-x 4 ."
;; C-, close tag view window
;; M-. find tag at point and jump
;; M-, jump back
;; C-M-, prompt tag to find and jump
(defun lev/find-tag (&optional show-only)
  "Show tag in other window with no prompt in minibuf."
  (interactive)
  (let ((default (funcall (or find-tag-default-function
                              (get major-mode 'find-tag-default-function)
                              'find-tag-default))))
    (if show-only
        (progn (find-tag-other-window default)
               (shrink-window (- (window-height) 12)) ;; limit 12 lines
               (recenter 1)
               (other-window 1))
      (find-tag default))))


(global-set-key [(control .)] '(lambda () (interactive) (lev/find-tag t)))
(global-set-key [(control ,)] 'delete-other-windows)
;; (global-set-key [(meta .)] 'lev/find-tag)
;;; (global-set-key [(meta ,)] 'pop-tag-mark)
(global-set-key [(meta ,)] 'gtags-pop-stack)
;; (global-set-key (kbd "C-M-,") 'find-tag)
(global-set-key [(meta .)] 'gtags-find-tag-from-here)
(global-set-key (kbd "C-M-,") 'gtags-find-rtag)

(global-set-key (kbd "C-x C-2") 'clone-indirect-buffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c s") '(lambda() (interactive) (switch-to-buffer "*scratch*")))

;; imenu -> ido-completing-read
(defun ido-imenu-completion (index-alist &optional prompt)
  ;; Create a list for this buffer only when needed.
  (let ((name (thing-at-point 'symbol))
        choice
        (prepared-index-alist
         (if (not imenu-space-replacement) index-alist
           (mapcar
            (lambda (item)
              (cons (subst-char-in-string ?\s (aref imenu-space-replacement 0)
                                          (car item))
                    (cdr item)))
            index-alist))))
    (when (stringp name)
      (setq name (or (imenu-find-default name prepared-index-alist) name)))
    (setq name (ido-completing-read "Index item: "
                                    (mapcar 'car prepared-index-alist)
                                    nil t nil 'imenu--history-list
                                    (and name (imenu--in-alist
                                               name prepared-index-alist) name)))
    (when (stringp name)
      (setq choice (assoc name prepared-index-alist))
      (if (imenu--subalist-p choice)
          (imenu--completion-buffer (cdr choice) prompt)
        choice))))
(defalias 'imenu--completion-buffer 'ido-imenu-completion)


;; C-d, donot put it into kill-ring
(defun my-delete-char-or-region ()
  (interactive)
  (if (and mark-active transient-mark-mode)
      (delete-region (region-beginning) (region-end))
    (call-interactively 'delete-char)))
(global-set-key (kbd "C-d") 'my-delete-char-or-region)


(require 'pager)
(global-set-key "\C-v"     'pager-page-down)
(global-set-key [next]     'pager-page-down)
(global-set-key "\M-v"     'pager-page-up)
(global-set-key [prior]    'pager-page-up)
(global-set-key '[up]    'pager-row-up)
(global-set-key '[M-kp-8]  'pager-row-up)
(global-set-key '[down]  'pager-row-down)
(global-set-key '[M-kp-2]  'pager-row-down)
;;; smooth to scroll when the cursor near edge
;;; (defadvice next-line (before wcy-next-line (arg))
;;;   (let ((pos
;;;          (save-excursion
;;;            (forward-line arg)
;;;            (point)))
;;;         (end-pos (window-end)))
;;;     (if (not (pos-visible-in-window-p pos))
;;;         (let ((linenum (1+ (count-lines end-pos pos))))
;;;           (scroll-up linenum)))))
;;; (ad-activate 'next-line)
;;; (defadvice previous-line (before wcy-previous-line (arg))
;;;   (let ((pos
;;;          (save-excursion
;;;            (forward-line (* -1 arg))
;;;            (point))))
;;;     (if (not (pos-visible-in-window-p pos))
;;;         (let ((linenum (1+ (count-lines pos (window-start)))))
;;;           (scroll-down linenum)))))
;;; (ad-activate 'previous-line)
