;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; my functions ;;;;;;;;;;;;;;;;;;;;
;; Most useful interactive function and commands for keybinds

(dolist (func '("func-dired-ext"
                "func-elisp-helper"
                "func-prog"
                "func-misc"))
  (load (expand-file-name func my-config-dir)))



;;{{{ hack comment-or-uncomment-region
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
;;}}}

;;{{{ hack kill-line
(defadvice kill-line (before check-position activate)
  "killing the newline between indented lines and remove extra spaces."
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode python-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))
;;}}}

;;{{{ similar to dd & yy in VIM
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy the current single line to `kill-ring' instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
                   (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill the current single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
                   (line-beginning-position 2)))))
;;}}}

;;{{{ polish beginning-of-line & end-of-line (`C-a'/`C-e')
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
;;}}}

;;{{{ rebind `C-o', auto open next line and indent as in VIM
(defun vi-open-next-line (arg)
 "Move to the next line (like vi) and then opens a line."
 (interactive "p")
 (end-of-line)
 (open-line arg)
 (next-line 1)
 (indent-according-to-mode))
;;}}}

;;{{{ redo command, bind to `C-''
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
;;}}}

;;{{{ untabify
(defun my-untabify ()
  "My untabify function as discussed and described at
 http://www.jwz.org/doc/tabs-vs-spaces.html
 and improved by Claus Brunzema:
 - return nil to get `write-contents-hooks' to work correctly
   (see documentation there)
 - `make-local-hook' instead of `make-local-variable'
 - when instead of if
 Use some lines along the following for getting this to work in the
 modes you want it to:

 \(add-hook 'some-mode-hook
           '(lambda ()
               (make-local-hook 'write-contents-hooks)
                (add-hook 'write-contents-hooks 'my-untabify nil t)))"
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\t" nil t)
      (untabify (1- (point)) (point-max)))
    nil))
;;}}}

;;{{{ clone-buffer, bind to `C-x c'
(defun ywb-clone-buffer (non-indirect)
  "If with prefix argument, clone buffer, other wise, clone indirect buffer"
  (interactive "P")
  (if non-indirect
      (call-interactively 'clone-buffer)
    (let ((indir-bufs (mapcar (lambda (buf) (cons buf (buffer-base-buffer buf)))
                              (remove-if-not 'buffer-base-buffer (buffer-list))))
          buf)
      (if (setq buf (assoc (current-buffer) indir-bufs))
          (select-window (display-buffer (cdr buf)))
        (if (setq buf (rassoc (current-buffer) indir-bufs))
            (select-window (display-buffer (car buf)))
          (setq current-prefix-arg nil)
          (call-interactively 'clone-indirect-buffer-other-window))))))
;;}}}

;;{{{ camelcase move, rebind `M-f/M-b'
(defun ywb-camelcase-move-word (fw)
  (let ((case-fold-search nil)
        wordpos casepos)
    (save-excursion
      (forward-word fw)
      (setq wordpos (point)))
    (save-excursion
      (and (re-search-forward "\\w[A-Z][a-z]" nil t fw)
           (setq casepos (- (point) (if (> fw 0) 2 -1)))))
    (cond ((and wordpos casepos)
           (goto-char
            (if (< (abs (- casepos (point)))
                   (abs (- wordpos (point))))
                casepos
              wordpos)))
          (wordpos (goto-char wordpos))
          (casepos (goto-char casepos))
          (t (goto-char (if (> fw 0) (point-max) (point-min)))))))

(defun ywb-camelcase-forward-word (arg)
  (interactive "p")
  (let ((fw (signum arg)))
    (dotimes (i (abs arg))
      (ywb-camelcase-move-word fw))))

(defun ywb-camelcase-backward-word (arg)
  (interactive "p")
  (ywb-camelcase-forward-word (- arg)))
;;}}}

;;{{{ sort and uniq region
(defun ywb-uniq-region (beg end)
  (interactive "r")
  (shell-command-on-region beg end "sort | uniq" nil t))
;;}}}

;;{{{ create/switch-scratch, bind to `C-c c c'
(defvar ywb-scratch-buffer "*scratch*")
(defun ywb-create/switch-scratch (arg)
  (interactive "P")
  (when arg
    (setq ywb-scratch-buffer (read-buffer "Set scratch to: " (buffer-name))))
  (let ((buf (get-buffer ywb-scratch-buffer)))
    (if (null buf)
        (progn
          (or arg
              (setq ywb-scratch-buffer (if (y-or-n-p "The buffer no exists! Create *scratch*? ")
                                           "*scratch*"
                                         (read-buffer "Set scratch to: " (buffer-name)))))
          (switch-to-buffer ywb-scratch-buffer)
          (lisp-interaction-mode))
      (switch-to-buffer ywb-scratch-buffer))))
;;}}}

;;{{{ delete char or region, skip kill ring, rebind `C-d'
(defun delete-char-or-region ()
  (interactive)
  (if (and mark-active transient-mark-mode)
      (delete-region (region-beginning) (region-end))
    (call-interactively 'delete-char)))
;;}}}

;;{{{ switch major mode
(defvar switch-major-mode-history nil)
(defun switch-major-mode (mode)
  (interactive
   (list
    (intern
     (completing-read "Switch to mode: "
                      obarray (lambda (s)
                                (and (fboundp s)
                                     (string-match "-mode$" (symbol-name s))))
                      t nil 'switch-major-mode-history))))
  (setq switch-major-mode-history
        (cons (symbol-name major-mode) switch-major-mode-history))
  (funcall mode))
;;;}}}

;;{{{ display current buffer path
(defun my-display-buffer-path (&optional copy)
  "Display the absolute path of current buffer in mini-buffer. If
you call this function by prefix `C-u', the path will be store
into `kill-ring'."
  (interactive
   (list current-prefix-arg))
  (let ((f (buffer-file-name (current-buffer))))
    (if f
        (case copy
          ((nil)
           (message "Buffer path: %s" f))
          (1                                ; store only path
           (let ((d (file-name-directory f)))
             (kill-new d)
             (message "Copy directory: %s" d)))
          (2                                ; store only file name
           (let ((d (file-name-nondirectory f)))
             (kill-new d)
             (message "Copy filename: %s" d)))
          (t                                ; store absolute file path
           (kill-new f)
           (message "Copy path: %s" f))))))
;;}}}

;;{{{ revert buffer
(defun my-revert-buffer ()
  "Revert buffer without prompt."
  (interactive)
  (revert-buffer nil t nil))
;;}}}

;;{{{ toggle sr speedbar window
(defun my-toggle-sr-speedbar ()
  "Toggle sr speedbar."
  (interactive)
  (sr-speedbar-toggle) (sr-speedbar-select-window))
;;}}}

;;{{{ switch other buffer
(defun my-switch-recent-buffer ()
  "Swith to the recent visited buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))
;;}}}

;;{{{ Copy current buffer's full file name
(defun my-copy-full-file-name ()
  "Copy full file name of current-buffer."
  (interactive)
  (let ((file (expand-file-name buffer-file-name)))
    (kill-new file)
    (message "File `%s' copied." file)))
;;}}}

;;{{{ disable indent when paste
(defun ywb-set-paste ()
  "Avoid content indent when paste from clipboard."
  (interactive)
  (fundamental-mode)
  (setq indent-line-function 'ignore))
;;}}}

;;{{{ auto generate autoloads
(defun ywb-generate-loaddefs ()
  (interactive)
  (require 'autoload)
  (with-temp-buffer
    (let (files)
      (setq files (directory-files my-config-dir t "func-.*\\.el$"))
      (dolist (dir '("contrib" "goodies"))
        (setq files (append files
                            (directory-files
                             (expand-file-name dir my-site-lisp-dir)
                             t ".*\\.el$"))))
      (dolist (file files)
        (unless (file-directory-p file)
          (generate-file-autoloads file)))
      (write-region (point-min) (point-max)
                    "~/.emacs.d/config/100-loaddefs.el"))))
;;}}}
