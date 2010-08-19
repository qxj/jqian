;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; my functions ;;;;;;;;;;;;;;;;;;;;
;; Most useful function and commands

(dolist (func '("func-dired-ext"
                "func-elisp-helper"
                "func-prog"
                "func-sudo"
                "func-skeleton"))
  (load (expand-file-name func my-config-dir)))



;; hack comment-or-uncomment-region
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

;;{{{ imenu -> ido-completing-read
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
;;}}}

;;{{{ keybind utils
(defun apply-define-key (map key-pairs)
  (dolist (key-pair key-pairs)
    (if key-pair
        (define-key map (eval `(kbd ,(nth 0 key-pair))) (nth 1 key-pair)))))
;;}}}

;; sort line
(defun ywb-sort-lines-1 (reverse beg end predicate)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (sort-subr reverse 'forward-line 'end-of-line nil nil
                 predicate))))

(defalias 'pp* 'cl-prettyprint)

(defsubst join (separator sequence)
  (mapconcat 'identity sequence separator))

(defun ywb-find-top-directory (file &optional dir)
  (or dir (setq dir (expand-file-name default-directory)))
  (let ((thefile (expand-file-name file dir)))
    (if (file-exists-p thefile)
        thefile
      (setq pdir (directory-file-name (file-name-directory dir)))
      (if (string= pdir dir)
          nil
        (ywb-find-top-directory file pdir)))))

;; remove-from-list
(defun ywb-remove-from-list (list key)
  "like add-to-list"
  (set list (remove (assoc key (symbol-value list))
                    (symbol-value list))))

;;{{{ help functions
;; clone-buffer
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

;; browse-url-generic
(defun ywb-toggle-browse-url-program ()
  (interactive)
  (if (string= browse-url-generic-program "opera")
      (setq browse-url-generic-program "w3m"
            browse-url-generic-args '()
            browse-url-browser-function 'w3m-browse-url)
    (setq browse-url-generic-program "opera"
          browse-url-generic-args '("-newpage")
          browse-url-browser-function 'browse-url-generic))
  (message "Will browse url using %s" browse-url-generic-program))

;; camelcase move
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

;; uniq region
(defun ywb-uniq-region (beg end)
  (interactive "r")
  (shell-command-on-region beg end "sort | uniq" nil t))

;; hippie-expand-filename
(defun ywb-hippie-expand-filename ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-complete-file-name try-complete-file-name-partially)))
    (call-interactively 'hippie-expand)))

;; get-column
(defun ywb-get-column (start end)
  (interactive "r")
  (let ((cols (mapcar 'string-to-number
                      (split-string (read-from-minibuffer "cols(seperate by space): "))))
        (standard-output (get-buffer-create "*column*"))
        line)
    (with-current-buffer standard-output
      (erase-buffer))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (setq line (split-string (buffer-substring-no-properties (line-beginning-position)
                                                                 (line-end-position))
                                 "\t"))
        (princ (mapconcat 'identity (mapcar (lambda (c)
                                              (nth (1- c) line))
                                            cols) "\t"))
        (princ "\n")
        (forward-line 1)))
    (pop-to-buffer standard-output)))

;; create/switch-scratch
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

;; mark-sexp
(defun ywb-mark-sexp ()
  (interactive)
  (let ((bound (bounds-of-thing-at-point 'sexp)))
    (if bound
        (progn
          (goto-char (car bound))
          (set-mark (point))
          (goto-char (cdr bound)))
      (message "No sexp found at point!"))))

;; dictionary files
(defun ywb-add-dict-word (word)
  (interactive (list (let (w)
                       (read-from-minibuffer
                        (format "add word%s: "
                                (if (setq w (current-word))
                                    (concat "(default: " w ")")
                                  ""))))))
  (or (not (string= word "")) (setq word (current-word)))
  (let ((mode (symbol-name major-mode)))
    (save-excursion
      (set-buffer (get-buffer-create (format " %s-dict" mode)))
      (goto-char (point-min))
      (if (not (re-search-forward word nil t))
          (progn
            (setq major-mode (intern mode))
            (goto-char (point-min))
            (insert word "\n")
            (with-current-buffer (find-file-noselect ywb-dict-file)
              (goto-char (point-min))
              (if (re-search-forward (concat "\\* " mode) nil t)
                  (progn
                    (forward-line 1)
                    (insert word "\n"))
                (goto-char (point-max))
                (insert "* " mode "\n"
                        word "\n"))
              (save-buffer)
              (kill-buffer (current-buffer)))
            (message "add %s to %s" word mode))
        (message "%s is in the dictionary!" word)))))
(defvar ywb-dict-file "~/.emacs.d/.dict")
(defun ywb-read-dict-file ()
  "Read dictionary file"
  (interactive)
  (if (file-exists-p ywb-dict-file)
      (save-excursion
        (let ((buffer (find-file-noselect ywb-dict-file))
              (done nil)
              mode beg end)
          (set-buffer buffer)
          (goto-char (point-min))
          (re-search-forward "^\\*\\s-*\\(.*-mode\\)" nil t)
          (setq mode (buffer-substring (match-beginning 1)
                                       (match-end 1)))
          (setq beg (1+ (match-end 0)))
          (while (progn
                   (if (re-search-forward "^\\*\\s-*\\(.*-mode\\)" nil t)
                       (setq end (match-beginning 0))
                     (setq end (point-max))
                     (setq done t))
                   ;; (message "mode: %s, beg: %d, end: %d" mode beg end)
                   (with-current-buffer  (get-buffer-create (format " %s-dict" mode))
                     (erase-buffer)
                     (setq major-mode (intern mode))
                     (insert-buffer-substring buffer beg end))
                   (setq mode (match-string 1))
                   (setq beg (1+ (match-end 0)))
                   (not done)))
          (kill-buffer buffer)))
    (message "file %s is not exits!" ywb-dict-file)))
(ywb-read-dict-file)

(defun ywb-change-window-size (n char)
  "Use {}^v to change window size, any key to quit"
  (interactive "p\ncdirect({}^v): ")
  (while (string-match (char-to-string char) "{}^v")
    (cond ((char-equal ?\{ char)
           (enlarge-window-horizontally (- n)))
          ((char-equal ?\} char)
           (enlarge-window-horizontally n))
          ((char-equal ?^ char)
           (enlarge-window (- n)))
          ((char-equal ?v char)
           (enlarge-window n)))
    (setq char (read-char))))
;; favorite-window-config
(defun ywb-favorite-window-config (&optional percent)
  "Split window to proper portion"
  (interactive "P")
  (or percent (setq percent 50.5))
  (setq percent (/ percent 100.0))
  (let (buf)
    (if (> (length (window-list)) 1)
        (setq buf (window-buffer (next-window))))
    (delete-other-windows)
    (let ((maxwidth (window-width)))
      (split-window-horizontally (round (* maxwidth percent))))
    (if buf (save-selected-window
              (pop-to-buffer buf))))
  (call-interactively 'his-transpose-windows))

;; info-elisp
(defun info-elisp ()
  (interactive)
  (info "elisp"))

(defun delete-char-or-region ()
  (interactive)
  (if (and mark-active transient-mark-mode)
      (delete-region (region-beginning) (region-end))
    (call-interactively 'delete-char)))

;; transpose windows
(defun his-transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; switch major mode
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
;; ido-imenu-completion
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
                                    (and name (imenu--in-alist name prepared-index-alist) name)))
    (when (stringp name)
      (setq choice (assoc name prepared-index-alist))
      (if (imenu--subalist-p choice)
          (imenu--completion-buffer (cdr choice) prompt)
        choice))))
;;;}}}


;;{{{ sdcv - tooltip
(defun sdcv-tooltip ()
  (interactive)
  (let ((begin (point-min))
        (end (point-max)))
    (if mark-active
        (setq begin (region-beginning)
              end (region-end))
      (save-excursion
        (backward-word)
        (mark-word)
        (setq begin (region-beginning)
              end (region-end))))
    (message "searching for %s ..." (buffer-substring begin end))
    (tooltip-show (shell-command-to-string
                   (concat "sdcv -n --utf8-output --utf8-input "
                           (buffer-substring begin end))) (not window-system))))
;; (if (fboundp 'tooltip-show)
;;     (global-set-key (kbd "M-1") 'sdcv-tooltip)
;;   (global-set-key (kbd "M-1") 'sdcv-search))
;;}}}

;;{{{ Copy current buffer's full file name
(defun my-copy-full-file-name ()
  "Copy full file name of current-buffer."
  (interactive)
  (let ((file (expand-file-name buffer-file-name)))
    (kill-new file)
    (message "File `%s' copied." file)))
;;}}}

(defun ywb-emacswiki-goto (word)
  (interactive (list (read-from-minibuffer "Goto Emacswiki page: "
                                           (current-word))))
  (browse-url (format "http://www.emacswiki.org/wiki/%s" word)))

(deh-section "function-alias"
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'chr 'char-to-string)
  (defalias 'sc 'smart-compile)
  (defalias 'list-ascii 'ascii-table-show))

(defun ywb-set-paste ()
  (interactive)
  (fundamental-mode)
  (setq indent-line-function 'ignore))

(define-generic-mode vimrc-mode
  '("\"")
  '("set" "syntax")
  nil
  '("\\.vim\\'")
  nil)