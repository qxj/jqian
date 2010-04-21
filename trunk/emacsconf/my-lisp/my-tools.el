;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; my-tools.el ---
;; Time-stamp: <2010-04-21 19:34:21 Wednesday by jqian>

;;{{{ etags setting
;; (setq tags-file-name "~/projects/TAGS")
(defun my-generate-tag-table ()
  "Generate tag tables under current directory(Linux)."
  (interactive)
  (let ((exp "") (dir ""))
    (setq dir (read-from-minibuffer "generate tags in: " default-directory)
          exp (read-from-minibuffer "suffix: "))
    (with-temp-buffer
      (shell-command
       (concat "find " dir " -name \"" exp "\" | xargs etags ")
       (buffer-name)))))
;;}}}

;;{{{ select a word
(defun my-isearch-word ()
  (interactive)
  (when (not mark-active)
    (let (word-beg word-end)
      (unless (looking-at "\\<")
        (if (eq (char-syntax (char-after)) ?w)
            (backward-word)
          (and (forward-word) (backward-word))))
      (setq word-beg (point))
      (forward-word)
      (setq word-end (point))
      (setq my-isearch-word (filter-buffer-substring word-beg word-end nil t))
      (backward-word))
    (when (> (length my-isearch-word) 0)
      (kill-new my-isearch-word)
      (setq my-isearch-word (concat "\\<" my-isearch-word "\\>"))
      (isearch-update-ring my-isearch-word t)
      (add-hook 'isearch-mode-end-hook 'my-isearch-word-end-hook)
      (isearch-mode t t)
      (isearch-repeat 'forward)
      (message "%s" isearch-string))))
(global-set-key (kbd "C-M-1") 'my-isearch-word)
;;}}}


;;{{{ count Chinese, English words of a region
(defun my-count-ce-word (beg end)
  "Count Chinese and English words in marked region."
  (interactive "r")
  (let* ((cn-word 0)
         (en-word 0)
         (total-word 0)
         (total-byte 0))
    ;; string-to-int
    (setq cn-word (count-matches "\\cc" beg end)
          en-word (count-matches "\\w+\\W" beg end)
          total-word (+ cn-word en-word)
          total-byte (+ cn-word (abs (- beg end))))
    (message (format "Total: %d (cn: %d, en: %d) words, %d bytes."
                     total-word cn-word en-word total-byte))))
;;}}}

;;{{{ insert current time-stamp
(defun my-insert-date ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))
;; (global-set-key (kbd "C-c C-d d") 'my-insert-date)
;;}}}

;;{{{ sdcv
(defun sdcv-ywb (&optional word)
  (interactive)
  (or word (setq word (current-word)))
  (shell-command (format "sdcv -n %s" word)))
(defun sdcv-lookup (word)
  (interactive "sword: ")
  (sdcv-ywb word))

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
(if (fboundp 'tooltip-show)
    (global-set-key (kbd "M-1") 'sdcv-tooltip)
  (global-set-key (kbd "M-1") 'sdcv-ywb))
;;}}}

;;{{{ Copy current buffer's full file name
(defun my-copy-full-file-name ()
  "Copy full file name of current-buffer."
  (interactive)
  (let ((file (expand-file-name buffer-file-name)))
    (kill-new file)
    (message "File `%s' copied." file)))
;;}}}

;;{{{ new indent code method
(defun ywb-indent-accoding-to-paren ()
  "Indent source code by [],{},()."
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char)))
        (pos (point)))
    (save-excursion
      (cond ((string-match "[[{(<]" next-char)
             (indent-region pos (progn (forward-sexp 1) (point)) nil))
            ((string-match "[\]})>]" prev-char)
             (indent-region (progn (backward-sexp 1) (point)) pos nil))))))
(global-set-key (kbd "C-M-]") 'ywb-indent-accoding-to-paren)
;;}}}

;;{{{ show paren
(defun my-show-paren-function ()
  (if show-paren-mode
      (let ((oldpos (point))
            dir
            pos mismatch face)
        (setq dir
              (cond ((eq (syntax-class (syntax-after (1- (point)))) 5) -1)
                    ((eq (syntax-class (syntax-after (point)))      4) 1)))
          ;;
          ;; Find the other end of the sexp.
        (when dir
          (save-excursion
            (save-restriction
              ;; Determine the range within which to look for a match.
              (when blink-matching-paren-distance
                (narrow-to-region
                 (max (point-min) (- (point) blink-matching-paren-distance))
                 (min (point-max) (+ (point) blink-matching-paren-distance))))
              ;; Scan across one sexp within that range.
              ;; Errors or nil mean there is a mismatch.
              (condition-case ()
                  (setq pos (scan-sexps (point) dir))
                (error (setq pos t mismatch t)))
              ;; Move back the other way and verify we get back to the
              ;; starting point.  If not, these two parens don't really match.
              ;; Maybe the one at point is escaped and doesn't really count.
              (when (integerp pos)
                (unless (condition-case ()
                            (eq (point) (scan-sexps pos (- dir)))
                          (error nil))
                  (setq pos nil)))
              ;; If found a "matching" paren, see if it is the right
              ;; kind of paren to match the one we started at.
              (when (integerp pos)
                (let ((beg (min pos oldpos)) (end (max pos oldpos)))
                  (unless (eq (syntax-class (syntax-after beg)) 8)
                    (setq mismatch
                          (not (or (eq (char-before end)
                                       ;; This can give nil.
                                       (cdr (syntax-after beg)))
                                   (eq (char-after beg)
                                       ;; This can give nil.
                                       (cdr (syntax-after (1- end))))
                                   ;; The cdr might hold a new paren-class
                                   ;; info rather than a matching-char info,
                                   ;; in which case the two CDRs should match.
                                   (eq (cdr (syntax-after (1- end)))
                                       (cdr (syntax-after beg))))))))))))
        ;;
        ;; Highlight the other end of the sexp, or unhighlight if none.
        (if (not pos)
            (progn
              ;; If not at a paren that has a match,
              ;; turn off any previous paren highlighting.
              (and show-paren-overlay (overlay-buffer show-paren-overlay)
                   (delete-overlay show-paren-overlay))
              (and show-paren-overlay-1 (overlay-buffer show-paren-overlay-1)
                   (delete-overlay show-paren-overlay-1)))
          ;;
          ;; Use the correct face.
          (if mismatch
              (progn
                (if show-paren-ring-bell-on-mismatch
                    (beep))
                (setq face 'show-paren-mismatch))
            (setq face 'show-paren-match))
          ;;
          ;; If matching backwards, highlight the closeparen
          ;; before point as well as its matching open.
          ;; If matching forward, and the openparen is unbalanced,
          ;; highlight the paren at point to indicate misbalance.
          ;; Otherwise, turn off any such highlighting.
          (if (and (not show-paren-highlight-openparen) (= dir 1) (integerp pos))
              (when (and show-paren-overlay-1
                         (overlay-buffer show-paren-overlay-1))
                (delete-overlay show-paren-overlay-1))
            (let ((from (if (= dir 1)
                            (point)
                          (forward-point -1)))
                  (to (if (= dir 1)
                          (forward-point 1)
                        (point))))
              (if show-paren-overlay-1
                  (move-overlay show-paren-overlay-1 from to (current-buffer))
                (setq show-paren-overlay-1 (make-overlay from to nil t)))
              ;; Always set the overlay face, since it varies.
              (overlay-put show-paren-overlay-1 'priority show-paren-priority)
              (overlay-put show-paren-overlay-1 'face face)))
          ;;
          ;; Turn on highlighting for the matching paren, if found.
          ;; If it's an unmatched paren, turn off any such highlighting.
          (unless (integerp pos)
            (delete-overlay show-paren-overlay))
          (let ((to (if (or (eq show-paren-style 'expression)
                            (and (eq show-paren-style 'mixed)
                                 (not (pos-visible-in-window-p pos))))
                        (point)
                      pos))
                (from (if (or (eq show-paren-style 'expression)
                              (and (eq show-paren-style 'mixed)
                                   (not (pos-visible-in-window-p pos))))
                          pos
                        (save-excursion
                          (goto-char pos)
                          (forward-point (- dir))))))
            (if show-paren-overlay
                (move-overlay show-paren-overlay from to (current-buffer))
              (setq show-paren-overlay (make-overlay from to nil t))))
          ;;
          ;; Always set the overlay face, since it varies.
          (overlay-put show-paren-overlay 'priority show-paren-priority)
          (overlay-put show-paren-overlay 'face face)

          ;;Added by magicring :P
          (if (< pos (point))
              (let ((pos-line-number (line-number-at-pos pos))
                    (number 0))
                (while
                    (and
                     (equal (line-number-at-pos (+ pos number)) pos-line-number)
                     (not (> (+ pos number) (point))))
                  (setq number (+ number 1)))
                (message "%s" (buffer-substring pos (+ pos (- number 1)))))
            (let ((pos-line-number (line-number-at-pos pos))
                  (number 0))
              (while
                  (and
                   (equal (line-number-at-pos (- pos number)) pos-line-number)
                   (not (< (- pos number) (point))))
                  (setq number (+ number 1)))
              (message "%s" (buffer-substring (- pos (- number 1)) pos))))
          ))
    ;; show-paren-mode is nil in this buffer.
    (and show-paren-overlay
         (delete-overlay show-paren-overlay))
    (and show-paren-overlay-1
         (delete-overlay show-paren-overlay-1))))
;;}}}

