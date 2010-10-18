;; table convert
;;;###autoload
(defun ywb-org-table-convert-region (beg end wspace)
  (interactive "r\nP")
  (require 'org)
  (when (= beg (point-min))
    (save-excursion
      (goto-char beg)
      (insert "\n")
      (setq beg (1+ beg))))
  (or (eq major-mode 'org-mode) (org-mode))
  (org-table-convert-region beg end wspace))
;;;###autoload
(defun ywb-org-table-export-here (beg end)
  (interactive "r")
  (require 'org)
  (let ((buf (generate-new-buffer "*temp*"))
        (table (delete-and-extract-region beg end)))
    (with-current-buffer buf
      (insert table)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*|[ \t]*" nil t)
        (replace-match "" t t)
        (end-of-line 1))
      (goto-char (point-min))
      (while (re-search-forward "[ \t]*|[ \t]*$" nil t)
        (replace-match "" t t)
        (goto-char (min (1+ (point)) (point-max))))
      (goto-char (point-min))
      (while (re-search-forward "^-[-+]*$" nil t)
        (replace-match "")
        (if (looking-at "\n")
            (delete-char 1)))
      (goto-char (point-min))
      (while (re-search-forward "[ \t]*|[ \t]*" nil t)
        (replace-match "\t" t t))
      (setq table (buffer-string))
      (kill-buffer buf))
    (insert table)))
;;;###autoload
(defun ywb-org-table-sort-lines (reverse beg end numericp)
  (interactive "P\nr\nsSorting method: [n]=numeric [a]=alpha: ")
  (setq numericp (string-match "[nN]" numericp))
  (org-table-align)
  (save-excursion
    (setq beg (progn (goto-char beg) (line-beginning-position))
          end (progn (goto-char end) (line-end-position))))
  (let ((col (org-table-current-column))
        (cmp (if numericp
                 (lambda (a b) (< (string-to-number a)
                                  (string-to-number b)))
               'string<)))
    (ywb-sort-lines-1 reverse beg end
                      (lambda (pos1 pos2)
                        (let ((dat1 (split-string (buffer-substring-no-properties
                                                   (car pos1) (cdr pos1))
                                                  "\\s-*|\\s-*"))
                              (dat2 (split-string (buffer-substring-no-properties
                                                   (car pos2) (cdr pos2))
                                                  "\\s-*|\\s-*")))
                          (funcall cmp (nth col dat1) (nth col dat2)))))
    (dotimes (i col) (org-table-next-field))))
;;;###autoload
(defun ywb-box-table (beg end)
  (interactive "r")
  (require 'org)
  (let ((tbl (delete-and-extract-region beg end)))
    (insert
     (substring
      (with-temp-buffer
        (org-mode)
        (insert "\n")
        (setq beg (point))
        (insert tbl)
        (setq end (point))
        (ywb-org-table-convert-region beg end nil)
        (beginning-of-line 1) (open-line 1) (insert "|-")
        (org-cycle 1) (end-of-line 1) (insert "\n|-")
        (org-cycle 1) (goto-char (point-max)) (insert "|-")
        (org-cycle 1) (beginning-of-line 1) (kill-line)
        (buffer-string)) 1))))


;;;;;;;;;;;;;;;;;;;;;;;
;; footnote enhancement
;;;;;;;;;;;;;;;;;;;;;;;
(defun ywb-footnote-detect-style (start end)
  "Detect `footnote-style' by counting match regexp of each style in
`footnote-style-alist', and select the style matches most."
  (let ((case-fold-search nil))
    (caar
     (sort
      (mapcar
       (lambda (footnote-style)
         (cons footnote-style
               (count-matches
                (concat "\n"
                        (regexp-quote footnote-start-tag)
                        (Footnote-current-regexp)
                        (regexp-quote footnote-end-tag))
                start end nil)))
       (mapcar 'car footnote-style-alist))
      (lambda (s1 s2) (> (cdr s1) (cdr s2)))))))

;;;###autoload
(defun ywb-footnote-rescan (&optional force)
  "Rescan footnote position in the file."
  (interactive "P")
  (when (or force
            (not (and footnote-text-marker-alist
                      footnote-pointer-marker-alist)))
    (setq footnote-text-marker-alist nil
          footnote-pointer-marker-alist nil)
    (let ((arg 1)                       ; footnote index
          (modified (buffer-modified-p))
          old-point start end match count pointer)
      (save-excursion
        (Footnote-goto-char-point-max)
        (setq end (point))
        (when (re-search-backward (concat "^" footnote-section-tag-regexp) nil t)
          (setq start (point)
                footnote-style (ywb-footnote-detect-style start end))
          (while (and (< (point) end)
                      (re-search-forward (concat "\n\\("
                                                 (regexp-quote footnote-start-tag)
                                                 (Footnote-current-regexp)
                                                 (regexp-quote footnote-end-tag)
                                                 "\\)")
                                         nil t))
            (setq match (match-string 1))
            (forward-line 0)
            (setq old-point (point))
            ;; find footnote position in text, if the index appear
            ;; more than once, select interactively
            (save-excursion
              (goto-char (point-min))
              (setq count 0)
              (while (re-search-forward (regexp-quote match) start t)
                (setq pointer (point)
                      count (1+ count))))
            (cond ((= count 0)
                   (setq footnote-text-marker-alist nil
                         footnote-pointer-marker-alist nil)
                   (error "No footnote found for index %s" match))
                  ((> count 1)
                   (setq pointer (ywb-footnote-select-pointer start match))))
            ;; renumber footnote
            (delete-region (point)
                           (progn (re-search-forward (regexp-quote footnote-end-tag))
                                  (point)))
            (Footnote-insert-numbered-footnote arg nil)
            (Footnote-insert-text-marker arg old-point)
            (goto-char pointer)
            (delete-region (point)
                           (progn (re-search-backward (regexp-quote footnote-start-tag))
                                  (point)))
            (Footnote-insert-pointer-marker arg (point))
            (Footnote-insert-numbered-footnote arg t)
            (setq arg (1+ arg))
            (goto-char old-point)
            (end-of-line))))
      (set-buffer-modified-p modified))))

(defun ywb-footnote-select-pointer (end index)
  "Set message position for INDEX."
  (let ((line (buffer-substring (line-beginning-position)
                                (min (line-end-position)
                                     (+ 30 (line-beginning-position)))))
        (overlay (make-overlay (point-min) (1+ (point-min))))
        point)
    (overlay-put overlay 'face 'match)
    (save-excursion
      (goto-char (point-min))
      (while (and (null point)
                  (re-search-forward (regexp-quote index) end t))
        (move-overlay overlay (match-beginning 0) (match-end 0))
        (if (y-or-n-p (format "Set point of \"%s\" here? " line))
            (setq point (point)))))
    (delete-overlay overlay)
    point))

(add-hook 'footnote-mode-hook 'ywb-footnote-rescan)