;;; * patch for some elisp

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

;; (setq woman-rebuild-exclude-path
;;       (remove-if-not (lambda (path) (string-match "^/usr" path))
;;                      woman-manpath))
(eval-after-load "woman"
  '(progn
     (defun ywb-directory-files (dir &optional full match nosort)
       (let ((files
              (split-string
               (shell-command-to-string
                (format "perl -e 'opendir(DIR, $ARGV[0]); print join(qq{\n}, grep {/$ARGV[1]/} readdir(DIR))' '%s' '%s'"
                        dir (or match ".")))
               "\n" t)))
         (if full
             (setq files (mapcar (lambda (f) (expand-file-name f dir)) files)))
         (or nosort
             (setq files (sort files 'string<)))
         files))
     (defun woman-file-name-all-completions (topic)
       "Return an alist of the files in all man directories that match TOPIC."
       ;; Support 3 levels of caching: each element of
       ;; woman-topic-all-completions is a list of one of the forms:
       ;;   (topic)
       ;;   (topic (path-index) (path-index) ... )
       ;;   (topic (path-index filename) (path-index filename) ... )
       ;; where the are no duplicates in the value lists.
       ;; Topic must match first `word' of filename, so ...
       (let ((topic-regexp
              (concat "^" (regexp-quote topic)
                      "(\\..+)*\\.([0-9lmnt]\\w*)(\\.(g?z|bz2))?$"))
             (topics woman-topic-all-completions)
             (path woman-expanded-directory-path)
             dir files)
         (if (cdr (car topics))
             ;; Use cached path-info to locate files for each topic:
             (let ((path-info (cdr (assoc topic topics)))
                   filename)
               (while path-info
                 (setq dir (nth (car (car path-info)) path)
                       filename (car (cdr (car path-info)))
                       path-info (cdr path-info)
                       files (nconc files
                                    ;; Find the actual file name:
                                    (if filename
                                        (list (concat dir "/" filename))
                                      (ywb-directory-files dir t topic-regexp)
                                      )))))
           ;; Search path for the files for each topic:
           (while path
             (setq dir (car path)
                   path (cdr path))
             (if (woman-not-member dir path) ; use each directory only once!
                 (setq files (nconc files
                                    (directory-files dir t topic-regexp))))
             ))
         (mapcar 'list files)
         ))))

'(eval-after-load "filesets"
  '(progn
     (defun filesets-add-buffer (&optional name buffer)
       "Add BUFFER (or current-buffer) to the fileset called NAME.
User will be queried, if no fileset name is provided."
       (interactive)
       (let* ((buffer (or buffer
                          (current-buffer)))
              (file (or (buffer-file-name buffer)
                        (if (eq major-mode 'dired-mode)
                            (buffer-local-value 'default-directory buffer)
                          (error "No file name found for the buffer"))))
              (name   (or name
                          (completing-read
                           (format "Add '%s' to fileset: " buffer)
                           filesets-data nil)))
              (entry  (or (assoc name filesets-data)
                          (when (y-or-n-p
                                 (format "Fileset %s does not exist. Create it? "
                                         name))
                            (progn
                              (add-to-list 'filesets-data (list name '(:files)))
                              (message
                               "Fileset %s created.  Call `M-x filesets-save-config' to save."
                               name)
                              (car filesets-data))))))
         (if entry
             (let* ((files  (filesets-entry-get-files entry))
                    (this   (or (buffer-file-name buffer)
                                (substring (buffer-local-value 'default-directory buffer) 0 -1)))
                    (inlist (filesets-member this files
                                             :test 'filesets-files-equalp)))
               (cond
                (inlist
                 (message "Filesets: '%s' is already in '%s'" this name))
                ((and (equal (filesets-entry-mode entry) ':files)
                      this)
                 (filesets-entry-set-files entry (cons this files) t)
                 (filesets-set-config name 'filesets-data filesets-data))
                (t
                 (message "Filesets: Can't add '%s' to fileset '%s'" this name)))))))
     (defun filesets-open-file ()
       (interactive)
       (let (set file)
         (setq set (completing-read "Open file in set: " filesets-data nil t))
         (setq set
               (delq nil
                     (mapcar (lambda (file)
                               (if (and (vectorp file)
                                        (eq (car (aref file 1)) 'filesets-file-open))
                                   (cons (aref file 0) (aref file 1))))
                             (cdr (cadr (member set filesets-submenus))))))
         (find-file (cadr (nth 2 (assoc-default (ido-completing-read "file: " set) set))))))
     ))

(defadvice occur (around occur-mark-region)
  (save-restriction
    (if (and mark-active transient-mark-mode)
        (narrow-to-region (region-beginning) (region-end)))
    ad-do-it))
(ad-activate 'occur)

;; (defadvice browse-url-generic (before ywb-browse-url-generic)
;;   (setq url (replace-regexp-in-string "\\cC" 'url-hexify-string url)))
;; (ad-activate 'browse-url-generic)

(defadvice browse-url-file-url (after ywb-url-han)
  (let ((file ad-return-value))
    (while (string-match "[\x7f-\xff]" file)
      (let ((enc (format "%%%x" (aref file (match-beginning 0)))))
        (setq file (replace-match enc t t file))))
    (setq ad-return-value file)))
(ad-activate 'browse-url-file-url)

(eval-after-load "shell"
  '(defun shell-cd (dir)
     "Do normal `cd' to DIR, and set `list-buffers-directory'."
     (cd dir)
     (when shell-dirtrackp
       (setq list-buffers-directory default-directory)
       (rename-buffer  (concat "*shell: " default-directory "*") t))))
