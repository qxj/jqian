;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-02-02 17:21:44 Tuesday by julian>

(defun get-lines-4-mode-line ()
  (let ((lines (count-lines (point-min) (point-max))))
    (concat (propertize
             (concat "%l:" (format "%dL" lines))
             'mouse-face 'mode-line-highlight
             ;; make it colorful
             ;; 'face 'mode-line-lines-face
             'help-echo (format "%d lines" lines)) " ")))

(defun get-size-indication-format ()
  (if (and transient-mark-mode mark-active)
      (format "%d chars" (abs (- (mark t) (point))))
    "%I"))

(defun get-mode-line-region-face ()
  (and transient-mark-mode mark-active
       (if window-system 'region 'region-invert)))

(size-indication-mode 1)
(setq-default mode-line-buffer-identification (propertized-buffer-identification "%b"))

(if is-after-emacs-23
    (setq-default
     mode-line-position
     `((:eval (get-lines-4-mode-line))
       (:propertize
        ;; "%p " ;; no need to indicate this position
        'local-map mode-line-column-line-number-mode-map
        'mouse-face 'mode-line-highlight
        'help-echo "Size indication mode\n\
mouse-1: Display Line and Column Mode Menu")
       ;; caculate word numbers of selected region. Otherwise, indicate all word number of this buffer, if no region selected.
       (size-indication-mode
         (:eval
          (propertize (get-size-indication-format)
           'face (and transient-mark-mode mark-active (get-mode-line-region-face))
           'local-map mode-line-column-line-number-mode-map
           'mouse-face 'mode-line-highlight
           'help-echo "Buffer position, mouse-1: Line/col menu")))))
  (let* ((help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
    (setq-default
     mode-line-position
     `((:eval (get-lines-4-mode-line))
       (:propertize 'help-echo ,help-echo)
       (size-indication-mode
        (:eval (propertize
                     (get-size-indication-format) 'help-echo ,help-echo
                     'face (and transient-mark-mode mark-active (get-mode-line-region-face)))))))))

(let* ((help-echo
        "mouse-1: Select (drag to resize)\n\
mouse-2: Make current window occupy the whole frame\n\
mouse-3: Remove current window from display")
       (recursive-edit-help-echo "Recursive edit, type C-M-c to get out")
       (standard-mode-line-modes
        (list
         " "
         (propertize "%[" 'help-echo recursive-edit-help-echo)
         (propertize "(" 'help-echo help-echo)
         `(:propertize ("" mode-name)
                       help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                       mouse-face mode-line-highlight
                       local-map ,mode-line-major-mode-keymap)
         '("" mode-line-process)
         `(:propertize ("" minor-mode-alist)
                       mouse-face mode-line-highlight
                       help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
                       local-map ,mode-line-minor-mode-keymap)
         (propertize "%n" 'help-echo "mouse-2: Remove narrowing from the current buffer"
                     'mouse-face 'mode-line-highlight
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 #'mode-line-widen))
         (propertize ")" 'help-echo help-echo)
         (propertize "%]" 'help-echo recursive-edit-help-echo))))
  (setq-default mode-line-modes standard-mode-line-modes)
  (setq-default mode-line-format
                `("%e%t"
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  " "
                  mode-line-buffer-identification
                  ,(propertize " " 'help-echo help-echo)
                  mode-line-position
                  (which-func-mode (" " which-func-format))
                  (vc-mode vc-mode)
                  mode-line-modes
                  (working-mode-line-message (" " working-mode-line-message))
                  ,(propertize "-%-" 'help-echo help-echo))))

(setq mode-line-format-bak mode-line-format)
(setq mode-line t)
(defun toggle-mode-line ()
  "Toggle mode-line."
  (interactive)
  (if mode-line
      (setq-default mode-line-format nil)
    (setq-default mode-line-format mode-line-format-bak))
  (setq mode-line (not mode-line)))

;; 在标题栏显示登陆名称和文件名
(setq frame-title-format
      '((:eval
         (let ((login-name (getenv-internal "LOGNAME")))
           (if login-name (concat login-name "@") "")))
        (:eval (system-name))
        ":"
        (:eval (or (buffer-file-name) (buffer-name)))))

;; (setq frame-title-format "%b/%n%F")
;; (setq-default icon-title-format "Emacs - %b")

;;{{{ I'ld like to see a host indication in the mode line when I'm remote
;; (defconst my-mode-line-buffer-identification
;;   (list
;;    '(:eval
;;      (let ((host-name
;;             (if (file-remote-p default-directory)
;;           		(tramp-file-name-host
;;           		 (tramp-dissect-file-name default-directory))
;;               (system-name))))
;;        (if (string-match "^[^0-9][^.]*\\(\\..*\\)" host-name)
;;            (substring host-name 0 (match-beginning 1))
;;          host-name)))
;;    ": %12b"))
          
;; (setq-default
;;  mode-line-buffer-identification
;;  my-mode-line-buffer-identification)
          
;; (add-hook
;;  'dired-mode-hook
;;  '(lambda ()
;;     (setq
;;      mode-line-buffer-identification
;;      my-mode-line-buffer-identification)))
;;}}}
