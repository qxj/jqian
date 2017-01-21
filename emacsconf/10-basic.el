;; -*- emacs-lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end


(defconst my/ismac (equal system-type 'darwin))
(defconst my/iswin (or (eq system-type 'windows-nt) (eq system-type 'cygwin)))

(let ((en-font (cond (my/iswin "Consolas")
                     (my/ismac "Menlo")
                     (t "DejaVu Sans Mono")))
      (zh-font (cond (my/iswin "Microsoft YaHei")
                     (my/ismac "PingFang SC")
                     (t "WenQuanYi Micro Hei Mono")))
      (pixel-size (if (> (x-display-pixel-width) 1280)
                      ":pixelsize=16" ":pixelsize=12"))
      (frame-width (if (> (x-display-pixel-width) 1280) 90 80)))
  (setq default-frame-alist
        `((width . ,frame-width)
          (font . ,(concat en-font pixel-size)))))

;; If terminal and X is sharing the same emacs server, color-theme
;; will affect terminal display. Below function will resolve this
;; issue.
(defun init-window-frame (&optional frame)
  (and frame (select-frame frame))
  ;; Only enable color theme in window system
  ;; the same color-theme  looks bad in terminal
  (set-variable 'color-theme-is-global nil)

  ;; No scroll bar
  (set-scroll-bar-mode nil)
  ;; No tool bar
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  ;; Transparent frame
  ;; (set-frame-parameter (selected-frame) 'alpha '(95 85))
  ;; (add-to-list 'default-frame-alist '(alpha 95 85))
  ;; For the height, subtract a couple hundred pixels from the
  ;; screen height (for panels, menubars and whatnot), then divide
  ;; by the height of a char to get the height we want
  (add-to-list 'default-frame-alist
               (cons 'height (/ (- (x-display-pixel-height) 100)
                                (frame-char-height)))))

(when window-system
  (use-package fit-frame
    ;; avoid conflicting to winsav.el
    ;; (add-hook 'after-make-frame-functions 'fit-frame)
    )

  (add-hook 'after-make-frame-functions 'init-window-frame)
  (add-hook 'after-init-hook 'init-window-frame)

  ;; (load-theme 'tango-dark :no-confirm)
  (use-package zenburn-theme
    :config
    (load-theme 'zenburn :no-confirm)))


;;;{{{ Auto indent pasted content
(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not
automatically occur. Indent too many content will impact yank
performance!")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after yank-indent-region activate)
      (and (not (ad-get-arg 0))
           (member major-mode
                   '(emacs-lisp-mode
                     ;; python-mode
                     c-mode c++-mode
                     ;; latex-mode
                     ;; js-mode
                     ;; php-mode
                     plain-tex-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (yank-advised-indent-function
              (region-beginning) (region-end)))))))

(defun yank-unindented ()
  (interactive) (yank 1))
  ;;;}}}
