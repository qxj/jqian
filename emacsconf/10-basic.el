;; -*- emacs-lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end


(defconst my/ismac (equal system-type 'darwin))
(defconst my/iswin (or (eq system-type 'windows-nt) (eq system-type 'cygwin)))


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
    (load-theme 'zenburn :no-confirm))

  ;;; Font setting
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
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add more directory to environment variable PATH and exec-path
(let (path)
  (mapc (lambda (p)
          (setq p (convert-standard-filename
                   (expand-file-name p)))
          (add-to-list 'exec-path p)
          (add-to-list 'path p t))
        (append
         ;; let your dirs prepend original PATH
         (if (eq system-type 'windows-nt)
             '("d:/programs/emacs/bin" "d:/cygwin/bin" "d:/cygwin/usr/bin"
               "d:/cygwin/usr/local/bin")
           '("~/bin" "~/local/bin" "~/.local/bin"
             "/usr/local/bin" "/usr/local/sbin" "/usr/texbin"))
         (split-string (getenv "PATH") path-separator)))
  (setenv "PATH" (mapconcat 'identity path path-separator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto indent pasted content
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package ivy
;;   :ensure t
;;   :diminish ivy-mode
;;   :bind
;;   (("C-x b" . ivy-switch-buffer)
;;    ("C-c C-r" . ivy-resume)
;;    :map ivy-minibuffer-map
;;    ("C-w" . ivy-backward-kill-word)
;;    ("C-l" . ivy-backward-kill-word)
;;    ("<return>" . ivy-alt-done)
;;    ("C-c o" . ivy-occur))
;;   :config
;;   (ivy-mode 1)

;;   (setq ivy-re-builders-alist
;;         '((ivy-switch-buffer . ivy--regex-plus)
;;           (swiper . ivy--regex-plus)
;;           (t . ivy--regex-fuzzy)))

;;   (ivy-set-actions
;;    t '(("I" insert "insert")))

;;   (use-package swiper
;;     :ensure
;;     :bind
;;     ("C-s" . swiper)
;;     ("C-r" . swiper)
;;     ("C-M-s" . swiper-all))

;;   (use-package counsel
;;     :ensure
;;     :bind
;;     ("M-x" . counsel-M-x)
;;     ("M-y" . counsel-yank-pop)

;;     ("C-x d" . counsel-dired-jump)
;;     ("C-x C-f" . counsel-find-file)

;;     ("C-c i" . counsel-imenu)
;;     ("C-c c o" . counsel-recentf)

;;     ("C-h f" . counsel-describe-function)
;;     ("C-h v" . counsel-describe-variable)
;;     ("C-h l" . counsel-find-library)
;;     ("C-h i" . counsel-info-lookup-symbol)
;;     ("C-h u" . counsel-unicode-char)

;;     ("C-x c g"  . counsel-git)
;;     ("C-x c j"  . counsel-git-grep)
;;     ("C-x c a"  . counsel-ag)
;;     ("C-x c l"  . counsel-locate)

;;     :bind
;;     (:map help-map
;;           ("f" . counsel-describe-function)
;;           ("v" . counsel-describe-variable)
;;           ("l" . counsel-info-lookup-symbol))
;;     :bind
;;     (:map read-expression-map
;;           ("C-r" . counsel-expression-history))

;;     :config
;;     (when (eq 'system-type 'darwin)
;;       (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind))

;;     (setq counsel-find-file-at-point t
;;           ivy-use-virtual-buffers t
;;           ivy-display-style 'fancy
;;           ivy-initial-inputs-alist nil)
;;     )

;;   (use-package counsel-gtags
;;     :ensure t
;;     :defer 3
;;     :if (executable-find "gtags")
;;     :diminish (counsel-gtags-mode . "cG")
;;     :bind (:map counsel-gtags-mode-map
;;                 ("M-." . counsel-gtags-find-definition)
;;                 ("M-," . counsel-gtags-pop-stack)
;;                 ("M-s d" . counsel-gtags-dwim)
;;                 ("M-s r" . counsel-gtags-find-reference)
;;                 ("M-s s" . counsel-gtags-find-symbol))
;;     :config
;;     (add-hook 'c-mode-hook 'counsel-gtags-mode)
;;     (add-hook 'c++-mode-hook 'counsel-gtags-mode)
;;     )

;;   (use-package counsel-projectile
;;     :ensure t
;;     :defer 3
;;     :config
;;     (counsel-projectile-on))

;;   (use-package counsel-dash
;;     :commands counsel-dash
;;     :bind
;;     ("C-x c d" . counsel-dash))
;;   )
