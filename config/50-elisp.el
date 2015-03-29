;; -*- mode: Emacs-Lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end


;;; Session management
(deh-package bookmark
  :defer
  :config
  ;; autosave bookmark into the diskete
  (setq bookmark-default-file (expand-file-name "emacs.bookmark" my-data-dir)
        bookmark-save-flag 1)
  (add-to-list 'bookmark-after-jump-hook 'recenter)
  (deh-add-hook bookmark-bmenu-mode-hook
    (font-lock-add-keywords
     nil
     '(("^\\s-+\\(.*+\\)[ ]\\{2,\\}"
        (1 (let ((file (split-string (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)) " \\{2,\\}")))
             (if (and (not (file-remote-p (nth 2 file)))
                      (file-directory-p (nth 2 file)))
                 font-lock-function-name-face
               nil))))
       ("^>.*" . font-lock-warning-face)
       ("^D.*" . font-lock-type-face)))))

(deh-package desktop
  :config
  (setq desktop-base-file-name (concat "emacs.desktop-" (system-name))
        desktop-path (list my-data-dir)
        desktop-restore-eager 8)        ; firstly restore 8 buffers

  ;;# not to save
  (setq desktop-globals-to-save
        (delq nil (mapcar (lambda (x) (if (memq x '(tags-table-list
                                                    file-name-history)) nil x))
                          desktop-globals-to-save)))
  (setq desktop-buffers-not-to-save
        (concat "\\(" "\\.log\\|\\.diary\\|\\.elc" "\\)$"))
  (dolist (mode '(dired-mode info-lookup-mode fundamental-mode))
    (add-to-list 'desktop-modes-not-to-save mode))

  ;;# to save
  (add-to-list 'desktop-globals-to-save 'kill-ring)
  (if (boundp 'windata-name-winconf)
      (add-to-list 'desktop-globals-to-save 'windata-named-winconf))
  (if (boundp 'smex-history)
      (add-to-list 'desktop-globals-to-save 'smex-history))

  ;; if error occurred, no matter it!
  ;; (condition-case nil
  ;;     (desktop-read)
  ;;   (error nil))
  (unless (emacs-process-duplicated-p)
    (desktop-save-mode 1)
    ;;# persist desktop into file every 10 mins
    (run-with-idle-timer 600 600 'desktop-save-in-desktop-dir))

  ;; desktop-menu.el can store many desktops, it works besides
  ;; desktop.el and its settings don't cofflict with desktop.el, so
  ;; please don't mix up `desktop-base-file-name' and
  ;; `desktop-menu-base-filename'.
  ;;
  (deh-package desktop-menu
    :config
    (setq desktop-menu-directory my-data-dir
          desktop-menu-base-filename (concat "emacs.desktops-" (system-name))
          desktop-menu-list-file "emacs.desktops"
          desktop-menu-clear 'ask)
    ;; customize some standard `desktop' variables
    (setq desktop-load-locked-desktop t)

    ;; desktop-menu-autosave can NOT be customized by setq
    (customize-set-variable 'desktop-menu-autosave 500)

    ;;# save individual bm repository with different desktops
    (deh-after-load "bm"
      (defun bm-repository-file-of-desktop-menu ()
        (let ((current-desktop (cdr desktop-menu--current-desktop)))
          (if current-desktop
              (setq bm-repository-file
                    (concat current-desktop ".bm-repo")))))
      (deh-add-hook 'desktop-after-read-hook
        (if (bm-repository-file-of-desktop-menu) (bm-repository-load)))
      (deh-add-hook 'desktop-save-hook
        (when (bm-repository-file-of-desktop-menu)
          (bm-buffer-save-all)
          (bm-repository-save))))
    ))


(deh-package revive-mode-config
  :config
  ;; (setq revive:configuration-file (expand-file-name "revive.layout" my-data-dir))
  (deh-add-hook kill-emacs-hook 'emacs-save-layout)
  (bind-keys
   :map ctl-x-map
    ("S" . emacs-save-layout)
    ("L" . emacs-load-layout)))

(deh-package session
  :disabled
  :config
  (setq session-save-file (expand-file-name "emacs.session" my-data-dir))
  (setq session-save-file-coding-system 'utf-8-unix)
  (add-to-list 'session-globals-exclude 'org-mark-ring)
  (add-hook 'after-init-hook 'session-initialize))

(deh-package saveplace
  :config
  (setq save-place-file (expand-file-name "emacs.saveplace" my-data-dir))
  (setq-default save-place t))

(deh-package savehist
  :config
  (setq savehist-additional-variables
        '(kill-ring mark-ring global-mark-ring search-ring regexp-search-ring
                    extended-command-history)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "emacs.savehist" my-data-dir))
  (savehist-mode t))

(deh-package bm
  :commands (bm-toggle bm-next bm-previous bm-show bm-show-all bm-toggle-cycle-all-buffers)
  :bind
  ("C-c b b" . bm-toggle)
  ("C-c b n" . bm-next)
  ("C-c b p" . bm-previous)
  ("C-c b s" . bm-show)
  ("C-c b l" . bm-show)
  ("C-c b a" . bm-show-all)
  ("C-c b t" . bm-toggle-cycle-all-buffers)
  :config
  (setq bm-cycle-all-buffers nil
        bm-highlight-style (if window-system
                               'bm-highlight-line-and-fringe
                             'bm-highlight-line)
        bm-restore-repository-on-load t)
  (setq-default bm-buffer-persistence t)

  (setq bm-repository-file
        (expand-file-name "emacs.bm-repository" my-data-dir))

  ;; buffer setting
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)

  ;; for persistent bookmarks
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))

  ;; mouse setting
  (global-set-key [left-margin mouse-2] 'bm-toggle-mouse)
  (global-set-key [left-margin mouse-3] 'bm-next-mouse)

  (bind-keys
   :map bm-show-mode-map
    ("n"  . bm-show-next)
    ("p"  . bm-show-prev)))

(deh-package recent-jump                ; deprecated by back-button
  :disabled)

;; The function ‘back-button-push-mark-local-and-global’ may be useful
;; to call from Lisp. It is essentially a replacement for ‘push-mark’
;; which unconditionally pushes onto the global mark ring, functionality
;; which is not possible using vanilla ‘push-mark’.
(deh-package back-button
  :disabled
  :diminish
  :config
  ;; Global mark-ring:
  ;; C-x C-<SPC>
  ;; C-x C-<left>
  ;; C-x C-<right>
  ;;
  ;; Local mark-ring:
  ;; C-x <SPC>
  ;; C-x <left>
  ;; C-x <right>
  (setq back-button-never-push-mark t)
  (back-button-mode 1))

(deh-package recentf
  :config
  ;; recent finded buffers
  (setq recentf-max-saved-items 1000
        recentf-save-file (expand-file-name "emacs.recentf" my-data-dir)
        recentf-exclude `(,my-data-dir
                          ,tramp-file-name-regexp))
  (recentf-mode t)

  (defun recentf-open-files-compl ()
    "Open files opened recently with `ido-completing-read'."
    (interactive)
    (let (elist)
      (dolist (item recentf-list)
        (let* ((el (if (file-directory-p item)
                       (concat (basename item) "/") ; flag directories with a slash
                     (basename item)))
               (collided-item (cdr (assoc el elist)))
               collided-el)
          ;; distinguish collided file names
          (when collided-item
            (let ((len (directory-depth item))
                  (collided-len (directory-depth collided-item))
                  (slices (split-file-name item))
                  (collided-slices (split-file-name collided-item))
                  (idx 0)
                  found-unique)
              (while (and (not found-unique)
                          (< idx (min len collided-len)))
                (let ((slice (nth idx slices))
                      (collided-slice (nth idx collided-slices)))
                  (if (string= slice collided-slice)
                      (incf idx)
                    (setq found-unique t)
                    (remove-from-list 'elist el)
                    (setq collided-el
                          (if (= idx (1- collided-len))
                              el
                            (format "%s/%s" collided-slice el)))
                    ;; (setq elist (delq (cons el item) elist))
                    (add-to-list 'elist (cons collided-el collided-item))
                    (setq el
                          (if (= idx (1- len))
                              el
                            (format "%s/%s" slice el))))))))
          (unless (zerop (length el))
            (add-to-list 'elist (cons el item)))))
      ;; use `ido-completing-read' instead of `completing-read'
      (find-file (cdr (assoc (ido-completing-read "Open file: "
                                                  (mapcar 'car elist))
                             elist)))))

  (deh-after-load "recentf"
    ;; Also store recent opened directories besides files
    (deh-add-hook dired-mode-hook
      (recentf-add-file dired-directory))))

(deh-package ffap
  :commands (ffap)
  :config
  ;; (ffap-bindings)

  ;; for windows path recognize
  (setq ffap-string-at-point-mode-alist
        '((file "--{}:\\\\$+<>@-Z_a-z~*?\x100-\xffff" "<@" "@>;.,!:")
          (url "--:=&?$+@-Z_a-z~#,%;*" "^A-Za-z0-9" ":;.,!?")
          (nocolon "--9$+<>@-Z_a-z~" "<@" "@>;.,!?")
          (machine "-a-zA-Z0-9." "" ".")
          (math-mode ",-:$+<>@-Z_a-z~`" "<" "@>;.,!?`:")))

  (setq ffap-c-path (append ffap-c-path my-include-dirs)))

(deh-package filecache
  :commands file-cache-minibuffer-complete
  :init
  (bind-keys
   :map minibuffer-local-map
    ("C-M-f" . file-cache-minibuffer-complete))
  :config
  (setq file-cache-ignore-case t)
  (message "Loading file cache...")
  (file-cache-add-directory my-config-dir)
  (file-cache-add-directory-list load-path)
  (file-cache-add-directory-using-find (expand-file-name "~/works")))

;;; Buffer
(deh-package windmove
  :commands windmove-default-keybindings
  :init
  ;; <S-up> windmove-up
  ;; <S-down> windmove-down
  ;; <S-left> windmove-left
  ;; <S-right> windmove-right
  (windmove-default-keybindings 'shift))

(deh-package buffer-move
  :bind
  ("<M-up>" . buf-move-up)
  ("<M-down>" . buf-move-down)
  ("<M-left>" . buf-move-left)
  ("<M-right>" . buf-move-right))

(deh-package help-mode
  :commands (help-go-back help-go-forward)
  :config
  (setq help-window-select t)
  (bind-keys
   :map help-mode-map
    ("<left>"  . help-go-back)
    ("<right>" . help-go-forward) ))

(deh-package view
  :bind
  ("C-c c v" . view-mode)
  :config
  (setq view-read-only t)

  (bind-keys
   :map view-mode-map
    ;; simulate vi keybinds
    ("h"  .  backward-char)
    ("l"  .  forward-char)
    ("j"  .  next-line)
    ("k"  .  previous-line)
    ("c"  .  recenter-top-bottom)
    ("0"  .  beginning-of-line)
    ("$"  .  end-of-line)
    ("g"  .  beginning-of-buffer)
    ("G"  .  end-of-buffer)
    ("n"  .  View-scroll-line-forward)
    ("p"  .  View-scroll-line-backward)
    ("<backspace>" .  View-scroll-page-backward)
    ("SPC"  . View-scroll-page-forward)
    ("?"  . View-search-regexp-backward)
    ;; register
    ("m"  .  point-to-register)
    ("'"  .  register-to-point)
    ;; gtags
    ("."  .  gtags-find-tag)
    (","  .  gtags-pop-stack)
    ("i"  .  gtags-find-tag)
    ("u"  .  gtags-pop-stack)
    ;; sourcepair
    ("a"  .  sourcepair-load)
    ;; eassist
    ("L"  .  eassist-list-methods)
    ;; generic
    ("f"  .  ido-find-file)
    ("d"  .  dired-jump)
    ("o"  .  my-switch-recent-buffer)
    ;; ("q"  'bury-buffer)
    ("q"   . View-quit)
    ("C-k" . kill-this-buffer)))

(deh-package doc-view
  :defer
  :config
  (deh-add-hook doc-view-mode-hook
    (define-key doc-view-mode-map [remap move-beginning-of-line] 'image-bol)
    (define-key doc-view-mode-map [remap move-end-of-line] 'image-eol)))

(deh-package image
  :defer
  :config
  (defun image-display-info ()
    (interactive)
    (let ((image (image-get-display-property))
          info)
      (setq info
            (list
             (cons "File Size"
                   (let ((size (length (plist-get (cdr image) :data))))
                     (if (> size (* 10 1024))
                         (format "%.2f kb" (/ size 1024))
                       size)))
             (cons "Image Type" (plist-get (cdr image) :type))
             (cons "Image Size"
                   (let ((size (image-size image t)))
                     (format "%d x %d pixels" (car size) (cdr size))))))
      (with-current-buffer (get-buffer-create "*image-info*")
        (erase-buffer)
        (dolist (item info)
          (insert (format "%12s: %s\n"
                          (propertize (car item) 'face 'bold)
                          (cdr item))))
        (display-buffer (current-buffer)))))
  (deh-add-hook image-mode-hook
    (define-key image-mode-map "I" 'image-display-info)))

(deh-section w3m
  :defer
  :load-path "~/.emacs.d/site-lisp/w3m"
  :config
  (setq w3m-verbose t                   ; log in *Messages*
        w3m-default-display-inline-images t
        w3m-use-favicon nil
        w3m-search-word-at-point nil
        w3m-use-cookies t
        w3m-cookie-accept-bad-cookies t
        w3m-session-crash-recovery nil)
  (bind-keys
   :map w3m-mode-map
    ("f" . w3m-go-to-linknum)
    ("L" . w3m-lnum-mode)
    ("o" . w3m-previous-anchor)
    ("i" . w3m-next-anchor)
    ("w" . w3m-search-new-session)
    ("p" . w3m-previous-buffer)
    ("n" . w3m-next-buffer)
    ("z" . w3m-delete-buffer)
    ("O" . w3m-goto-new-session-url))
  (defun w3m-go-to-linknum ()
    "Turn on link numbers and ask for one to go to."
    (interactive)
    (let ((active w3m-lnum-mode))
      (when (not active) (w3m-lnum-mode))
      (unwind-protect
          (w3m-move-numbered-anchor (read-number "Anchor number: "))
        (when (not active) (w3m-lnum-mode))))
    (w3m-view-this-url))
  (defun w3m-browse-anchor-external ()
    "Browse one w3m's anchor link with another external browser."
    (interactive)
    (let ((deactivate-mark nil)
          (url (or (w3m-anchor) (w3m-image))))
      (if url (browse-url url)
        (w3m-message "Invalid url."))))

  (define-mode-toggle "w3m" w3m
    (derived-mode-p 'w3m-mode))
  )

;;; Edit
(deh-package inline-string-rectangle
  :bind
  ("C-x r t" . inline-string-rectangle))

(deh-package mark-more-like-this
  :bind
  ("C-<" . mark-previous-like-this)
  ("C->" . mark-next-like-this)
  ("C-M-m" . mark-more-like-this)
  ("C-*" . mark-all-like-this))

(deh-package key-chord
  :disabled
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2
        key-chord-in-macros nil)
  ;; global key chords
  (key-chord-define-global ",." "<>\C-b")
  ;; key chord in c++-mode
  (deh-after-load "cc-mode"
    (key-chord-define c++-mode-map ",," "<<")
    (key-chord-define c++-mode-map ".." ">>"))
  (deh-after-load "python"
    (key-chord-define python-mode-map "''" "\"\"\"\"\"\"\C-b\C-b\C-b"))
  )

(deh-package midnight
  :config
  (setq midnight-mode t
        clean-buffer-list-delay-general 2 ; delete after two days
        ;; clean-buffer-list-kill-never-buffer-names '("*scratch*"
        ;;                                             "*Messages*"
        ;;                                             "*server*")
        clean-buffer-list-kill-never-regexps '("^ \\*Minibuf-.*\\*$"
                                               "^ \\*MULTI-TERM-.*")
        clean-buffer-list-kill-regexps '("^ \\*Customize.*")
        ))

;;; Enhanced terminal
(deh-package multi-term
  :bind
  ("c" . multi-term)
  ("t" . multi-term-dedicated-open-select)
  ("q" . multi-term-dedicated-close)
  ("s" . multi-term-dedicated-select)
  ("g" . multi-term-dedicated-toggle)
  :config
  (setq multi-term-dedicated-window-height 10
        multi-term-dedicated-max-window-height 10)

  ;; compatible with normal terminal keybinds
  (add-to-list 'term-bind-key-alist '("<M-backspace>" . term-send-backward-kill-word))
  (add-to-list 'term-bind-key-alist '("<C-backspace>" . term-send-backward-kill-word))
  (add-to-list 'term-bind-key-alist '("M-DEL" . term-send-backward-kill-word))
  (add-to-list 'term-bind-key-alist '("<backspace>" . term-send-backspace))
  (add-to-list 'term-bind-key-alist '("C-d" . term-send-del))
  (add-to-list 'term-bind-key-alist '("<delete>" . term-send-del))
  (add-to-list 'term-bind-key-alist '("M-d" . term-send-forward-kill-word))
  (add-to-list 'term-bind-key-alist '("<tab>" . (lambda nil (interactive) (term-send-raw-string "\C-i"))))
  ;; some helpful key bindings
  (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))
  (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
  (add-to-list 'term-bind-key-alist '("C-y" . term-paste))
  ;; Only close dedicated window
  (add-to-list 'term-bind-key-alist '("C-q" . multi-term-dedicated-close))
  ;; unbind keys
  (setq term-unbind-key-list (append term-unbind-key-list '("C-v" "M-v")))

  ;; hack to backward kill word as it does in terminal
  (defun term-send-backward-kill-word ()
    "Backward kill word in term mode."
    (interactive)
    (term-send-raw-string "\e\C-?"))

  (defun multi-term-dedicated-open-select ()
    (interactive)
    (unless (multi-term-dedicated-exist-p)
      (multi-term-dedicated-open))
    (multi-term-dedicated-select))

  :init
  (defun my-toggle-multi-term ()
    "Toggle dedicated `multi-term' window and select."
    (interactive)
    (if (multi-term-dedicated-exist-p)
        (multi-term-dedicated-close)
      (multi-term-dedicated-open-select)))
  )

(deh-package shell
  :defer
  :config
  (setenv "HISTFILE" (expand-file-name "shell.history" my-data-dir))

  (deh-add-hook shell-mode-hook
    (rename-buffer (concat "*shell: " default-directory "*") t)
    (ansi-color-for-comint-mode-on)
    (setq-default comint-dynamic-complete-functions
                  (let ((list (default-value 'comint-dynamic-complete-functions)))
                    (add-to-list 'list 'shell-dynamic-complete-command t)))
    ;;# Auto save command history and kill buffers when exit ibuffer.
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (process state)
                            (shell-write-history-on-exit process state)
                            (kill-buffer (process-buffer process)))))

  ;; shell-completion
  (deh-package shell-completion
    :config
    (setq shell-completion-sudo-cmd "\\(?:sudo\\|which\\)")
    (defvar my-lftp-sites (if (file-exists-p "~/.lftp/bookmarks")
                              (shell-completion-get-file-column "~/.lftp/bookmarks" 0 "[ \t]+")))
    (add-to-list 'shell-completion-options-alist
                 '("lftp" my-lftp-sites))
    (add-to-list 'shell-completion-prog-cmdopt-alist
                 '("lftp" ("help" "open" "get" "mirror" "bookmark")
                   ("open" my-lftp-sites)
                   ("bookmark" "add")))))

(deh-package anything                   ; deprecated by helm
  :disabled
  :commands (anything)
  :config
  (bind-keys
   :map anything-map
      ("C-n"  . anything-next-line)
      ("C-p"  . anything-previous-line)
      ("M-n"  . anything-next-source)
      ("M-p"  . anything-previous-source))

  ;; redefine anything-command-map-prefix-key
  (setq anything-command-map-prefix-key "")

  (deh-package anything-config
    :config
    (setq anything-c-adaptive-history-file
          (expand-file-name "anything-c-adaptive-history" my-data-dir)
          anything-c-yaoddmuse-cache-file
          (expand-file-name "yaoddmuse-cache.el" my-data-dir))
    (setq anything-c-find-files-show-icons t
          ;; anything-c-external-programs-associations nil
          anything-c-google-suggest-url "http://www.google.com/complete/search?output=toolbar&q="
          ;; anything-google-suggest-use-curl-p t
          anything-kill-ring-threshold 50
          anything-su-or-sudo "sudo")

    (defun anything-info-pages ()
      "Preconfigured anything for info pages."
      (interactive)
      (anything-other-buffer 'anything-c-source-info-pages "*info pages*"))
    ))

(deh-package helm-config
  :disabled
  :diminish
  :init
  ("<C-return>" . helm-mini)
  :config
  (setq enable-recursive-minibuffers t)
  (helm-mode 1)
  (setq helm-idle-delay 0.1
        helm-input-idle-delay 0.1
        helm-buffer-max-length 50
        helm-M-x-always-save-history t)
  (deh-package helm-ls-git)
  (deh-package helm-gtags
    :config (helm-gtags-mode 1))
  (deh-package helm-descbinds
    :config (helm-descbinds-mode 1))
  (deh-package helm-M-x
    :bind ("C-c M-x" . execute-extended-command))
  (deh-package helm-projectile
    :bind ("C-h h" . helm-projectile)))

;;; Speedbar
(deh-package speedbar
  :defer
  :config
  (setq speedbar-directory-unshown-regexp
        "^\\(CVS\\|RCS\\|SCCS\\|\\.bak\\|\\..*\\)\\'")

  ;; add supported extensions
  (dolist (ext (list ".php" ".js" ".css" ".xml"
                     ".sh"
                     ".txt" ".org" ".md" "README"
                     ".pdf" ".doc"
                     ".jpg" ".png"))
    (speedbar-add-supported-extension ext))
  ;; (setq speedbar-show-unknown-files t)

  (add-to-list 'speedbar-fetch-etags-parse-list
               '("\\.php" . speedbar-parse-c-or-c++tag))

  (bind-keys
   :map speedbar-key-map
    ("j"  . speedbar-next)
    ("k"  . speedbar-prev)
    ("M-u"  . speedbar-up-directory))
  (bind-keys
   :map speedbar-file-key-map
    ("RET" .  speedbar-toggle-line-expansion)) ; SPC

  ;; WORKAROUND: shortkey cofflict, disable view-mode in speedbar
  (setq speedbar-mode-hook '(lambda () (View-exit))))

;; speedbar in one frame
(deh-package sr-speedbar
  :disabled
  :config
  (setq sr-speedbar-skip-other-window-p t
        ;; sr-speedbar-delete-windows t
        sr-speedbar-width-x 22
        sr-speedbar-max-width 30)

  (defun my-toggle-sr-speedbar ()
    "Toggle sr speedbar window."
    (interactive)
    (sr-speedbar-toggle) (sr-speedbar-select-window))
  )

;;; highlight
(deh-package highlight-parentheses
  :disabled
  :diminish
  :init
  (deh-add-hook (emacs-lisp-mode-hook
                 c-mode-common-hook)
    (highlight-parentheses-mode 1))
  :commands highlight-parentheses-mode
  :config
  ;; colors is applied by reversed order
  (setq hl-paren-colors
        '("orange1" "yellow1" "greenyellow" "green1"
          "springgreen1" "cyan1" "slateblue1" "magenta1" "purple")))

(deh-package hl-line
  :config
  ;; (global-hl-line-mode 1)
  (setq hl-line-face 'underline)
  ;; (set-face-background 'hl-line "white smoke") ; list-colors-display
  )

(deh-package highlight-symbol
  :commands (highlight-symbol-mode highlight-symbol-at-point)
  :diminish
  :init
  (when window-system
    (deh-add-hook (emacs-lisp-mode-hook
                   python-mode-hook
                   c-mode-common-hook)
      (highlight-symbol-mode 1)))       ; NOTE: maybe performance issue
  :config
  (setq highlight-symbol-idle-delay 0.5
        highlight-symbol-on-navigation-p t))


(deh-package hi-lock
  :diminish
  :config
  (setq hi-lock-file-patterns-range 5000
        hi-lock-file-patterns-policy '(lambda (dummy) t)))

(deh-package rainbow-mode
  :commands rainbow-mode
  ;; (add-hook 'prog-mode-hook 'rainbow-mode)
  )

;;; Project

;; Currently git, mercurial, darcs and bazaar repos are considered
;; projects by default. So are lein, maven, sbt, rebar and bundler
;; projects. If you want to mark a folder manually as a project just
;; create an empty .projectile file in it.
(deh-package projectile
  :diminish
  :config
  (projectile-global-mode)
  (setq projectile-cache-file
        (expand-file-name "projectile.cache" my-data-dir)
        projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" my-data-dir))
  (setq projectile-switch-project-action 'projectile-dired
        projectile-completion-system 'ido)
  (dolist (dir '(".svn" "CVS" "bin" ".git"))
    (add-to-list 'projectile-globally-ignored-directories dir))
  (dolist (dir '("ede-project.el"))
    (add-to-list 'projectile-project-root-files dir))
  (dolist (file '("GTAGS" "GPATH" "GRTAGS"))
    (add-to-list 'projectile-globally-ignored-files file))

  ;; C-c p f projectile-find-file
  ;; C-c p z projectile-cache-current-file
  ;; C-c p s projectile-switch-project
  ;; C-c p g projectile-grep
  ;; C-c p b projectile-switch-to-buffer
  ;; C-c p o projectile-multi-occur
  ;; C-c p r projectile-replace
  ;; C-c p e projectile-recentf
  ;; C-c p R projectile-regenerate-tags
  ;; C-c p c projectile-compile-project
  )

;; Grizzl is a small utility library to be used in other Elisp code
;; needing fuzzy search behaviour. It is optimized for large data sets,
;; using a special type of lookup table and supporting incremental
;; searches (searches where the result can be narrowed-down by only
;; searching what is already matched).
(deh-package grizzl
  :config
  (setq *grizzl-read-max-results* 30))

;; diff-hl-mode highlights uncommitted changes on the left side of the
;; window, allows you to jump between and revert them selectively.
(deh-package diff-hl
  :config
  (global-diff-hl-mode 1)
  (add-hook 'magit-refresh-file-buffer-hook
            (lambda ()
              (with-current-buffer (current-buffer) (diff-hl-update)))))

(deh-package psvn
  :commands 'svn-status-in-vc-mode?
  :init
  ;; inspired from git-emacs-autoloads
  (defadvice vc-find-file-hook (after svn-status-vc-svn-find-file-hook activate)
    "vc-find-file-hook advice for synchronizing psvn with vc-svn interface"
    (when (svn-status-in-vc-mode?) (svn-status-update-modeline)))
  :config
  (defsubst svn-status-interprete-state-mode-color (stat)
    "Interpret vc-svn-state symbol to mode line color"
    (case stat
      ('up-to-date "GreenYellow")
      ('edited     "tomato")
      ('unknown    "gray")
      ('added      "blue")
      ('deleted    "red")
      ('unmerged   "purple")
      (t           "black")))

  ;; (setq vc-svn-diff-switches nil
  ;;       vc-diff-switches '("--normal" "-bB"))
  )

(deh-package popwin
  :disabled
  :config
  (popwin-mode 1)
  ;;# popwin-mode cofflict with occur-mode, which makes buffers read-only.
  (setq popwin:special-display-config
        (remove-if (lambda (item) (and (listp item) (eq (car item) 'occur-mode)))
                   popwin:special-display-config))
  (define-key global-map (kbd "C-,") popwin:keymap)
  )

;; Crash Course on Emacswiki:
;;
;; - M-x magit-status to see git status, and in the status buffer:
;; - s to stage files
;; - c to commit (type in your commit message then C-c C-c to save the message and commit)
;; - b b to switch to a branch
;;
;; Other handy keys:
;;
;; - P P to do a git push
;; - F F to do a git pull
;;
;; try to press TAB
;;
(use-package magit
  :bind
  ("C-c g"  . magit-status)
  ("C-c l"  . magit-log)
  :config
    ;; Subtler highlight
    (set-face-background 'magit-item-highlight "#121212")
    (set-face-background 'diff-file-header "#121212")
    (set-face-foreground 'diff-context "#666666")
    (set-face-foreground 'diff-added "#00cc33")
    (set-face-foreground 'diff-removed "#ff0000")

    (set-default 'magit-stage-all-confirm nil)
    (set-default 'magit-unstage-all-confirm nil)

    (add-hook 'magit-mode-hook 'magit-load-config-extensions)
    (defun magit-save-and-exit-commit-mode ()
      (interactive)
      (save-buffer)
      (server-edit)
      (delete-window))

    (defun magit-exit-commit-mode ()
      (interactive)
      (kill-buffer)
      (delete-window))

    (eval-after-load "git-commit-mode"
      '(define-key git-commit-mode-map (kbd "C-c C-k") 'magit-exit-commit-mode))

    ;; C-c C-a to amend without any prompt

    (defun magit-just-amend ()
      (interactive)
      (save-window-excursion
        (magit-with-refresh
          (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))

    (eval-after-load "magit"
      '(define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend))

    ;; C-x C-k to kill file on line

    (defun magit-kill-file-on-line ()
      "Show file on current magit line and prompt for deletion."
      (interactive)
      (magit-visit-item)
      (delete-current-buffer-file)
      (magit-refresh))

    (define-key magit-status-mode-map (kbd "C-x C-k") 'magit-kill-file-on-line)

    ;; full screen magit-status

    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))

    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

    ;; full screen vc-annotate

    (defun vc-annotate-quit ()
      "Restores the previous window configuration and kills the vc-annotate buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :vc-annotate-fullscreen))

    (eval-after-load "vc-annotate"
      '(progn
         (defadvice vc-annotate (around fullscreen activate)
           (window-configuration-to-register :vc-annotate-fullscreen)
           ad-do-it
           (delete-other-windows))

         (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

    ;; ignore whitespace

    (defun magit-toggle-whitespace ()
      (interactive)
      (if (member "-w" magit-diff-options)
          (magit-dont-ignore-whitespace)
        (magit-ignore-whitespace)))

    (defun magit-ignore-whitespace ()
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))

    (defun magit-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))

    (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

    ;; Show blame for current line

    (use-package git-messenger
      :bind ("C-x v p" . git-messenger:popup-message))
  )

;;; others

;; ;; erc
;; (deh-package erc
;;   :config
;;   (setq erc-log-channels-directory (expand-file-name "erc" my-data-dir))
;;   (deh-after-load "erc"
;;     (deh-require 'emoticons
;;        (add-hook 'erc-insert-modify-hook 'emoticons-fill-buffer)
;;        (add-hook 'erc-send-modify-hook 'emoticons-fill-buffer)
;;        (add-hook 'erc-mode-hook
;;                  (lambda ()
;;                    (eldoc-mode t)
;;                    (setq eldoc-documentation-function 'emoticons-help-echo))))))

(deh-package epa-file
  :commands epa-file-enable
  :config
  (setq epa-file-cache-passphrase-for-symmetric-encryption t))

(deh-package gmail-notifier
  :config
  ;;# set user/passwd in ~/.authinfo.gpg
  (file-exists-p "~/.authinfo")
  (gmail-notifier-start)
  )
