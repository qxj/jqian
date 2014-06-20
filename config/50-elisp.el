;; -*- mode: Emacs-Lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end


(deh-section "std-lib"
  ;; (filesets-init)
  (require 'generic-x)

  ;; smart mark, useful when edit markuped documents
;  (require 'smart-mark)
  ;; visible-line
  (require 'visible-lines nil t)
  ;; for normal term
  ;; (add-hook 'term-mode-hook 'kill-buffer-when-shell-command-exit)
  ;; .vimrc syntax hightlight
  (require 'vimrc-mode))

(deh-require 'package
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))


;;; Session management
(deh-section-after "bookmark"
  ;; autosave bookmark into the diskete
  (setq bookmark-default-file (expand-file-name "emacs.bookmark" my-data-dir)
        bookmark-save-flag 1)
  (add-to-list 'bookmark-after-jump-hook 'recenter)
  (deh-add-hook 'bookmark-bmenu-mode-hook
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

(deh-require-if 'desktop
  (not (emacs-process-duplicated-p))

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
  (desktop-save-mode 1)

  ;;# persist desktop into file every 10 mins
  (run-with-idle-timer 600 600 'desktop-save-in-desktop-dir)

  ;; desktop-menu.el can store many desktops, it works besides
  ;; desktop.el and its settings don't cofflict with desktop.el, so
  ;; please don't mix up `desktop-base-file-name' and
  ;; `desktop-menu-base-filename'.
  ;;
  (deh-require 'desktop-menu
    (setq desktop-menu-directory my-data-dir
          desktop-menu-base-filename (concat "emacs.desktops-" (system-name))
          desktop-menu-list-file "emacs.desktops"
          desktop-menu-autosave 600     ; auto save every 10mins and when exit
          desktop-menu-clear 'ask)
    ;; customize some standard `desktop' variables
    (setq desktop-load-locked-desktop t)

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


(deh-require 'revive-mode-config
  ;; (setq revive:configuration-file (expand-file-name "revive.layout" my-data-dir))
  (deh-add-hook 'kill-emacs-hook 'emacs-save-layout)
  (deh-define-key ctl-x-map
    ("S" 'emacs-save-layout)
    ("L" 'emacs-load-layout)))

(deh-require-reserved 'session
  (setq session-save-file (expand-file-name "emacs.session" my-data-dir))
  (setq session-save-file-coding-system 'utf-8-unix)
  (add-to-list 'session-globals-exclude 'org-mark-ring)
  (add-hook 'after-init-hook 'session-initialize))

(deh-require 'saveplace
  (setq save-place-file (expand-file-name "emacs.saveplace" my-data-dir))
  (setq-default save-place t))

(deh-require 'savehist
  (setq savehist-additional-variables '(kill-ring mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "emacs.savehist" my-data-dir))
  (savehist-mode t))

(deh-require 'bm
  (setq bm-cycle-all-buffers t
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

  (deh-define-key bm-show-mode-map
    ("n"  'bm-show-next)
    ("p"  'bm-show-prev)))

(deh-require-reserved 'recent-jump)     ; deprecated by back-button

;; The function ‘back-button-push-mark-local-and-global’ may be useful
;; to call from Lisp. It is essentially a replacement for ‘push-mark’
;; which unconditionally pushes onto the global mark ring, functionality
;; which is not possible using vanilla ‘push-mark’.
(deh-require 'back-button
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

(deh-section "recentf"
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
    (deh-add-hook 'dired-mode-hook
      (recentf-add-file dired-directory))))

(deh-section "ffap"
  (autoload 'ffap "ffap" "Alias of find-file-at-point")

  ;; (ffap-bindings)

  ;; for windows path recognize
  (setq ffap-string-at-point-mode-alist
        '((file "--{}:\\\\$+<>@-Z_a-z~*?\x100-\xffff" "<@" "@>;.,!:")
          (url "--:=&?$+@-Z_a-z~#,%;*" "^A-Za-z0-9" ":;.,!?")
          (nocolon "--9$+<>@-Z_a-z~" "<@" "@>;.,!?")
          (machine "-a-zA-Z0-9." "" ".")
          (math-mode ",-:$+<>@-Z_a-z~`" "<" "@>;.,!?`:")))

  (deh-after-load "ffap"
    (setq ffap-c-path (append ffap-c-path my-include-dirs)))
  )

(deh-section "filecache"
  (deh-define-key minibuffer-local-map
    ((kbd "C-M-f")  'file-cache-minibuffer-complete))

  (setq file-cache-ignore-case t)

  (deh-after-load "filecache"
    (message "Loading file cache...")
    (file-cache-add-directory my-config-dir)
    (file-cache-add-directory-list load-path)
    (file-cache-add-directory-using-find (expand-file-name "~/works"))))

;;; Buffer
(deh-section "windmove"
  ;; <S-up> windmove-up
  ;; <S-down> windmove-down
  ;; <S-left> windmove-left
  ;; <S-right> windmove-right
  (windmove-default-keybindings 'shift))

(deh-require 'buffer-move
  (deh-define-key global-map
    ((kbd "<M-up>") 'buf-move-up)
    ((kbd "<M-down>") 'buf-move-down)
    ((kbd "<M-left>") 'buf-move-left)
    ((kbd "<M-right>") 'buf-move-right)))

(deh-section-after "help-mode"
  (setq help-window-select t)
  (deh-define-key help-mode-map
    ((kbd "<left>")  'help-go-back)
    ((kbd "<right>") 'help-go-forward) ))

(deh-section-after "view"
  (setq view-read-only t)

  (deh-define-key view-mode-map
    ;; simulate vi keybinds
    ("h"  'backward-char)
    ("l"  'forward-char)
    ("j"  'next-line)
    ("k"  'previous-line)
    ("c"  'recenter-top-bottom)
    ("0"  'beginning-of-line)
    ("$"  'end-of-line)
    ("g"  'beginning-of-buffer)
    ("G"  'end-of-buffer)
    ("n"  'View-scroll-line-forward)
    ("p"  'View-scroll-line-backward)
    ((kbd "<backspace>")  'View-scroll-page-backward)
    ((kbd "SPC")  'View-scroll-page-forward)
    ("?"  'View-search-regexp-backward)
    ;; register
    ("m"  'point-to-register)
    ("'"  'register-to-point)
    ;; gtags
    ("."  'gtags-find-tag)
    (","  'gtags-pop-stack)
    ("i"  'gtags-find-tag)
    ("u"  'gtags-pop-stack)
    ;; sourcepair
    ("a"  'sourcepair-load)
    ;; eassist
    ("L"  'eassist-list-methods)
    ;; generic
    ("f"  'ido-find-file)
    ("d"  'dired-jump)
    ("o"  'my-switch-recent-buffer)
    ;; ("q"  'bury-buffer)
    ("q"   'View-quit)
    ("\C-k"  'kill-this-buffer)))

(deh-section "doc-view"
  (deh-add-hook 'doc-view-mode-hook
    (define-key doc-view-mode-map [remap move-beginning-of-line] 'image-bol)
    (define-key doc-view-mode-map [remap move-end-of-line] 'image-eol)))

(deh-section "image"
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
    (deh-define-key image-mode-map "I" 'image-display-info)))

(deh-section-after "w3m"
  (setq w3m-verbose t                   ; log in *Messages*
        w3m-default-display-inline-images t
        w3m-use-favicon nil
        w3m-search-word-at-point nil
        w3m-use-cookies t
        w3m-cookie-accept-bad-cookies t
        w3m-session-crash-recovery nil)
  (deh-define-key w3m-mode-map
    ("f" 'w3m-go-to-linknum)
    ("L" 'w3m-lnum-mode)
    ("o" 'w3m-previous-anchor)
    ("i" 'w3m-next-anchor)
    ("w" 'w3m-search-new-session)
    ("p" 'w3m-previous-buffer)
    ("n" 'w3m-next-buffer)
    ("z" 'w3m-delete-buffer)
    ("O" 'w3m-goto-new-session-url))
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
(deh-section "mark-multiple"
  (deh-try-require 'inline-string-rectangle
    (deh-define-key global-map
      ((kbd "C-x r t") 'inline-string-rectangle)))

  (deh-try-require 'mark-more-like-this
    (deh-define-key global-map
      ((kbd "C-<") 'mark-previous-like-this)
      ((kbd "C->") 'mark-next-like-this)
      ((kbd "C-M-m") 'mark-more-like-this)
      ((kbd "C-*") 'mark-all-like-this)))

  (deh-after-load "sgml-mode"
    (deh-try-require 'rename-sgml-tag
      (deh-define-key sgml-mode-map
        ((kbd "C-c C-r") 'rename-sgml-tag))))

  (deh-after-load "js2"
    (deh-try-require 'js2-rename-var
      (deh-define-key js2-mode-map
        ((kbd "C-c C-r") 'js2-rename-var)))))

(deh-require-reserved 'key-chord
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

(deh-require 'midnight
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
(deh-require 'multi-term
  (setq multi-term-dedicated-window-height 10
        multi-term-dedicated-max-window-height 10)

  (deh-after-load "multi-term"
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
      (term-send-raw-string "\e\C-?")))

  (defun multi-term-dedicated-open-select ()
    (interactive)
    (unless (multi-term-dedicated-exist-p)
      (multi-term-dedicated-open))
    (multi-term-dedicated-select))

  (defun my-toggle-multi-term ()
    "Toggle dedicated `multi-term' window and select."
    (interactive)
    (if (multi-term-dedicated-exist-p)
        (multi-term-dedicated-close)
      (multi-term-dedicated-open-select)))
  )

(deh-section "shell"
  (setenv "HISTFILE" (expand-file-name "shell.history" my-data-dir))

  (deh-add-hook 'shell-mode-hook
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
  (deh-try-require 'shell-completion
    (setq shell-completion-sudo-cmd "\\(?:sudo\\|which\\)")
    (defvar my-lftp-sites (if (file-exists-p "~/.lftp/bookmarks")
                              (shell-completion-get-file-column "~/.lftp/bookmarks" 0 "[ \t]+")))
    (add-to-list 'shell-completion-options-alist
                 '("lftp" my-lftp-sites))
    (add-to-list 'shell-completion-prog-cmdopt-alist
                 '("lftp" ("help" "open" "get" "mirror" "bookmark")
                   ("open" my-lftp-sites)
                   ("bookmark" "add")))))

;; fold content
;; (deh-require 'fold
;;   (setq fold-mode-prefix-key "\C-c\C-o")
;;   (setq fold-autoclose-other-folds nil)
;;   (add-hook 'find-file-hook 'fold-find-file-hook t))

;; (deh-section "linum"
;;   (setq linum-format (concat (propertize "%6d " 'face 'default)
;;                              (propertize " " 'face 'fringe)))
;;   (autoload 'linum-mode "linum" "Display line number" t))

(deh-section-reserved "anything"        ; deprecated by helm
  (autoload 'anything "anything" "" t)

  (deh-after-load "anything"
    (deh-define-key anything-map
      ("\C-n"  'anything-next-line)
      ("\C-p"  'anything-previous-line)
      ("\M-n"  'anything-next-source)
      ("\M-p"  'anything-previous-source)))

  ;; redefine anything-command-map-prefix-key
  (setq anything-command-map-prefix-key "")

  (deh-after-load "anything-config"
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

(deh-require-reserved 'helm-config
  (setq enable-recursive-minibuffers t)
  (helm-mode 1)
  (setq helm-idle-delay 0.1
        helm-input-idle-delay 0.1
        helm-buffer-max-length 50
        helm-M-x-always-save-history t)
  (deh-try-require 'helm-ls-git)
  (deh-try-require 'helm-gtags
    (helm-gtags-mode 1))
  (deh-try-require 'helm-descbinds
    (helm-descbinds-mode 1))
  (deh-try-require 'helm-M-x
    ;; (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-c M-x") 'execute-extended-command))
  (deh-try-require 'helm-projectile
    (global-set-key (kbd "C-h h") 'helm-projectile))

  (global-set-key (kbd "<C-return>") 'helm-mini)
  )

;;; Speedbar
(deh-section-after "speedbar"
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

  (deh-define-key speedbar-key-map
    ("j"  'speedbar-next)
    ("k"  'speedbar-prev)
    ("\M-u"  'speedbar-up-directory))
  (deh-define-key speedbar-file-key-map
    ((kbd "RET")  'speedbar-toggle-line-expansion)) ; SPC

  ;; WORKAROUND: shortkey cofflict, disable view-mode in speedbar
  (setq speedbar-mode-hook '(lambda () (View-exit))))

;; speedbar in one frame
(deh-require-reserved 'sr-speedbar
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
(deh-require-reserved 'highlight-parentheses
  ;; colors is applied by reversed order
  (setq hl-paren-colors
        '("orange1" "yellow1" "greenyellow" "green1"
          "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))
  (deh-add-hook '(emacs-lisp-mode-hook
                  c-mode-common-hook)
    (highlight-parentheses-mode 1)))

(deh-section "highlight-line"
  ;; (global-hl-line-mode 1)
  (setq hl-line-face 'underline)
  ;; (set-face-background 'hl-line "white smoke") ; list-colors-display
  )

(deh-section "highlight-symbol"
  (autoload 'highlight-symbol-mode "highlight-symbol" "hl-s" t)
  (autoload 'highlight-symbol-at-point "highlight-symbol" "hl-s" t)

  (setq highlight-symbol-idle-delay 0.5
        highlight-symbol-on-navigation-p t)
  (when window-system
    (deh-add-hook '(emacs-lisp-mode-hook
                    python-mode-hook
                    c-mode-common-hook)
      (highlight-symbol-mode 1)))       ; NOTE: maybe performance issue
  )


(deh-section "hi-lock"
  (setq hi-lock-file-patterns-range 5000
        hi-lock-file-patterns-policy '(lambda (dummy) t)))

(deh-section "rainbow-mode"
  (autoload 'rainbow-mode "rainbow-mode" "Background colors" t)
  ;; (add-hook 'prog-mode-hook 'rainbow-mode)
  )

;;; Project

;; Currently git, mercurial, darcs and bazaar repos are considered
;; projects by default. So are lein, maven, sbt, rebar and bundler
;; projects. If you want to mark a folder manually as a project just
;; create an empty .projectile file in it.
(deh-require 'projectile
  (projectile-global-mode)
  (setq projectile-cache-file
        (expand-file-name "projectile.cache" my-data-dir)
        projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" my-data-dir))
  (setq projectile-switch-project-action 'projectile-dired
        projectile-completion-system 'ido)
  (dolist (dir '(".svn" "CVS" "bin"))
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
(deh-require 'grizzl
  (setq *grizzl-read-max-results* 30)
  )

;; diff-hl-mode highlights uncommitted changes on the left side of the
;; window, allows you to jump between and revert them selectively.
(deh-require 'diff-hl
  (global-diff-hl-mode 1)
  (add-hook 'magit-refresh-file-buffer-hook
            (lambda ()
              (with-current-buffer (current-buffer) (diff-hl-update)))))

(deh-section "svn"
  (autoload 'svn-status-in-vc-mode? "psvn" "Is vc-svn active?")

  ;; inspired from git-emacs-autoloads
  (defadvice vc-find-file-hook (after svn-status-vc-svn-find-file-hook activate)
    "vc-find-file-hook advice for synchronizing psvn with vc-svn interface"
    (when (svn-status-in-vc-mode?) (svn-status-update-modeline)))

  (deh-after-load "psvn"
    (defsubst svn-status-interprete-state-mode-color (stat)
      "Interpret vc-svn-state symbol to mode line color"
      (case stat
        ('up-to-date "GreenYellow")
        ('edited     "tomato")
        ('unknown    "gray")
        ('added      "blue")
        ('deleted    "red")
        ('unmerged   "purple")
        (t           "black"))))

  ;; (setq vc-svn-diff-switches nil
  ;;       vc-diff-switches '("--normal" "-bB"))
  )
  
(deh-require 'popwin
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
(deh-require 'magit
  ;; (global-set-key (kbd "C-c g") 'magit-status)
  ;; (global-set-key (kbd "C-c l") 'magit-log)
  )

;;; others

(deh-require 'diminish
  (diminish 'abbrev-mode "Abv")
  (deh-after-load "undo-tree" (diminish 'undo-tree-mode))
  (deh-after-load "back-button" (diminish 'back-button-mode))
  (deh-after-load "projectile" (diminish 'projectile-mode))
  (deh-after-load "helm-mode" (diminish 'helm-mode))
  (deh-after-load "highlight-symbol" (diminish 'highlight-symbol-mode))
  (deh-after-load "outline" (diminish 'outline-minor-mode "ol"))
  (deh-after-load "highlight-parentheses" (diminish 'highlight-parentheses-mode))
  (deh-after-load "hi-lock" (diminish 'hi-lock-mode))
  (deh-after-load "eldoc" (diminish 'eldoc-mode)))

;; ;; erc
;; (deh-section "erc"
;;   (setq erc-log-channels-directory (expand-file-name "erc" my-data-dir))
;;   (deh-after-load "erc"
;;     (deh-require 'emoticons
;;        (add-hook 'erc-insert-modify-hook 'emoticons-fill-buffer)
;;        (add-hook 'erc-send-modify-hook 'emoticons-fill-buffer)
;;        (add-hook 'erc-mode-hook
;;                  (lambda ()
;;                    (eldoc-mode t)
;;                    (setq eldoc-documentation-function 'emoticons-help-echo))))))

(deh-section-after "epa-file"
  (setq epa-file-cache-passphrase-for-symmetric-encryption t))

(deh-require-reserved 'gmail-notifier
  ;;# set user/passwd in ~/.authinfo.gpg
  (file-exists-p "~/.authinfo")
  (gmail-notifier-start)
  )
