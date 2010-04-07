;; Load CEDET

(setq load-path (append
				 '("~/.emacs.d/site-lisp"
				   "~/.emacs.d/site-lisp/cedet/speedbar"
				   "~/.emacs.d/site-lisp/cedet/common"
				   "~/.emacs.d/site-lisp/cedet/eieio"
				   "~/.emacs.d/site-lisp/cedet/semantic")
				 load-path))

;; Load CEDET
(setq semantic-load-turn-useful-things-on t)
(load-file "~/.emacs.d/site-lisp/cedet/common/cedet.el")

;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
;; (semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as the nascent intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-guady-code-helpers)

;; Semantic search scope
(setq semanticdb-project-roots
	  (list
	   (expand-file-name "/")))

;; semantic cache directory
(setq semanticdb-default-save-directory my-temp-dir)

;; keybind
(global-set-key (kbd "M-?") 'semantic-ia-complete-symbol-menu)

(autoload 'senator-try-expand-semantic "senator")
;; Time in seconds of idle before scheduling events
(setq semantic-idle-scheduler-idle-time 5)
;; Time in seconds of idle before scheduling big work.
(setq semantic-idle-scheduler-work-idle-time 10)
;; Maximum size in bytes of buffers automatically reparsed
(setq semantic-idle-scheduler-max-buffer-size 100000)