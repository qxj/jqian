;;_+ php-mode
(require 'php-mode)

(define-key php-mode-map
  [menu-bar php php-debug]
  '("PHP Debug" . php-debug))

(defun php-debug ()
  (interactive)
  (shell-command
   (concat "D:/WebEngine/php-5.2.1-Win32/php.exe -l \""
           (buffer-file-name)
           "\""))
)

(define-key php-mode-map
  [menu-bar php php-run]
  '("Run PHP" . php-run))

(defun php-run ()
  (interactive)
  (shell-command
   (concat "D:/WebEngine/php-5.2.1-Win32/php.exe -q \""
           (buffer-file-name)
           "\""))
)

(defun my-php-mode()
  ;; ���س�����C-j�Ĺ��ܣ����е�ͬʱ����
  (define-key php-mode-map [return] 'newline-and-indent)
  (define-key php-mode-map [(control c) (r)] 'php-run)
  (define-key php-mode-map [(control c) (d)] 'php-debug)
  (interactive)
  ;; ����php����Ķ�����
  (c-set-style "K&R")
  ;; �Զ�ģʽ���ڴ���ģʽ�µ������{ʱ�����Զ����������õĶ��������
  ;;(c-toggle-auto-state)
  ;; ��ģʽ�£�����Backspaceʱ��ɾ�����Ŀո�
  (c-toggle-hungry-state)
  ;; TAB���Ŀ������Ϊ4
  (setq c-basic-offset 4)
  ;; �ڲ˵��м��뵱ǰBuffer�ĺ�������
  (imenu-add-menubar-index)
  ;; ��״̬������ʾ��ǰ������ĸ��������ڲ�
  (which-function-mode)
  )

(add-hook 'php-mode-hook 'my-php-mode)
