;;; prowj.el --- Manage project commands eaily -*- lexical-binding: t -*-

;; Copyright © 2025 Witness jo <witnessjo@gmail.com>

(require 'cl-lib)
(require 'ansi-color)

(defcustom prowj-use-pty t
  "Use PTY for colored output. Set nil for pipe (faster, no color)."
  :type 'boolean
  :group 'prowj)

(defcustom prowj-exec-command-frame-enable nil
  "Enable to execute command in the new frame."
  :type 'boolean
  :group 'prowj)

(defconst prowj-output-buffer "*prowj-output*")
(defconst prowj-log-frame "prowj-log")
(defconst prowj-compile-buffer "prowj-compile-buffer")
(defconst prowj-root-file ".projectile")

(defvar prowj-prev-dir)
(defvar prowj-prev-cmd)
(defvar prowj-window-buffer-pair nil)

(defvar-local prowj-run-default-dir nil)
(defvar-local prowj-run-command nil)
(defvar-local prowj-subcmds nil)

(put 'prowj-run-default-dir 'safe-local-variable #'stringp)
(put 'prowj-run-command 'safe-local-variable #'stringp)

(setq enable-local-variables t)
(setq enable-local-eval t)

(defun prowj-walkup-and-find-file (FILENAME &optional DIRECTORY)
  "Walkup and find a file.
'FILENAME' is a file name to find
'DIRECTORY' is a base directory name"
  (let ((path-to-find)
         (parent-dir))
    (when (equal DIRECTORY "/")
      (error
        (format
          "Couldn't find the target file (\"%s\") (Last directory was \"/\")"
          FILENAME)))
    (if (equal DIRECTORY nil)
      (setq DIRECTORY default-directory))
    (setq path-to-find (format "%s%s" DIRECTORY FILENAME))
    (if (file-exists-p path-to-find)
      path-to-find
      (progn
        (setq parent-dir
          (file-name-directory (directory-file-name DIRECTORY)))
        (prowj-walkup-and-find-file FILENAME parent-dir)))))

(defun prowj-walkup-and-find-project-root ()
  "Find .projectile and return project-root-path for projectile."
  (interactive)
  (let ((project-path))
    (setq project-path (prowj-walkup-and-find-file prowj-root-file))
    (if (equal project-path nil)
      (progn
        (message "Couldn't find a dot-projectile file.")
        "")
      (file-name-directory (directory-file-name project-path)))))

(defun prowj-make-dot-dir-locals-el ()
  "Make a .dir-locals.el file in the project root."
  (interactive)
  (let ((target-directory)
         (target-path)
         (template))
    (setq target-directory (prowj-walkup-and-find-project-root))
    (unless (file-exists-p target-directory)
      (error "Couldn't find \".projectile\" file"))
    (setq target-path
      (format "%s%s" target-directory ".dir-locals.el"))
    (unless (file-exists-p target-path)
      (setq template
        (concat
          "(\n"
          "(nil . (\n"
          (format "(prowj-run-default-dir . \"%s\")\n" target-directory)
          "(prowj-run-command . \"none\")\n"
          "(eval . (progn\n"
          "(setq prowj-subcmds\n"
          "(list\n"
          (format "(list \"testcmd1\" \"ls\" \"%s\")))))\n" target-directory)
          "))\n"
          ")\n"))
      (write-region template nil target-path))
    (find-file target-path)))

(defun prowj-visit-dot-dir-locals-el ()
  "Visit the .dir-locals.el file in the project root."
  (interactive)
  (let ((target-directory)
         (target-path))
    (setq target-directory (prowj-walkup-and-find-project-root))
    (unless (file-exists-p target-directory)
      (error "Couldn't find \".projectile\" file"))
    (setq target-path
      (format "%s%s" target-directory ".dir-locals.el"))
    (find-file target-path)))

(defun prowj-focus-log-window ()
  "Focus or create a log frame."
  (interactive)
  (condition-case nil
    (select-frame-by-name prowj-log-frame)
    (error
      (make-frame `((name . ,prowj-log-frame)))))
  (select-frame-by-name prowj-log-frame))

(defun prowj-switch-buffer-log-frame ()
  "Switch to another buffer in the log frame."
  (interactive)
  (let ((parent-frame (selected-frame)))
    (call-interactively 'prowj-focus-log-window)
    (ignore-errors
      (call-interactively 'switch-to-buffer))
    (select-frame-set-input-focus parent-frame)))

(defun prowj--setup-process-buffer (buffer-name directory)
  "Setup comint buffer with ANSI color support."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
             (inhibit-modification-hooks t))
        (erase-buffer))
      (unless (eq major-mode 'comint-mode)
        (comint-mode))
      ;; ANSI color 지원
      (ansi-color-for-comint-mode-on)
      ;; 환경변수 & 연결 타입
      (setq-local process-environment
        (cons "TERM=xterm-256color" process-environment))
      (setq-local process-connection-type prowj-use-pty)
      (setq-local default-directory directory)
      ;; 성능: undo 비활성화
      (setq-local buffer-undo-list t)
      (buffer-disable-undo))
    buf))

(defun prowj--make-sentinel ()
  "Return process sentinel."
  (lambda (proc _event)
    (when (memq (process-status proc) '(exit signal))
      (with-current-buffer (process-buffer proc)
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (insert "\n------ DONE ------\n"))
        (compilation-mode)
        (message "Done: %s" (buffer-name))))))

(defun prowj--start-process (buffer-name buffer cmd)
  "Start process."
  (let ((proc
          (if (string-match-p "/ssh:" default-directory)
            (start-file-process-shell-command buffer-name buffer cmd)
            (start-process-shell-command buffer-name buffer cmd))))
    (when proc
      (set-process-filter proc #'comint-output-filter)
      (set-process-sentinel proc (prowj--make-sentinel)))
    proc))

(defun prowj--exec-command (directory cmd &optional new-frame)
  "Execute CMD in DIRECTORY. NEW-FRAME if non-nil."
  (unless (file-directory-p directory)
    (error "Directory not found: %s" directory))
  (when (or (null cmd) (string-empty-p cmd))
    (error "Command is empty"))
  (projectile-save-project-buffers)
  (let* ((parent-frame (selected-frame))
          (short-name (file-name-nondirectory (directory-file-name directory)))
          (buffer-name (format "*prowj:%s:%s*" short-name cmd))
          (existing (get-buffer buffer-name)))
    ;; 기존 프로세스 정리
    (when existing
      (let ((proc (get-buffer-process existing)))
        (when (and proc (process-live-p proc))
          (kill-process proc)))
      (kill-buffer existing))
    ;; 프레임 처리
    (when new-frame
      (prowj-focus-log-window))
    ;; 버퍼 설정 & 실행
    (let ((buf (prowj--setup-process-buffer buffer-name directory)))
      (switch-to-buffer buf)
      (when new-frame (delete-other-windows))
      (prowj--start-process buffer-name buf cmd))
    ;; 포커스 복원
    (when new-frame
      (select-frame-set-input-focus parent-frame))))

(defun prowj-exec-command-window (directory cmd &optional _after-func)
  "Execute a command in the current window."
  (interactive)
  (prowj--exec-command directory cmd nil))

(defun prowj-exec-command-frame (directory cmd &optional _after-func)
  "Execute a command in a new frame."
  (interactive)
  (prowj--exec-command directory cmd t))

(defun prowj-reload-dir-locals ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun prowj-run-project (&optional _num)
  "Run the project command."
  (interactive "p")
  (call-interactively #'prowj-reload-dir-locals)
  (if (and (boundp 'prowj-run-default-dir)
        (boundp 'prowj-run-command))
    (if prowj-exec-command-frame-enable
      (prowj-exec-command-frame prowj-run-default-dir prowj-run-command)
      (prowj-exec-command-window prowj-run-default-dir prowj-run-command))
    (message "prowj-run-default-dir and prowj-run-command were not bound.")))

(defun prowj-run-prev-command ()
  "Run previous project command."
  (interactive)
  (if (boundp 'prowj-prev-dir)
    (if prowj-exec-command-frame-enable
      (prowj-exec-command-frame prowj-prev-dir prowj-prev-cmd)
      (prowj-exec-command-window prowj-prev-dir prowj-prev-cmd))
    (message "No command executed yet.")))

(defun prowj-subcmd-list ()
  "List and execute sub commands."
  (interactive)
  (unless (and (boundp 'prowj-subcmds) (listp prowj-subcmds))
    (user-error "No subcmds defined"))
  (let* ((subcmds-temp
           (mapcar
             (lambda (cmd)
               (cons
                 (format "%s: \"%s\", %s"
                   (nth 0 cmd) (nth 1 cmd) (nth 2 cmd))
                 cmd))
             prowj-subcmds))
          (choice (completing-read "Select command: " subcmds-temp))
          (cmd-cons (cdr (assoc choice subcmds-temp))))
    (setq prowj-prev-dir (nth 2 cmd-cons))
    (setq prowj-prev-cmd (nth 1 cmd-cons))
    (if prowj-exec-command-frame-enable
      (prowj-exec-command-frame (nth 2 cmd-cons) (nth 1 cmd-cons))
      (prowj-exec-command-window (nth 2 cmd-cons) (nth 1 cmd-cons)))))

(global-set-key (kbd "C-c c m") 'prowj-make-dot-dir-locals-el)
(global-set-key (kbd "C-c c v") 'prowj-visit-dot-dir-locals-el)
(global-set-key (kbd "C-c c r") 'prowj-run-project)
(global-set-key (kbd "C-c c l") 'prowj-subcmd-list)
(global-set-key (kbd "C-c c f") 'prowj-focus-log-window)
(global-set-key (kbd "C-c c p") 'prowj-run-prev-command)

(provide 'prowj)
