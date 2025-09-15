;;; prowj.el --- Manage project commands eaily -*- lexical-binding: t -*-

;; Copyright © 2025 Witness jo <witnessjo@gmail.com>

(require 'cl-lib)

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
  "Walkup and find a file .
'FILENAME' is a file name to fine
'DIRECTORY' is a base directory name'"
  (let ((path-to-find)
         (parent-dir))

    (when (equal DIRECTORY "/")
      (error
        (format
          "Coudlnt find the target file (\"%s\") (Last directory was \"/\")"
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
  (interactive)
  "Find .projectile and return project-root-path for projectile."
  (let ((project-path))
    (setq project-path (prowj-walkup-and-find-file prowj-root-file))
    (if (equal project-path nil)
      (progn
        (message "Couldnt find a dot-projectile file.")
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
      (error "Couldnt find \".projectile\" file"))

    (setq target-path
      (format "%s%s" target-directory ".dir-locals.el"))
    (unless (file-exists-p target-path)
      (setq template
        (concat
          "(\n"
          (concat "(nil . (\n")
          (format "(prowj-run-default-dir . \"%s\")\n"
            target-directory)
          (format "(prowj-run-command . \"none\")\n")
          "(eval . (progn\n"
          "(setq prowj-subcmds\n"
          "(list\n"
          (format "(list \"testcmd1\" \"ls\" \"%s\")))))\n"
            target-directory)
          ")\n"
          ")\n"
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
      (error "Couldnt find \".projectile\" file"))
    (setq target-path
      (format "%s%s" target-directory ".dir-locals.el"))
    (find-file target-path)))

(defun prowj-focus-log-window ()
  "Focus or create a log frame."
  (interactive)
  (condition-case err
    (select-frame-by-name prowj-log-frame)
    (error
      (progn
        (make-frame `((name . "prowj-log"))))))
  (select-frame-by-name prowj-log-frame))

(defun prowj-switch-buffer-log-frame ()
  "Switch to another buffer in the log frame."
  (interactive)
  (let ((parent-frame (selected-frame)))
    (call-interactively 'prowj-focus-log-window)
    (ignore-errors
      (call-interactively 'switch-to-buffer))
    (select-frame-set-input-focus parent-frame)))

(defun prowj-exec-command-window (directory cmd &optional after-func)
  "Execute a command in a comint buffer in the current window."
  (interactive)
  (let ((proc)
         (buffer-name (format "*wj/command-%s-%s*" directory cmd)))
    (projectile-save-project-buffers)
    (unless (file-directory-p directory)
      (error (format "Couldnt find the directory %s\"" directory)))

    (when (or (equal cmd nil) (string= cmd ""))
      (error (format "Couldnt find  %s\"" cmd)))

    (sleep-for 0.5)
    (when (get-buffer buffer-name)
      ;; remove buffer window
      (kill-buffer buffer-name))

    (with-current-buffer (get-buffer-create buffer-name)
      (comint-mode)
      (setq-local process-connection-type nil)
      (setq default-directory directory)

      (switch-to-buffer (current-buffer))

      ;; check tramp buffer
      (if (string-match-p "/ssh:" default-directory)
        (setq proc
          (progn
            (start-file-process-shell-command
              buffer-name (current-buffer) cmd)))
        (setq proc
          (progn
            (start-process-shell-command
              buffer-name (current-buffer) cmd))))
      (when (not (null proc))
        (set-process-filter proc 'comint-output-filter)
        (set-process-sentinel
          proc
          (lambda (p e)
            (with-current-buffer (get-buffer (process-buffer p))
              ;; After process done
              (compilation-mode)
              (compilation-shell-minor-mode)
              (message "Done!!"))))))))

(defun prowj-exec-command-frame (directory cmd &optional after-func)
  "Execute a command in a comint buffer in a new frame."
  (interactive)
  (let ((proc)
         (parent-frame (selected-frame))
         (default-directory
           (if (not (file-directory-p directory))
             (error "The directory was not existing")
             directory))
         (buffer-name (format "*wj/command-%s-%s*" directory cmd)))
    (projectile-save-project-buffers)
    (unless (file-directory-p directory)
      (error (format "Couldnt find the directory %s\"" directory)))

    (when (or (equal cmd nil) (string= cmd ""))
      (error (format "Couldnt find  %s\"" cmd)))

    (prowj-focus-log-window)
    (sleep-for 0.5)

    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (with-current-buffer (get-buffer-create buffer-name)
      (comint-mode)
      (setq-local process-connection-type nil)
      (setq default-directory directory)

      (switch-to-buffer (current-buffer))
      (delete-other-windows)

      ;; check tramp buffer
      (if (string-match-p "/ssh:" default-directory)
        (setq proc
          (progn
            (start-file-process-shell-command
              buffer-name (current-buffer) cmd)))
        (setq proc
          (progn
            (start-process-shell-command
              buffer-name (current-buffer) cmd))))

      (when (not (null proc))
        (set-process-filter proc 'comint-output-filter)
        (set-process-sentinel
          proc
          (lambda (p e)
            (with-current-buffer (get-buffer (process-buffer p))
              (compilation-mode)
              (compilation-shell-minor-mode)
              (message "Done!!"))))))

    ;; After process done
    (select-frame-set-input-focus parent-frame)
    (make-frame-visible parent-frame)))

(defun prowj-run-project (&optional num)
  (interactive "p")
  ;; (print num)
  (wj/common-reload-dir-locals)
  (cond
    ((equal num 1)
      (if (and (boundp 'prowj-run-default-dir)
            (boundp 'prowj-run-command))
        (if prowj-exec-command-frame-enable
          (prowj-exec-command-frame
            prowj-run-default-dir prowj-run-command)
          (prowj-exec-command-window
            prowj-run-default-dir prowj-run-command))
        (message
          "\"projectile-project-root\" and \"prowj-run-command were not binded.")))))


(defun prowj-run-prev-command ()
  "Run previous project command."
  (interactive)
  (if (boundp 'prowj-prev-dir)
    (if prowj-exec-command-frame-enable
      (prowj-exec-command-frame prowj-prev-dir prowj-prev-cmd)
      (prowj-exec-command-window prowj-prev-dir prowj-prev-cmd))
    (message "not executed any command yet!!!")))


(defun prowj-subcmd-list ()
  (interactive)
  (let ((subcmds-temp)
         (choice)
         (cmd-cons))
    (when (not (boundp 'prowj-subcmds))
      nil)
    (when (not (listp 'prowj-subcmds))
      nil)
    (setq subcmds-temp
      (mapcar
        (lambda (cmd)
          (cons
            (format "%s: \"%s\", %s"
              (nth 0 cmd)
              (nth 1 cmd)
              (nth 2 cmd))
            cmd))
        prowj-subcmds))

    (setq choice (completing-read "Select command: " subcmds-temp))

    ;; exac choice commands
    (setq cmd-cons (cdr (assoc choice subcmds-temp)))
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


(provide 'prowj)
