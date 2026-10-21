;;; prowj.el --- Manage project commands and bounded run logs -*- lexical-binding: t -*-

;; Copyright © 2025 Witness jo <witnessjo@gmail.com>
;; Author: Witness jo <witnessjo@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (projectile "2.8.0"))
;; Keywords: tools, processes
;; URL: https://github.com/witnessJo/prowj

;;; Commentary:
;; Run project commands with colored, bounded log previews and complete log files.
;; Required external packages are installed by package.el/package-vc.

;;; Code:

(require 'cl-lib)
(require 'projectile)

(defgroup prowj nil
  "Project command execution."
  :group 'tools)

(require 'prowj-runner)

(defcustom prowj-use-pty t
  "Legacy setting, ignored by the non-interactive project runner."
  :type 'boolean
  :group 'prowj)

(make-obsolete-variable 'prowj-use-pty
                        "Runs now use a pipe; use a terminal for interactive commands."
                        "0.2.0")

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

(defun prowj--log-frame (&optional create)
  "Return the log frame, or nil when there is none.
With CREATE non-nil, make the frame when it is missing.  Unlike
`prowj-focus-log-window' the input focus is left on the frame that is
selected right now."
  (or (cl-find-if
        (lambda (frame)
          (equal (frame-parameter frame 'name) prowj-log-frame))
        (frame-list))
    (when create
      (let* ((parent-frame (selected-frame))
              (frame (make-frame `((name . ,prowj-log-frame)))))
        (select-frame-set-input-focus parent-frame)
        frame))))

(defun prowj-switch-buffer-log-frame-from-parent-frame ()
  "Switch to another buffer in the log frame from parent frame.
Unlike `prowj-switch-buffer-log-frame', the log frame is never
selected: the buffer name is read in the minibuffer of the current
frame and only the log frame's window is redisplayed, so the point
never leaves the buffer being worked on.  The log frame is created
when it does not exist yet."
  (interactive)
  (let* ((frame (prowj--log-frame))
          (buffer (read-buffer "Switch to buffer in log frame: "
                    (other-buffer (current-buffer))))
          (window (frame-selected-window (or frame (prowj--log-frame t)))))
    (set-window-buffer window (get-buffer-create buffer))
    (make-frame-visible (window-frame window))
    (message "Log frame: %s" buffer)))

(defun prowj--exec-command (directory cmd &optional new-frame)
  "Execute non-interactive CMD in DIRECTORY, optionally in the log frame."
  (prowj-runner-exec-command directory cmd new-frame))

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

(defun prowj-reload-dir-locals-all ()
  "Reload dir locals for all buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (prowj-reload-dir-locals))))

;;;###autoload
(defun prowj-run-prev-command ()
  "Run previous project command."
  (interactive)
  (if (boundp 'prowj-prev-dir)
    (if prowj-exec-command-frame-enable
      (prowj-exec-command-frame prowj-prev-dir prowj-prev-cmd)
      (prowj-exec-command-window prowj-prev-dir prowj-prev-cmd))
    (message "No command executed yet.")))

;;;###autoload
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

(defvar prowj-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c m") #'prowj-make-dot-dir-locals-el)
    (define-key map (kbd "C-c c v") #'prowj-visit-dot-dir-locals-el)
    (define-key map (kbd "C-c c l") #'prowj-subcmd-list)
    (define-key map (kbd "C-c c f") #'prowj-focus-log-window)
    (define-key map (kbd "C-c c p") #'prowj-run-prev-command)
    map)
  "Global keymap enabled by `prowj-mode'.")

;;;###autoload
(define-minor-mode prowj-mode
  "Enable Prowj's project command key bindings globally.
Disabling this mode restores access to underlying bindings.  Prowj
commands and running processes remain available independently of this mode."
  :global t
  :lighter nil
  :keymap prowj-mode-map)

(provide 'prowj)

;;; prowj.el ends here
