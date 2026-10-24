;;; prowj-runner.el --- Bounded project run logs -*- lexical-binding: t -*-

(require 'ansi-color)
(require 'ansi-osc)
(require 'button)
(require 'cl-lib)
(require 'comint)
(require 'subr-x)

(defgroup prowj-runner nil
  "Project commands with bounded, readable output."
  :group 'prowj)

(defcustom prowj-runner-buffer-limit 1000000
  "Maximum characters retained in a run buffer, excluding its finish message.
The complete text output is saved separately.  Trimming keeps approximately
three quarters of this limit to avoid deleting text on every update."
  :type 'natnum)

(defcustom prowj-runner-line-limit 2000
  "Characters of each output line to retain in the preview.
The rest remains available in the complete log via a clickable button."
  :type 'natnum)

(defcustom prowj-runner-log-directory
  (expand-file-name "prowj/logs/"
                    (or (getenv "XDG_STATE_HOME")
                        (expand-file-name "~/.local/state/")))
  "Local directory holding complete, ANSI-free run logs."
  :type 'directory)

(defcustom prowj-runner-highlight-words
  '("ERROR" "ERR" "WARNING" "WARN" "CRITICAL" "FATAL" "PANIC"
    "FAILED" "FAILURE" "Traceback" "Exception" "TIMEOUT" "DENIED"
    "REFUSED" "UNAUTHORIZED" "FORBIDDEN" "UNREACHABLE" "INVALID"
    "ABORTED" "CANCELLED" "CANCELED" "DEPRECATED" "RETRY" "RETRYING"
    "SUCCESS" "SUCCEEDED" "PASSED" "READY" "COMPLETED" "CONNECTED"
    "DISCONNECTED" "STARTED" "STOPPED" "OOM" "ENOENT" "EACCES"
    "ECONNREFUSED" "ETIMEDOUT")
  "Log words to emphasize case-insensitively, preserving their ANSI colors."
  :type '(repeat string))

(defvar ghostel-color-palette)
(defvar ghostel-bold-color)

(defvar-local prowj-runner-log-file nil)
(defvar-local prowj-runner-log-error nil)
(defvar-local prowj-runner-trimmed-characters 0)
(defvar-local prowj-runner-line-column 0)
(defvar-local prowj-runner-line-elided nil)
(defvar-local prowj-runner-output-line 1)

(declare-function prowj-focus-log-window "prowj")
(declare-function prowj-reload-dir-locals "prowj")
(declare-function projectile-save-project-buffers "projectile")

(defface prowj-runner-error
  '((t (:weight bold :underline t)))
  "Error and fatal log levels.")
(defface prowj-runner-warning
  '((t (:weight bold :underline t)))
  "Warning log levels.")
(defface prowj-runner-info
  '((t (:weight bold)))
  "Informational log levels.")
(defface prowj-runner-debug
  '((t (:slant italic)))
  "Debug and trace log levels.")

(defface prowj-runner-important
  '((t (:weight bold :underline t)))
  "Important words, preserving the output's foreground and background.")

(defface prowj-runner-location
  '((t (:underline t)))
  "Source links, preserving the output's colors.")

(defconst prowj-runner-level-prefix
  "^[ \t]*\\(?:[0-9][-0-9T:.,+Z/ ]*[ \t]+\\)?\\[?")

(defun prowj-runner-setup-appearance ()
  "Use terminal colors and color-neutral emphasis in a run buffer."
  (setq-local font-lock-defaults '(nil t t)
              font-lock-keywords-case-fold-search t)
  (when (require 'ghostel-faces nil t)
    (push '(default ghostel-default default) face-remapping-alist)
    (setq-local ansi-color-normal-colors-vector (cl-subseq ghostel-color-palette 0 8)
                ansi-color-bright-colors-vector (cl-subseq ghostel-color-palette 8 16)))
  (when (boundp 'ghostel-bold-color)
    (setq-local ansi-color-bold-is-bright (eq ghostel-bold-color 'bright)))
  (font-lock-add-keywords
   nil
   (mapcar
    (lambda (entry)
      (list (concat prowj-runner-level-prefix "\\(" (car entry) "\\)\\b")
            (list 1 (list 'quote (cdr entry)) 'prepend)))
    '(("ERROR\\|CRITICAL\\|FATAL" . prowj-runner-error)
      ("WARN\\(?:ING\\)?" . prowj-runner-warning)
      ("INFO" . prowj-runner-info)
      ("DEBUG\\|TRACE" . prowj-runner-debug))))
  (when prowj-runner-highlight-words
    (font-lock-add-keywords
     nil `((,(regexp-opt prowj-runner-highlight-words 'words)
            (0 'prowj-runner-important prepend)))))
  (font-lock-flush))

(defvar-local prowj-runner-command nil)
(defvar-local prowj-runner-status "Ready")
(defvar-local prowj-runner-error-position nil)
(defvar-local prowj-runner-project nil)
(defvar prowj-runner-processes nil)
(defvar prowj-runner-project-indicators (make-hash-table :test #'equal))

(defun prowj-runner-project-key (directory)
  "Find the project for DIRECTORY once, outside redisplay."
  (let ((default-directory directory))
    (file-name-as-directory
     (expand-file-name (or (ignore-errors (projectile-project-root)) directory)))))

(defun prowj-runner-project-processes (project)
  "Return live Prowj processes belonging to PROJECT without filesystem access."
  (cl-remove-if-not
   (lambda (process)
     (and (process-live-p process)
          (equal (process-get process 'prowj-runner-project) project)))
   prowj-runner-processes))

(defun prowj-runner-refresh-project (project)
  "Cache a compact indicator for PROJECT after a process changes state."
  (setq prowj-runner-processes (cl-remove-if-not #'process-live-p prowj-runner-processes))
  (let ((processes (prowj-runner-project-processes project)))
    (if (null processes)
        (remhash project prowj-runner-project-indicators)
      (puthash
       project
       (propertize
        (format "Run:%d" (length processes))
        'help-echo (concat project "\n"
                           (mapconcat (lambda (process)
                                        (or (process-get process 'prowj-runner-command) ""))
                                      (reverse processes) "\n")
                           "\nmouse-1: Choose running command")
        'mouse-face 'mode-line-highlight
        'local-map (make-mode-line-mouse-map 'mouse-1 #'prowj-runner-select-project-run))
       prowj-runner-project-indicators)))
  (force-mode-line-update t))

(defun prowj-runner-select-project-run (event)
  "Choose a running command from the project indicated by mouse EVENT."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (window-live-p window)
      (with-selected-window window
        (let* ((processes (prowj-runner-project-processes prowj-runner-project))
               (candidates (mapcar (lambda (process)
                                     (cons (buffer-name (process-buffer process))
                                           (process-buffer process)))
                                   processes)))
          (unless candidates (user-error "No running project commands"))
          (pop-to-buffer (cdr (assoc (completing-read "Running command: " candidates nil t)
                                    candidates))))))))

(defun prowj-runner-mode-line ()
  "Show the current run status and the cached project command count."
  (let ((active (gethash prowj-runner-project prowj-runner-project-indicators)))
    (concat " "
            (if (equal prowj-runner-status "Running")
                (or active "Run")
              (concat (cond ((equal prowj-runner-status "Finished") "Done")
                            ((equal prowj-runner-status "Stopped") "Stopped")
                            ((string-match "Failed (\\([0-9]+\\))" prowj-runner-status)
                             (concat "Fail:" (match-string 1 prowj-runner-status)))
                            ((string-prefix-p "Failed" prowj-runner-status) "Failed")
                            (t "Log"))
                      (when active (concat " / " active)))))))

(define-derived-mode prowj-run-mode special-mode "Prowj"
  "Read-only project command output, independent of compilation-mode."
  (setq-local mode-line-process
              '(:eval (prowj-runner-mode-line))
              next-error-function #'prowj-runner-next-error)
  (prowj-runner-setup-buffer))

(defun prowj-runner-setup-buffer ()
  "Configure bounded project command output."
  (setq-local bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t
              truncate-lines nil
              word-wrap t
              ansi-color-context nil
              prowj-runner-trimmed-characters 0
              prowj-runner-line-column 0
              prowj-runner-line-elided nil
              prowj-runner-output-line 1)
  (prowj-runner-setup-appearance)
  (buffer-disable-undo)
  (dolist (mode '(display-line-numbers-mode auto-highlight-symbol-mode))
    (when (and (boundp mode) (symbol-value mode)) (funcall mode -1)))
  (setq-local header-line-format
              '(:eval (if prowj-runner-log-error
                          (propertize (concat "Log archive error: " prowj-runner-log-error)
                                      'face 'error)
                        (format " Full log: %s  [C-c C-l]"
                                (or prowj-runner-log-file "starting…"))))))

(define-key prowj-run-mode-map (kbd "C-c C-l") #'prowj-runner-open-full-log)
(define-key prowj-run-mode-map (kbd "g") #'prowj-runner-rerun)
(define-key prowj-run-mode-map (kbd "C-c C-k") #'prowj-runner-stop)
(define-key prowj-run-mode-map (kbd "C-c C-c") #'prowj-runner-visit-source)
;; Output is read-only, so TAB has nothing to indent: move between links.
(define-key prowj-run-mode-map (kbd "TAB") #'forward-button)
(define-key prowj-run-mode-map (kbd "<backtab>") #'backward-button)

(defun prowj-runner-rerun ()
  "Repeat the command associated with this run buffer."
  (interactive)
  (unless prowj-runner-command (user-error "No previous command"))
  (prowj-runner-exec-command default-directory prowj-runner-command))

(defun prowj-runner-stop ()
  "Interrupt the current command, allowing it to clean up."
  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (unless (and process (process-live-p process)) (user-error "No running command"))
    (interrupt-process process)))

(defun prowj-runner-open-source (button)
  "Open the source location stored on BUTTON."
  (let ((file (button-get button 'prowj-runner-source))
        (line (button-get button 'prowj-runner-source-line))
        (column (button-get button 'prowj-runner-source-column)))
    (find-file-other-window (expand-file-name file default-directory))
    (goto-char (point-min))
    (forward-line (1- line))
    (when column (move-to-column (1- column)))))

(defun prowj-runner-visit-source ()
  "Visit the source button at point or on the current line."
  (interactive)
  (let ((button (or (button-at (point))
                    (next-button (line-beginning-position) t))))
    (unless (and button (<= (button-start button) (line-end-position)))
      (user-error "No source location on this line"))
    (button-activate button)))

(defun prowj-runner-next-error (n reset)
  "Visit the Nth source link in the preview, starting over when RESET is set."
  (setq reset (or reset (null prowj-runner-error-position)))
  (let ((position (if reset
                      (if (< n 0) (point-max) (point-min))
                    (marker-position prowj-runner-error-position)))
        button)
    (dotimes (_ (abs n))
      (setq button nil)
      (while (and (setq button (if (< n 0) (previous-button position)
                                (next-button position (and reset t))))
                  (not (button-get button 'prowj-runner-source)))
        (setq position (button-start button) reset nil))
      (unless button (user-error "No more source locations"))
      (setq position (button-start button) reset nil))
    (when button
      (goto-char position)
      (setq prowj-runner-error-position (copy-marker position))
      (prowj-runner-open-source button))))

(defconst prowj-runner-scan-look-back 512
  "How far before new output the filter rescans.
A source location or an escape sequence split by a chunk boundary is only
recognised if the scan reaches back over its earlier half, and neither
runs longer than this.")

(defun prowj-runner-link-sources (start end)
  "Link simple file:line[:column] and Python traceback locations in START..END.
Only the bounded preview is scanned; no filesystem access is performed here."
  (save-excursion
    (dolist (regexp '("\\(?:^\\|[ \t\"(]\\)\\([[:alnum:]_./~+@-]+\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?"
                      "File \"\\([^\"]+\\)\", line \\([0-9]+\\)"))
      (goto-char start)
      (while (re-search-forward regexp end t)
        (when (or (not (button-at (match-beginning 1)))
                  (get-text-property (match-beginning 1) 'prowj-runner-source))
          (let ((file (match-string-no-properties 1))
                (line (string-to-number (match-string-no-properties 2)))
                (column (match-string-no-properties 3)))
            (when (> line 0)
              (make-text-button
               (match-beginning 1) (match-end 0)
               'face 'prowj-runner-location 'follow-link t
               'action #'prowj-runner-open-source
               'help-echo "Visit source location"
               'prowj-runner-source file
               'prowj-runner-source-line line
               'prowj-runner-source-column (and column (string-to-number column))))))))))

(defun prowj-runner-insert-output (text)
  "Append TEXT while preserving scroll position unless following the tail."
  (let* ((inhibit-read-only t)
         (end (point-max))
         (following (= (point) end))
         (windows (cl-remove-if-not
                   (lambda (window) (= (window-point window) end))
                   (get-buffer-window-list (current-buffer) nil t))))
    (save-excursion
      (goto-char end)
      (let ((start (line-beginning-position)))
        (insert text)
        ;; Progress bars return to the start of the line to redraw themselves.
        ;; Left alone the carriage returns stay in the buffer as ^M and every
        ;; redraw piles onto the same line.  Scan from the new text rather than
        ;; from START: output that never breaks a line leaves START far behind,
        ;; and rescanning from there on every chunk is quadratic, while nothing
        ;; before the new text and its look-back can have changed anyway.
        ;; `comint-carriage-motion' works out the line beginning itself, so a
        ;; return still erases the whole line.
        (let ((fresh (max start (- (point-max)
                                   (+ (length text)
                                      prowj-runner-scan-look-back)))))
          (comint-carriage-motion fresh (point-max))
          ;; A return in TEXT erases the line, so the buffer can be shorter now.
          (setq fresh (min fresh (point-max)))
          ;; `ansi-color-apply' drops the cursor and erase sequences but leaves
          ;; OSC alone, so a command that sets the terminal title would show
          ;; the escape itself.  The look-back covers one split across chunks.
          (ansi-osc-apply-on-region fresh (point-max))
          (prowj-runner-link-sources fresh (point-max)))))
    (prowj-runner-trim-output)
    (when following (goto-char (point-max)))
    (dolist (window windows) (set-window-point window (point-max)))))


(defun prowj-runner-open-full-log ()
  "Open the complete text log of the current project run."
  (interactive)
  (unless prowj-runner-log-file (user-error "No archived run log"))
  (view-file-other-window prowj-runner-log-file))

(defun prowj-runner-open-log-line (button)
  "Open the archived line referenced by BUTTON."
  (view-file-other-window (button-get button 'prowj-runner-file))
  (goto-char (point-min))
  (forward-line (1- (button-get button 'prowj-runner-line))))

(defun prowj-runner-preview (text)
  "Return bounded line previews of TEXT, preserving state between chunks."
  (let ((start 0) parts)
    (while (< start (length text))
      (let* ((newline (string-search "\n" text start))
             (end (or newline (length text))))
        (unless prowj-runner-line-elided
          (let ((visible (min (- end start)
                              (max 0 (- prowj-runner-line-limit prowj-runner-line-column)))))
            (push (substring text start (+ start visible)) parts)
            (cl-incf prowj-runner-line-column visible)
            (when (< visible (- end start))
              (setq prowj-runner-line-elided t)
              (push (make-text-button " … [full log]" nil
                                      'follow-link t
                                      'action #'prowj-runner-open-log-line
                                      'prowj-runner-file prowj-runner-log-file
                                      'prowj-runner-line prowj-runner-output-line
                                      'help-echo "Open the complete line in the archived log")
                    parts)
              ;; End the preview now, even when a huge line arrives in pieces.
              (push "\n" parts))))
        (when newline
          (unless prowj-runner-line-elided (push "\n" parts))
          (setq prowj-runner-line-column 0 prowj-runner-line-elided nil)
          (cl-incf prowj-runner-output-line))
        (setq start (if newline (1+ newline) end))))
    (apply #'concat (nreverse parts))))

(defun prowj-runner-archive (text)
  "Append TEXT to the current run's complete log without text properties."
  (when (and prowj-runner-log-file (not prowj-runner-log-error))
    (condition-case err
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region (substring-no-properties text) nil
                        prowj-runner-log-file t 'silent))
      (file-error
       ;; Logging trouble must not abort the process filter or hide output.
       (setq prowj-runner-log-error (error-message-string err))
       (message "Project log archive failed: %s" prowj-runner-log-error)))))

(defun prowj-runner-trim-output ()
  "Keep a bounded tail of output."
  (save-restriction
    (widen)
    (when (> (buffer-size) prowj-runner-buffer-limit)
      (let ((inhibit-read-only t)
            (cut (- (point-max) (max 1 (/ (* prowj-runner-buffer-limit 3) 4)))))
        (save-excursion
          (goto-char cut)
          ;; Keep complete lines, unless the entire tail is one giant line.
          (let ((next (line-beginning-position 2)))
            (when (< next (point-max)) (setq cut next))))
        (cl-incf prowj-runner-trimmed-characters (- cut (point-min)))
        (delete-region (point-min) cut)))))

(defun prowj-runner-filter (process text)
  "Archive complete output and append a bounded, colored preview."
  (when-let* ((buffer (process-buffer process))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((colored (ansi-color-apply text)))
        ;; Save before trimming; escape sequences split across chunks are kept
        ;; in ansi-color-context and completed on the next filter invocation.
        (unless (string-empty-p colored)
          (prowj-runner-archive colored)
          (let ((preview (prowj-runner-preview colored)))
            (unless (string-empty-p preview)
              (prowj-runner-insert-output preview))))))))

(defun prowj-runner-prepare-archive ()
  "Prepare a new local archive before starting the command."
  (setq-local prowj-runner-log-error nil)
  (condition-case err
      (progn
        (make-directory prowj-runner-log-directory t)
        (setq-local prowj-runner-log-file
                    (make-temp-file
                     (expand-file-name (format-time-string "%Y%m%d-%H%M%S-")
                                       prowj-runner-log-directory)
                     nil ".log")))
    (file-error
     (setq-local prowj-runner-log-error (error-message-string err))
     (message "Project log archive failed: %s" prowj-runner-log-error))))

(defun prowj-runner-sentinel (process _event)
  "Report a terminal PROCESS status and update its project's run count."
  (when (and (memq (process-status process) '(exit signal failed))
             (not (process-get process 'prowj-runner-finished)))
    (process-put process 'prowj-runner-finished t)
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer (process-buffer process)
        (setq prowj-runner-status
              (pcase (process-status process)
                ('exit (if (zerop (process-exit-status process)) "Finished"
                         (format "Failed (%d)" (process-exit-status process))))
                ('signal "Stopped")
                (_ "Failed")))
        (let ((text (format "\n[Process %s]\n" prowj-runner-status)))
          (prowj-runner-archive text)
          (prowj-runner-insert-output text))))
    (prowj-runner-refresh-project (process-get process 'prowj-runner-project))))

(defun prowj-runner-exec-command (directory command &optional new-frame)
  "Run non-interactive COMMAND in DIRECTORY with bounded project logs.
Use a terminal for commands requiring terminal input or full-screen interfaces."
  (unless (file-directory-p directory)
    (user-error "Directory not found: %s" directory))
  (when (or (null command) (string-empty-p command))
    (user-error "Command is empty"))
  (when (< prowj-runner-buffer-limit 4096)
    (user-error "Project log buffer limit must be at least 4096 characters"))
  (when (file-remote-p prowj-runner-log-directory)
    (user-error "Archive logs locally to avoid blocking output on remote writes"))
  (projectile-save-project-buffers)
  (let* ((parent-frame (selected-frame))
         (run-directory (file-name-as-directory (expand-file-name directory)))
         (project (prowj-runner-project-key run-directory))
         (short-name (file-name-nondirectory (directory-file-name run-directory)))
         (name (format "*prowj:%s:%s*" short-name command))
         (process-connection-type nil)
         (process-environment
          (append '("TERM=xterm-256color" "FORCE_COLOR=1" "PYTHONUNBUFFERED=1")
                  process-environment))
         buffer)
    ;; Finish the previous process before reusing its buffer.
    (when-let* ((old-buffer (get-buffer name))
                (old-process (get-buffer-process old-buffer)))
      (delete-process old-process))
    (unwind-protect
        (progn
          (when new-frame (prowj-focus-log-window))
          (setq buffer (get-buffer-create name))
          (with-current-buffer buffer
            (prowj-run-mode)
            (setq default-directory run-directory
                  prowj-runner-command command
                  prowj-runner-project project
                  prowj-runner-status "Running")
            (let ((inhibit-read-only t)) (erase-buffer))
            (prowj-runner-prepare-archive)
            ;; start-file-process-shell-command also handles TRAMP directories.
            (condition-case err
                (let ((process (start-file-process-shell-command "prowj" buffer command)))
                  (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
                  (set-process-filter process #'prowj-runner-filter)
                  (set-process-sentinel process #'prowj-runner-sentinel)
                  (process-put process 'prowj-runner-project project)
                  (process-put process 'prowj-runner-command command)
                  (push process prowj-runner-processes)
                  (prowj-runner-refresh-project project))
              (error
               (setq prowj-runner-status "Failed to start")
               (prowj-runner-archive (concat (error-message-string err) "\n"))
               (prowj-runner-insert-output (concat (error-message-string err) "\n"))
               (signal (car err) (cdr err)))))
          (switch-to-buffer buffer)
          (when new-frame (delete-other-windows))
          (prowj-reload-dir-locals))
      (when (and new-frame (frame-live-p parent-frame))
        (select-frame-set-input-focus parent-frame)))
    buffer))

(provide 'prowj-runner)
;;; prowj-runner.el ends here
