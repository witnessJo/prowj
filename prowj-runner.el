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

(defcustom prowj-runner-log-keep 50
  "Number of recent run logs to keep in `prowj-runner-log-directory'.
Surplus logs are deleted when a run starts.  nil keeps every log."
  :type '(choice (natnum :tag "Logs to keep")
                 (const :tag "Keep every log" nil)))

(defcustom prowj-runner-log-max-age 14
  "Days a run log is kept in `prowj-runner-log-directory'.
Older logs are deleted when a run starts, however few
`prowj-runner-log-keep' would allow.  nil keeps logs of every age."
  :type '(choice (natnum :tag "Days")
                 (const :tag "Keep logs of every age" nil)))

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

(defcustom prowj-use-pty t
  "Non-nil runs commands on a pty set up like an ordinary terminal.
Typed keys then go straight to the command and the terminal edits the
line, as in a shell.  nil runs commands on a pipe and collects one line
of input in Emacs before sending it.  The change applies to commands
started afterwards."
  :type 'boolean)

(defconst prowj-runner-stty-settings "echo erase '^?' kill '^U' werase '^W' iutf8"
  "Terminal settings for a run's pty.
Emacs opens ptys without echo or erase and kill characters, because
comint edits lines itself; a run buffer leaves editing to the terminal.")

(defconst prowj-runner-tty-wait-syscalls
  (if (string-prefix-p "aarch64" system-configuration)
      '(63 72 73)
    '(0 7 23 270 271))
  "Syscall numbers of read, then of poll and select, on this machine.")

(defvar ghostel-color-palette)
(defvar ghostel-bold-color)

(defvar-local prowj-runner-log-file nil)
(defvar-local prowj-runner-log-error nil)
(defvar-local prowj-runner-trimmed-characters 0)
(defvar-local prowj-runner-line-column 0)
(defvar-local prowj-runner-line-elided nil)
(defvar-local prowj-runner-output-line 1)
(defvar-local prowj-runner-terminal-column 0
  "Column of a pty run's terminal cursor on the last line.")
(defvar-local prowj-runner-terminal-pending ""
  "Start of an escape sequence held back until the rest arrives.")

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

(defvar-local prowj-runner-input "")
(defvar-local prowj-runner-input-overlay nil)

(defun prowj-runner-remove-input-overlay ()
  "Remove the pending-input display when changing modes or rerunning."
  (when (overlayp prowj-runner-input-overlay)
    (delete-overlay prowj-runner-input-overlay)
    (setq prowj-runner-input-overlay nil)))

(defun prowj-runner-input-process ()
  "Return the live process for the current log buffer."
  (let ((process (get-buffer-process (current-buffer))))
    (unless (and process (process-live-p process))
      (user-error "No running process in this buffer"))
    process))

(defun prowj-runner-refresh-input (&rest _)
  "Display pending input after the output without modifying the log."
  (when (derived-mode-p 'prowj-run-mode)
    (unless (overlayp prowj-runner-input-overlay)
      (setq prowj-runner-input-overlay (make-overlay (point-max) (point-max))))
    (move-overlay prowj-runner-input-overlay (point-max) (point-max))
    (overlay-put prowj-runner-input-overlay 'after-string
                 (unless (string-empty-p prowj-runner-input)
                   (concat (propertize prowj-runner-input 'face 'font-lock-string-face)
                           (propertize " " 'cursor t))))))

(defun prowj-runner-pty-process ()
  "Return the current run buffer's live pty process, or nil."
  (when-let* ((process (get-buffer-process (current-buffer)))
              ((process-live-p process))
              ((process-tty-name process)))
    process))

(defun prowj-runner-send-terminal (process string)
  "Send STRING to the terminal of PROCESS and follow its cursor."
  (process-send-string process string)
  (goto-char (prowj-runner-terminal-point)))

(defun prowj-runner-read-proc (pid file)
  "Return the contents of /proc/PID/FILE, or nil when it cannot be read."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents (format "/proc/%d/%s" pid file))
      (buffer-string))))

(defun prowj-runner-descendants (pid)
  "Return PID and the ids of all its descendant processes."
  (let ((pending (list pid)) result)
    (while pending
      (let ((current (pop pending)))
        (push current result)
        (dolist (task (ignore-errors
                        (directory-files (format "/proc/%d/task" current)
                                         nil "\\`[0-9]+\\'")))
          (dolist (child (split-string
                          (or (prowj-runner-read-proc
                               current (format "task/%s/children" task))
                              "")))
            (push (string-to-number child) pending)))))
    result))

(defun prowj-runner-pid-reading-tty-p (pid tty)
  "Return non-nil when PID waits to read TTY on its standard input."
  (when (equal (file-symlink-p (format "/proc/%d/fd/0" pid)) tty)
    (let ((syscall (split-string (or (prowj-runner-read-proc pid "syscall") ""))))
      (if (and syscall (string-match-p "\\`[0-9]+\\'" (car syscall)))
          (let ((number (string-to-number (car syscall))))
            (if (= number (car prowj-runner-tty-wait-syscalls))
                (equal (cadr syscall) "0x0")
              (memq number (cdr prowj-runner-tty-wait-syscalls))))
        ;; No syscall to look at: a tty read sleeps in `wait_woken'.
        (member (prowj-runner-read-proc pid "wchan") '("wait_woken" "n_tty_read"))))))

(defun prowj-runner-process-reading-p (process)
  "Return non-nil when a process of the pty PROCESS is reading its terminal.
Local runs only: /proc of a remote host is out of reach."
  (when-let* ((tty (process-tty-name process))
              (pid (process-id process))
              ((not (file-remote-p default-directory))))
    (seq-some (lambda (child) (prowj-runner-pid-reading-tty-p child tty))
              (prowj-runner-descendants pid))))

(defun prowj-runner-global-command (keys)
  "Run the global binding of KEYS, a vector, as if the run buffer had none.
Empty KEYS stand for `last-command-event'."
  (let ((command (lookup-key global-map (if (length> keys 0) keys
                                          (vector last-command-event)))))
    (if (commandp command)
        (progn (setq this-command command)
               (call-interactively command))
      (ding))))

(defun prowj-runner-insert-input (count)
  "Enter COUNT copies of the typed character as process input.
A pty run receives them at once; a pipe run collects them as pending
input."
  (interactive "p")
  (let ((process (prowj-runner-input-process)))
    (unless (characterp last-command-event)
      (user-error "Use printable characters to enter process input"))
    (if (process-tty-name process)
        (prowj-runner-send-terminal process (make-string count last-command-event))
      (setq prowj-runner-input
            (concat prowj-runner-input (make-string count last-command-event)))
      (goto-char (point-max))
      (prowj-runner-refresh-input))))

(defun prowj-runner-delete-input ()
  "Delete the last input character without editing output."
  (interactive)
  (if-let* ((process (prowj-runner-pty-process)))
      (prowj-runner-send-terminal process "\177")
    (unless (string-empty-p prowj-runner-input)
      (setq prowj-runner-input (substring prowj-runner-input 0 -1)))
    (goto-char (point-max))
    (prowj-runner-refresh-input)))

(defun prowj-runner-reset-input ()
  "Forget pending pipe input, sending nothing to the command."
  (setq prowj-runner-input "")
  (prowj-runner-refresh-input))

(defun prowj-runner-clear-input ()
  "Discard the input line typed so far."
  (interactive)
  (if-let* ((process (prowj-runner-pty-process)))
      (prowj-runner-send-terminal process "\C-u")
    (setq prowj-runner-input "")
    (prowj-runner-refresh-input)))

(defun prowj-runner-yank-input ()
  "Paste the latest kill as process input.
A pty run receives it as a terminal paste; a pipe run takes one line."
  (interactive)
  (let ((process (prowj-runner-input-process))
        (text (current-kill 0)))
    (if (process-tty-name process)
        (prowj-runner-send-terminal process (substring-no-properties text))
      (when (string-match-p "[\n\r]" text)
        (user-error "Paste a single line, then press Enter to send it"))
      (setq prowj-runner-input (concat prowj-runner-input text))
      (goto-char (point-max))
      (prowj-runner-refresh-input))))

(defun prowj-runner-submit-input ()
  "End the input line and send it, allowing an empty line."
  (interactive)
  (let ((process (prowj-runner-input-process)))
    (if (process-tty-name process)
        (prowj-runner-send-terminal process "\r")
      (process-send-string process (concat prowj-runner-input "\n"))
      (prowj-runner-clear-input)
      (goto-char (point-max)))))

(defun prowj-runner-insert-state-p ()
  "Return non-nil unless a modal editing state keeps keys for Emacs.
Meow's normal and motion states browse the output; its insert state,
and buffers without Meow, type into the terminal."
  (not (and (bound-and-true-p meow-mode)
            (not (bound-and-true-p meow-insert-mode)))))

(defun prowj-runner-terminal-key-process ()
  "Return the pty process that should receive a key Emacs also uses.
That is while typing (see `prowj-runner-insert-state-p') and while one
of the command's processes reads its terminal; nil otherwise."
  (when-let* (((prowj-runner-insert-state-p))
              (process (prowj-runner-pty-process))
              ((prowj-runner-process-reading-p process)))
    process))

(defun prowj-runner-terminal-raw-p (process)
  "Return non-nil when the terminal of PROCESS is out of canonical mode.
A program such as readline then edits the line itself and moves its
cursor; in canonical mode the terminal has no cursor motion to offer."
  (when-let* ((tty (process-tty-name process))
              ((not (file-remote-p default-directory))))
    (with-temp-buffer
      (and (eq 0 (ignore-errors (call-process "stty" nil t nil "-F" tty "-a")))
           (progn (goto-char (point-min))
                  (re-search-forward "\\(?:^\\|[ \t]\\)-icanon\\_>" nil t))))))

(defun prowj-runner-delete-word-input ()
  "Erase the last word typed while the command reads its terminal.
Otherwise, and in pipe runs, run the key's global command."
  (interactive)
  (if-let* ((process (prowj-runner-terminal-key-process)))
      (prowj-runner-send-terminal process "\C-w")
    (prowj-runner-global-command (this-command-keys-vector))))

(defun prowj-runner-eof-input ()
  "Send end of input while the command reads its terminal.
On an empty line the command reads end of file.  Otherwise, and in pipe
runs, run the key's global command."
  (interactive)
  (if-let* ((process (prowj-runner-terminal-key-process)))
      (prowj-runner-send-terminal process "\C-d")
    (prowj-runner-global-command (this-command-keys-vector))))

(defconst prowj-runner-terminal-motion-keys
  '(("C-a" . "\C-a") ("C-e" . "\C-e") ("C-b" . "\C-b") ("C-f" . "\C-f")
    ("<left>" . "\e[D") ("<right>" . "\e[C") ("<up>" . "\e[A") ("<down>" . "\e[B")
    ("<home>" . "\e[H") ("<end>" . "\e[F"))
  "Keys moving a line editor's cursor or history, and what a terminal sends.")

(defun prowj-runner-terminal-motion ()
  "Send the key's terminal sequence to a line editor reading its terminal.
That is while typing, while the command reads, and while it edits the
line itself (readline, say); the cursor and history are then its own.
Otherwise the key runs its global command, moving around the output."
  (interactive)
  (let* ((keys (this-command-keys-vector))
         (keys (if (length> keys 0) keys (vector last-command-event)))
         (sequence (cdr (assoc (key-description keys)
                               prowj-runner-terminal-motion-keys))))
    (if-let* ((sequence)
              (process (prowj-runner-terminal-key-process))
              ((prowj-runner-terminal-raw-p process)))
        (prowj-runner-send-terminal process sequence)
      (prowj-runner-global-command keys))))

(defun prowj-runner-send-input ()
  "Read one line and send it to the current project command's standard input.
An empty line is allowed.  Input is not added to the log or input history."
  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (unless (and process (process-live-p process))
      (user-error "No running process in this buffer"))
    (let ((input (read-string "Process input (Enter sends an empty line): " nil t)))
      (unless (process-live-p process)
        (user-error "The process exited before input could be sent"))
      (process-send-string process (concat input "\n")))))

(define-derived-mode prowj-run-mode special-mode "Prowj"
  "Read-only project output with direct line input to the running command."
  (setq-local mode-line-process
              '(:eval (prowj-runner-mode-line))
              next-error-function #'prowj-runner-next-error)
  (prowj-runner-setup-buffer)
  (add-hook 'change-major-mode-hook #'prowj-runner-remove-input-overlay nil t)
  (prowj-runner-reset-input))

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
              prowj-runner-output-line 1
              prowj-runner-terminal-column 0
              prowj-runner-terminal-pending "")
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
;; Printable characters are process input, never log edits or commands.
(dotimes (index 95)
  (define-key prowj-run-mode-map (vector (+ 32 index)) #'prowj-runner-insert-input))
(define-key prowj-run-mode-map [remap self-insert-command] #'prowj-runner-insert-input)
(define-key prowj-run-mode-map [remap undefined] #'prowj-runner-insert-input)
(define-key prowj-run-mode-map (kbd "RET") #'prowj-runner-submit-input)
(define-key prowj-run-mode-map (kbd "<return>") #'prowj-runner-submit-input)
(define-key prowj-run-mode-map (kbd "DEL") #'prowj-runner-delete-input)
(define-key prowj-run-mode-map (kbd "<backspace>") #'prowj-runner-delete-input)
(define-key prowj-run-mode-map (kbd "C-u") #'prowj-runner-clear-input)
(define-key prowj-run-mode-map (kbd "C-y") #'prowj-runner-yank-input)
;; Emacs commands, except while a pty run's command reads its terminal.
(define-key prowj-run-mode-map (kbd "C-<backspace>") #'prowj-runner-delete-word-input)
(define-key prowj-run-mode-map (kbd "M-DEL") #'prowj-runner-delete-word-input)
(define-key prowj-run-mode-map (kbd "M-<backspace>") #'prowj-runner-delete-word-input)
(define-key prowj-run-mode-map (kbd "C-w") #'prowj-runner-delete-word-input)
(define-key prowj-run-mode-map (kbd "C-d") #'prowj-runner-eof-input)
(dolist (key prowj-runner-terminal-motion-keys)
  (define-key prowj-run-mode-map (kbd (car key)) #'prowj-runner-terminal-motion))
(define-key prowj-run-mode-map (kbd "C-c C-i") #'prowj-runner-send-input)
(define-key prowj-run-mode-map (kbd "C-c C-r") #'prowj-runner-rerun)
(define-key prowj-run-mode-map (kbd "C-c C-k") #'prowj-runner-stop)
(define-key prowj-run-mode-map (kbd "C-c C-c") #'prowj-runner-stop)
(define-key prowj-run-mode-map (kbd "C-c C-o") #'prowj-runner-visit-source)
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
    (interrupt-process process)
    (prowj-runner-reset-input)))

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
          ;; A BEL left over is a terminal bell, such as readline's when there
          ;; is nothing to erase.  Terminals ring it rather than show it.
          (goto-char fresh)
          (while (search-forward "\a" nil t)
            (delete-char -1))
          (prowj-runner-link-sources fresh (point-max)))))
    (prowj-runner-trim-output)
    (when following (goto-char (point-max)))
    (dolist (window windows) (set-window-point window (point-max)))
    (prowj-runner-refresh-input)))


;;;; Terminal cursor on the last line of a pty run

;; A pty run's line editor (readline, say) moves its cursor with
;; backspaces, carriage returns and CSI sequences and redraws the line
;; around it.  The run buffer follows that on its last line: a column
;; stands for the cursor, text overwrites from it, and only a newline
;; moves on.  Cursor motion across lines, as full-screen programs use,
;; is not followed; those belong in a terminal emulator.

(defconst prowj-runner-terminal-control-regexp
  (concat "\\(\e\\][^\a\e]*\\(?:\a\\|\e\\\\\\)\\)" ; 1: OSC
          "\\|\r\\|\b\\|\a"
          "\\|\e\\[\\([0-9;]*\\)\\([@CDGKPX]\\)")   ; 2: parameters, 3: final
  "Terminal controls a pty run renders itself.
Other CSI sequences, such as colors, are left to `ansi-color-apply'.")

(defconst prowj-runner-terminal-partial-regexp
  "\e\\(?:\\[[0-9;?]*\\|\\][^\a\e]*\e?\\)?\\'"
  "An escape sequence cut off at the end of a chunk of output.")

(defun prowj-runner-terminal-line-start ()
  "Return the start of the last line, where the terminal cursor is."
  (save-excursion (goto-char (point-max)) (pos-bol)))

(defun prowj-runner-terminal-goto (column &optional pad)
  "Move to COLUMN of the last line.
With PAD, extend a shorter line with spaces, as writing there would."
  (goto-char (prowj-runner-terminal-line-start))
  (move-to-column column pad))

(defun prowj-runner-terminal-point ()
  "Return the position of the terminal cursor, or the end of the output."
  (save-excursion
    (prowj-runner-terminal-goto prowj-runner-terminal-column)
    (point)))

(defun prowj-runner-terminal-write (text)
  "Write TEXT at the terminal cursor, overwriting as a terminal does.
A newline ends the last line wherever the cursor is."
  (let ((start 0))
    (while (< start (length text))
      (let* ((newline (string-search "\n" text start))
             (end (or newline (length text))))
        (when (< start end)
          (let* ((piece (substring text start end))
                 (width (string-width piece)))
            (prowj-runner-terminal-goto prowj-runner-terminal-column t)
            (let ((from (point)))
              (move-to-column (+ prowj-runner-terminal-column width))
              (delete-region from (point)))
            (insert piece)
            (cl-incf prowj-runner-terminal-column width)))
        (when newline
          (goto-char (point-max))
          (insert "\n")
          (setq prowj-runner-terminal-column 0))
        (setq start (if newline (1+ newline) end))))))

(defun prowj-runner-terminal-erase (from-column to-column)
  "Blank FROM-COLUMN up to TO-COLUMN of the last line, keeping its length."
  (when (< from-column to-column)
    (prowj-runner-terminal-goto from-column t)
    (let ((from (point)))
      (move-to-column to-column)
      (let ((width (- (current-column)
                      (save-excursion (goto-char from) (current-column)))))
        (delete-region from (point))
        (insert (make-string width ?\s))))))

(defun prowj-runner-terminal-csi (final parameter)
  "Carry out the CSI sequence ending in FINAL with numeric PARAMETER.
PARAMETER is nil when the sequence gives none."
  (let ((count (max 1 (or parameter 1)))
        (column prowj-runner-terminal-column))
    (pcase final
      (?C (setq prowj-runner-terminal-column (+ column count)))
      (?D (setq prowj-runner-terminal-column (max 0 (- column count))))
      (?G (setq prowj-runner-terminal-column (1- count)))
      (?K (pcase (or parameter 0)
            (0 (prowj-runner-terminal-goto column)
               (delete-region (point) (point-max)))
            (1 (prowj-runner-terminal-erase 0 (1+ column)))
            (_ (delete-region (prowj-runner-terminal-line-start) (point-max)))))
      (?P (prowj-runner-terminal-goto column)
          (let ((from (point)))
            (move-to-column (+ column count))
            (delete-region from (point))))
      (?@ (prowj-runner-terminal-goto column t)
          (insert (make-string count ?\s)))
      (?X (prowj-runner-terminal-erase column (+ column count))))))

(defun prowj-runner-terminal-text (text)
  "Archive TEXT and write its bounded, colored preview at the cursor."
  (let ((colored (ansi-color-apply text)))
    (unless (string-empty-p colored)
      (prowj-runner-archive colored)
      (prowj-runner-terminal-write (prowj-runner-preview colored)))))

(defun prowj-runner-terminal-output (text)
  "Render pty output TEXT on the last line as a terminal would.
Keep the scroll position unless following the terminal cursor."
  (setq text (concat prowj-runner-terminal-pending text)
        prowj-runner-terminal-pending "")
  (when-let* ((partial (string-match prowj-runner-terminal-partial-regexp text))
              ((< (- (length text) partial) 4096)))
    (setq prowj-runner-terminal-pending (substring text partial)
          text (substring text 0 partial)))
  (let* ((inhibit-read-only t)
         (cursor (prowj-runner-terminal-point))
         (following (memq (point) (list cursor (point-max))))
         (windows (cl-remove-if-not
                   (lambda (window) (memq (window-point window) (list cursor (point-max))))
                   (get-buffer-window-list (current-buffer) nil t)))
         (fresh (prowj-runner-terminal-line-start))
         (start 0))
    (save-excursion
      (while (string-match prowj-runner-terminal-control-regexp text start)
        (let ((control (match-string 0 text))
              (osc (match-string 1 text))
              (final (match-string 3 text))
              (parameter (match-string 2 text))
              (end (match-end 0)))
          (prowj-runner-terminal-text (substring text start (match-beginning 0)))
          (cond
           (osc
            ;; Zero width once applied: title, directory, hyperlink.
            (prowj-runner-terminal-goto prowj-runner-terminal-column t)
            (let ((from (point)))
              (insert osc)
              (ansi-osc-apply-on-region from (point))))
           (final
            (prowj-runner-terminal-csi
             (aref final 0)
             (and (string-match "\\`[0-9]+" parameter)
                  (string-to-number (match-string 0 parameter)))))
           ((equal control "\r")
            (prowj-runner-archive control)
            (setq prowj-runner-terminal-column 0))
           ((equal control "\b")
            (prowj-runner-archive control)
            (setq prowj-runner-terminal-column
                  (max 0 (1- prowj-runner-terminal-column)))))
          ;; A bell is rung by terminals, not shown, so it is dropped.
          ;; The preview's line budget follows the cursor.
          (setq prowj-runner-line-column prowj-runner-terminal-column
                start end)))
      (prowj-runner-terminal-text (substring text start))
      (prowj-runner-link-sources (min fresh (point-max)) (point-max)))
    (prowj-runner-trim-output)
    (let ((cursor (prowj-runner-terminal-point)))
      (when following (goto-char cursor))
      (dolist (window windows) (set-window-point window cursor)))
    (prowj-runner-refresh-input)))

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
      (if (process-tty-name process)
          (prowj-runner-terminal-output text)
      (let ((colored (ansi-color-apply text)))
        ;; Save before trimming; escape sequences split across chunks are kept
        ;; in ansi-color-context and completed on the next filter invocation.
        (unless (string-empty-p colored)
          (prowj-runner-archive colored)
          (let ((preview (prowj-runner-preview colored)))
            (unless (string-empty-p preview)
              (prowj-runner-insert-output preview)))))))))

(defun prowj-runner-logs-in-use ()
  "Return the log files of runs that are still writing."
  (delq nil
        (mapcar (lambda (buffer)
                  (with-current-buffer buffer
                    (and prowj-runner-log-file
                         (process-live-p (get-buffer-process buffer))
                         prowj-runner-log-file)))
                (buffer-list))))

(defun prowj-runner-expire-logs ()
  "Delete archived logs beyond the retention options.
Keep the newest `prowj-runner-log-keep' logs and discard those older
than `prowj-runner-log-max-age' days.  A run still writing keeps its
log whatever the options say."
  (interactive)
  (when (and (file-directory-p prowj-runner-log-directory)
             (or prowj-runner-log-keep prowj-runner-log-max-age))
    (condition-case err
        (let ((in-use (prowj-runner-logs-in-use))
              (cutoff (and prowj-runner-log-max-age
                           (time-subtract nil (* prowj-runner-log-max-age 86400))))
              (kept 0)
              (deleted 0)
              logs)
          (dolist (file (directory-files prowj-runner-log-directory t "\\.log\\'"))
            (push (cons file (file-attribute-modification-time
                              (file-attributes file)))
                  logs))
          ;; Newest first, so the survivors come before the count runs out.
          (setq logs (sort logs (lambda (a b) (time-less-p (cdr b) (cdr a)))))
          (pcase-dolist (`(,file . ,written) logs)
            (if (or (member file in-use)
                    (not (or (and cutoff (time-less-p written cutoff))
                             (and prowj-runner-log-keep
                                  (>= kept prowj-runner-log-keep)))))
                (cl-incf kept)
              (delete-file file)
              (cl-incf deleted)))
          (when (and deleted (called-interactively-p 'interactive))
            (message "Deleted %d run log%s, kept %d"
                     deleted (if (= deleted 1) "" "s") kept)))
      (file-error
       ;; Housekeeping must not keep a run from starting.
       (message "Project log expiry failed: %s" (error-message-string err))))))

(defun prowj-runner-log-prefix (project)
  "Return the leading file name identifying a PROJECT run log.
Keep letters of any script, so a project named in Korean still reads as
itself, and fold away the whitespace and punctuation that a shell, a
listing or a completion prompt would stumble over."
  (let* ((name (if (stringp project)
                   (file-name-nondirectory (directory-file-name project))
                 ""))
         (safe (string-trim (replace-regexp-in-string "[^[:alnum:]._-]+" "-" name)
                            "[-._]+" "[-._]+")))
    (when (string-empty-p safe) (setq safe "project"))
    (format "%s-%s-" (if (> (length safe) 40) (substring safe 0 40) safe)
            (format-time-string "%Y%m%d-%H%M%S"))))

(defun prowj-runner-prepare-archive ()
  "Prepare a new local archive before starting the command."
  (setq-local prowj-runner-log-error nil)
  (condition-case err
      (progn
        (make-directory prowj-runner-log-directory t)
        (prowj-runner-expire-logs)
        (setq-local prowj-runner-log-file
                    (make-temp-file
                     (expand-file-name
                      (prowj-runner-log-prefix prowj-runner-project)
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
  "Run COMMAND in DIRECTORY with bounded logs and direct standard input.
With `prowj-use-pty' the command runs on a terminal-like pty; otherwise
on a pipe with line input.  Use a terminal emulator for full-screen
interfaces."
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
         (process-connection-type prowj-use-pty)
         (window (selected-window))
         ;; The shell sets the pty up as a terminal before running COMMAND.
         (run-command
          (if prowj-use-pty
              (format "stty %s rows %d cols %d 2>/dev/null; %s"
                      prowj-runner-stty-settings (window-body-height window)
                      (window-max-chars-per-line window) command)
            command))
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
                (let ((process (start-file-process-shell-command "prowj" buffer run-command)))
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
          ;; Fit the pty to the window now showing the run.
          (when-let* ((process (get-buffer-process buffer))
                      ((process-tty-name process))
                      (shown (get-buffer-window buffer t)))
            (set-process-window-size process (window-body-height shown)
                                     (window-max-chars-per-line shown)))
          (prowj-reload-dir-locals))
      (when (and new-frame (frame-live-p parent-frame))
        (select-frame-set-input-focus parent-frame)))
    buffer))

(provide 'prowj-runner)
;;; prowj-runner.el ends here
