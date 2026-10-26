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
(require 'seq)
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

;;; Commands declared by the project's own task files

(defcustom prowj-discover-functions
  '(prowj-discover-makefile prowj-discover-npm-scripts)
  "Functions that read a project's commands out of its task files.
Each is called with the project root directory and returns a list of
\(LABEL COMMAND DIRECTORY) entries, the shape `prowj-subcmds' uses."
  :type '(repeat function)
  :group 'prowj)

(defcustom prowj-discover-ignore-regexp nil
  "Labels matching this are left out of the command list.
Discovery offers every target a task file declares, publish and deploy
targets included, and one of those is easy to pick by mistake.  The
default hides nothing: a command missing from the list without having
been asked for is harder to explain than one target too many."
  :type '(choice (const :tag "Hide nothing" nil) regexp)
  :group 'prowj)

(defcustom prowj-discover-npm-depth 1
  "How deep below the project root `package.json' is looked for.
A workspace tends to keep its client in a subdirectory of its own."
  :type 'natnum
  :group 'prowj)

(defcustom prowj-discover-skip-directories
  '("node_modules" "target" "dist" "build" "vendor" ".venv" ".git")
  "Directory names discovery does not walk into."
  :type '(repeat string)
  :group 'prowj)

(defconst prowj-discover-makefile-names '("Makefile" "makefile" "GNUmakefile")
  "The names GNU make itself looks for, in its own order.")

(defun prowj-discover--makefile-line-targets (line)
  "Return the targets declared on LINE of a makefile.
Nil for everything that only looks like a rule: a recipe, which is
indented; a variable, whose colon belongs to an assignment; and a
pattern rule, which names no target of its own."
  (let ((text (car (split-string line "#"))))
    (unless (or (string-empty-p text)
              (string-match-p "\\`[ \t]" text)
              (string-match-p "\\`[^:]*:=" text))
      (let ((colon (string-search ":" text)))
        (when colon
          (let ((head (substring text 0 colon)))
            ;; An `=' ahead of the colon makes the whole line a variable.
            (unless (string-search "=" head)
              (seq-remove
                (lambda (target)
                  (or (string-prefix-p "." target)
                    (string-search "%" target)
                    (string-search "$" target)))
                (split-string head "[ \t]+" t)))))))))

(defun prowj-discover-makefile (root)
  "Return the commands the makefile in ROOT declares."
  (let ((file (seq-find #'file-readable-p
                (mapcar (lambda (name) (expand-file-name name root))
                  prowj-discover-makefile-names)))
         (entries nil)
         (seen nil))
    (when file
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (dolist (target (prowj-discover--makefile-line-targets
                            (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position))))
            (unless (member target seen)
              (push target seen)
              (push (list (format "make: %s" target)
                      (format "make %s" target)
                      root)
                entries)))
          (forward-line 1))))
    (nreverse entries)))

(defun prowj-discover--package-json-files (directory depth)
  "Return the `package.json' files in DIRECTORY and DEPTH levels below it."
  (let ((file (expand-file-name "package.json" directory))
         (files nil))
    (when (file-readable-p file)
      (push file files))
    (when (> depth 0)
      (dolist (entry (ignore-errors
                       (directory-files directory t
                         directory-files-no-dot-files-regexp)))
        (when (and (file-directory-p entry)
                (not (member (file-name-nondirectory entry)
                       prowj-discover-skip-directories)))
          (setq files
            (append files
              (prowj-discover--package-json-files
                (file-name-as-directory entry) (1- depth)))))))
    files))

(defun prowj-discover--json-scripts (file)
  "Return the names under \"scripts\" in FILE, or nil if it cannot be read.
A package.json that is being edited is routinely invalid; that is no
reason to fail the command list."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (mapcar (lambda (pair) (format "%s" (car pair)))
          (alist-get 'scripts (json-parse-buffer :object-type 'alist))))
    (error nil)))

(defun prowj-discover-npm-scripts (root)
  "Return the npm scripts declared in and below ROOT."
  (let ((entries nil))
    (dolist (file (prowj-discover--package-json-files root prowj-discover-npm-depth))
      (let* ((directory (file-name-directory file))
              (relative (directory-file-name (file-relative-name directory root))))
        (dolist (script (prowj-discover--json-scripts file))
          (push (list (if (equal relative ".")
                        (format "npm: %s" script)
                        (format "npm[%s]: %s" relative script))
                  (format "npm run %s" script)
                  directory)
            entries))))
    (nreverse entries)))

(defconst prowj-configured-group "Project"
  "The group the entries in `prowj-subcmds' are listed under.")

(put 'prowj-discover-makefile 'prowj-discover-group "Makefile")
(put 'prowj-discover-npm-scripts 'prowj-discover-group "npm")

(defun prowj--project-root ()
  "Return the project root directory, or nil when there is no project."
  (let ((root (ignore-errors (prowj-walkup-and-find-project-root))))
    (and root (not (string-empty-p root)) (file-name-as-directory root))))

(defun prowj--discover-entries (root)
  "Return (GROUP LABEL COMMAND DIRECTORY) for the task files in ROOT.
A discovery function says which group its commands belong to through its
`prowj-discover-group' property, and is listed under its own name
without one."
  (when (and root (not (string-empty-p root)))
    (let ((directory (file-name-as-directory root))
           (entries nil))
      (dolist (function prowj-discover-functions)
        (let ((group (or (get function 'prowj-discover-group)
                       (symbol-name function))))
          (dolist (entry (ignore-errors (funcall function directory)))
            (push (cons group entry) entries))))
      (setq entries (nreverse entries))
      (if prowj-discover-ignore-regexp
        (seq-remove (lambda (entry)
                      (string-match-p prowj-discover-ignore-regexp (nth 1 entry)))
          entries)
        entries))))

(defun prowj-discover-commands (&optional root)
  "Return the commands the task files in ROOT declare.
ROOT defaults to the project root.  Nil when there is no project and
when nothing declares a command, so a project without task files is no
different from how it was before."
  (mapcar #'cdr (prowj--discover-entries (or root (prowj--project-root)))))

(defun prowj--command-entries (&optional root)
  "Return every command this project offers, configured ones first.
Each is (GROUP LABEL COMMAND DIRECTORY).  The entries in `prowj-subcmds'
are the ones written out by hand -- the combinations and the odd jobs no
task file holds -- so they lead, and what the task files declare follows."
  (append
    (mapcar (lambda (entry) (cons prowj-configured-group entry))
      (and (listp prowj-subcmds) prowj-subcmds))
    (prowj--discover-entries (or root (prowj--project-root)))))

(defun prowj-command-list ()
  "Return this project's commands as (LABEL COMMAND DIRECTORY)."
  (mapcar #'cdr (prowj--command-entries)))

;;; Choosing a command

(defface prowj-annotation-command '((t :inherit completions-annotations))
  "Face for the shell command shown beside a label in the command list."
  :group 'prowj)

(defface prowj-annotation-directory '((t :inherit font-lock-constant-face))
  "Face for the directory shown beside a label in the command list.
Coloured rather than dimmed: it is the column that says a command does
not run where the others do, and that is worth seeing at a glance."
  :group 'prowj)

(defcustom prowj-annotation-directory-prefix "▸ "
  "Marker put in front of the directory in the command list.
It gives the few commands that run outside the project root something to
catch the eye.  Use \"\" for none, or a plain \"@ \" where the font has
no arrow."
  :type 'string
  :group 'prowj)

(defcustom prowj-annotation-command-width 48
  "Widest the command column in the command list may grow.
Longer commands are shown cut short, so the directory beside them stays
where the eye expects it."
  :type 'natnum
  :group 'prowj)

(defun prowj--display-directory (directory root)
  "Return DIRECTORY as it is shown in the command list, against ROOT."
  (let ((directory (file-name-as-directory (expand-file-name directory))))
    (if (and root (string-prefix-p root directory))
      (let ((relative (file-relative-name directory root)))
        (if (equal relative "./") "." (directory-file-name relative)))
      (abbreviate-file-name (directory-file-name directory)))))

(defun prowj--unique-label (label directory root taken)
  "Return LABEL, made unique against the labels in TAKEN.
Two commands sharing a label would leave one of them unreachable, so the
directory telling them apart is added to the later one."
  (if (not (member label taken))
    label
    (let ((candidate (format "%s [%s]" label
                       (prowj--display-directory directory root))))
      (if (member candidate taken)
        (let ((n 2))
          (while (member (format "%s<%d>" candidate n) taken)
            (setq n (1+ n)))
          (format "%s<%d>" candidate n))
        candidate))))

(defun prowj--command-alist (entries root)
  "Return an alist of label to ENTRY for ENTRIES, each label unique.
ENTRIES are (GROUP LABEL COMMAND DIRECTORY).  ROOT shortens any
directory added to tell two identical labels apart."
  (let ((taken nil))
    (mapcar
      (lambda (entry)
        (let ((label (prowj--unique-label
                       (nth 1 entry) (nth 3 entry) root taken)))
          (push label taken)
          (cons label entry)))
      entries)))

(defun prowj--completion-table (table root)
  "Return a completion table over TABLE, annotated and grouped.
TABLE is the alist `prowj--command-alist\=' builds.  The label alone is
the candidate, so typing matches it and not the command or the path
behind it; those are shown alongside.  ROOT shortens the directories
under it."
  (let* ((label-width (apply #'max 0 (mapcar (lambda (cell)
                                                (string-width (car cell)))
                                        table)))
          (command-width
            (min prowj-annotation-command-width
              (apply #'max 0 (mapcar (lambda (cell)
                                       (string-width (nth 2 (cdr cell))))
                               table)))))
    (lambda (string predicate action)
      (cond
        ((eq action 'metadata)
          `(metadata
             (category . prowj-command)
             (display-sort-function . identity)
             (cycle-sort-function . identity)
             (group-function
               . ,(lambda (candidate transform)
                    (if transform
                      candidate
                      (nth 0 (cdr (assoc candidate table))))))
             (affixation-function
               . ,(lambda (candidates)
                    (mapcar
                      (lambda (candidate)
                        (let* ((entry (cdr (assoc candidate table)))
                                (command (nth 2 entry))
                                (directory (nth 3 entry)))
                          (list candidate
                            ""
                            (concat
                              (make-string
                                (max 1 (1+ (- label-width (string-width candidate))))
                                ?\s)
                              (propertize
                                (truncate-string-to-width
                                  command command-width nil ?\s t)
                                'face 'prowj-annotation-command)
                              "  "
                              ;; The root is where nearly everything runs, so
                              ;; the column speaks only for the rest.
                              (let ((shown (prowj--display-directory
                                             directory root)))
                                (if (equal shown ".")
                                  ""
                                  (propertize
                                    (concat prowj-annotation-directory-prefix
                                      shown)
                                    'face 'prowj-annotation-directory)))))))
                      candidates)))))
        (t (complete-with-action action table string predicate))))))

;;;###autoload
(defun prowj-subcmd-list ()
  "List and execute sub commands."
  (interactive)
  (let* ((root (prowj--project-root))
          (entries (prowj--command-entries root)))
    (unless entries
      (user-error "No project command is configured, and no task file declares one"))
    (let* ((table (prowj--command-alist entries root))
            (choice (completing-read "Project command: "
                      (prowj--completion-table table root) nil t))
            (entry (cdr (assoc choice table)))
            (command (nth 2 entry))
            (directory (nth 3 entry)))
      (setq prowj-prev-dir directory)
      (setq prowj-prev-cmd command)
      (if prowj-exec-command-frame-enable
        (prowj-exec-command-frame directory command)
        (prowj-exec-command-window directory command)))))

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
