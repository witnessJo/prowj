;;; -*- lexical-binding: t -*-
;; Run with: emacs -Q --batch -l tests/prowj-test.el
;;                         -f ert-run-tests-batch-and-exit
(require 'package)
(package-initialize)
(setq load-prefer-newer t)
(add-to-list 'load-path
             (file-name-directory (directory-file-name
                                   (file-name-directory load-file-name))))
(require 'prowj)
(when (getenv "PROWJ_TEST_FANCY")
  (require 'fancy-compilation)
  (fancy-compilation-mode 1))
(require 'prowj-runner)
(require 'ert)

(ert-deftest prowj-runner-test-case-insensitive-highlights ()
  (with-temp-buffer
    (prowj-run-mode)
    (prowj-runner-insert-output
     "error: warning success TraceBack eXcEpTiOn\ninfo: ready timeOut denied Retry ECONNREFUSED\nunsuccessful\n")
    (font-lock-ensure)
    (dolist (word '("error" "warning" "success" "TraceBack" "eXcEpTiOn"
                    "ready" "timeOut" "denied" "Retry" "ECONNREFUSED"))
      (goto-char (point-min))
      (search-forward word)
      (should (memq 'prowj-runner-important
                    (ensure-list (get-text-property (1- (point)) 'face)))))
    (goto-char (point-min))
    (search-forward "info")
    (should (memq 'prowj-runner-info
                  (ensure-list (get-text-property (1- (point)) 'face))))
    (search-forward "unsuccessful")
    (should-not (get-text-property (1- (point)) 'face))))

(ert-deftest prowj-test-global-mode-preserves-underlying-bindings ()
  (let ((original-map (current-global-map))
        (original-mode prowj-mode)
        (test-map (make-sparse-keymap)))
    (unwind-protect
        (with-temp-buffer
          (use-global-map test-map)
          (define-key test-map (kbd "C-c c l") #'ignore)
          (prowj-mode -1)
          (should (eq (key-binding (kbd "C-c c l")) #'ignore))
          (prowj-mode 1)
          (should (eq (key-binding (kbd "C-c c l")) #'prowj-subcmd-list))
          (should (eq (lookup-key test-map (kbd "C-c c l")) #'ignore))
          (prowj-mode -1)
          (should (eq (key-binding (kbd "C-c c l")) #'ignore)))
      (use-global-map original-map)
      (prowj-mode (if original-mode 1 -1)))))

(defun prowj-runner-test-wait (buffer)
  (let ((process (get-buffer-process buffer))
        (deadline (+ (float-time) 15)))
    (while (and process (process-live-p process) (< (float-time) deadline))
      (accept-process-output process 0.05))
    (when (and process (process-live-p process)) (error "Process timeout"))
    (accept-process-output nil 0.05)
    process))

(ert-deftest prowj-runner-test-output-archive-and-bound ()
  (let* ((directory (make-temp-file "prowj-runner-check-" t))
         (prowj-runner-log-directory (expand-file-name "logs/" directory))
         (prowj-runner-buffer-limit 8192)
         (prowj-runner-line-limit 2000)
         (producer (expand-file-name "producer.py" directory))
         (source (expand-file-name "sample.py" directory))
         buffer)
    (unwind-protect
        (progn
          (with-temp-file source (insert "one\ntwo\nthree\n"))
          (with-temp-file producer
            (insert "import sys,time\n"
                    "sys.stdout.write('\\x1b[');sys.stdout.flush();time.sleep(.02)\n"
                    "sys.stdout.write('31mERROR: red\\x1b[0m\\n');sys.stdout.flush()\n"
                    "print('x'*120000)\n"
                    "for i in range(3000): print(f'INFO: line {i} 한글')\n"
                    "print('sample.py:3: error: sample failure')\n"))
          (cl-letf (((symbol-function 'projectile-save-project-buffers) #'ignore)
                    ((symbol-function 'prowj-reload-dir-locals) #'ignore))
            (setq buffer
                  (prowj--exec-command directory
                    (concat "python3 -u " (shell-quote-argument producer)))))
          (let ((process (prowj-runner-test-wait buffer)))
            (should (eq (process-status process) 'exit))
            (should (= (process-exit-status process) 0)))
          (with-current-buffer buffer
            (should (eq major-mode 'prowj-run-mode))
            (should-not (derived-mode-p 'compilation-mode))
            (should-not (memq (get-buffer-process buffer) compilation-in-progress))
            (should (<= (buffer-size) prowj-runner-buffer-limit))
            (should (> prowj-runner-trimmed-characters 0))
            (should-not bidi-display-reordering)
            (should (file-exists-p prowj-runner-log-file))
            (let ((archive prowj-runner-log-file))
              (with-temp-buffer
                (insert-file-contents archive)
                (should (search-forward "ERROR: red" nil t))
                (should (search-forward (make-string 120000 ?x) nil t))
                (should (search-forward "line 2999 한글" nil t))
                (should-not (string-match-p "\e" (buffer-string)))))
            (goto-char (point-min))
            (should (search-forward "sample.py:3:" nil t))
            (should (text-property-not-all (line-beginning-position)
                                          (line-end-position) 'prowj-runner-source nil))
            (font-lock-ensure)
            (goto-char (point-min))
            (should (search-forward "INFO:" nil t))
            (should (memq 'prowj-runner-info
                          (ensure-list (get-text-property (- (point) 2) 'face))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest prowj-runner-test-fold-and-failure-status ()
  (let* ((directory (make-temp-file "prowj-runner-failure-" t))
         (prowj-runner-log-directory directory)
         buffer)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'projectile-save-project-buffers) #'ignore)
                    ((symbol-function 'prowj-reload-dir-locals) #'ignore))
            (setq buffer
                  (prowj--exec-command directory
                    "python3 -c 'print(\"x\"*120000);raise SystemExit(7)'")))
          (let ((process (prowj-runner-test-wait buffer)))
            (should (= (process-exit-status process) 7)))
          (with-current-buffer buffer
            (should (eq major-mode 'prowj-run-mode))
            (should (text-property-not-all (point-min) (point-max) 'button nil))
            (should (< (buffer-size) 4096))
            (should (equal prowj-runner-status "Failed (7)"))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest prowj-runner-test-ansi-boundaries ()
  (with-temp-buffer
    (let* ((file (make-temp-file "prowj-runner-ansi-"))
           (prowj-runner-log-file file)
           (process (make-pipe-process :name "prowj-runner-ansi" :buffer (current-buffer)))
           (prowj-runner-buffer-limit 8192))
      (unwind-protect
          (progn
            (process-put process 'prowj-runner-original-filter
                         (lambda (_proc output) (insert output)))
            (prowj-runner-filter process "\e[")
            (prowj-runner-filter process "31mERROR: red")
            (prowj-runner-filter process "\e[0m\n")
            (should (equal (buffer-string) "ERROR: red\n"))
            (should (get-text-property (point-min) 'font-lock-face)))
        (delete-process process)
        (delete-file file)))))

(ert-deftest prowj-runner-test-rerun ()
  (let* ((directory (make-temp-file "prowj-runner-rerun-" t))
         (prowj-runner-log-directory directory)
         (command "python3 -u -c 'print(\"ready\");import time;time.sleep(.15)'")
         buffer first-process first-log)
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-save-project-buffers) #'ignore)
                  ((symbol-function 'prowj-reload-dir-locals) #'ignore))
          (setq buffer (prowj--exec-command directory command)
                first-process (get-buffer-process buffer)
                first-log (buffer-local-value 'prowj-runner-log-file buffer))
          (should (eq buffer (prowj--exec-command directory command)))
          (should-not (process-live-p first-process))
          (should-not (equal first-log (buffer-local-value 'prowj-runner-log-file buffer)))
          (prowj-runner-test-wait buffer)
          (with-current-buffer buffer
            (let ((previous-log prowj-runner-log-file))
              (prowj-runner-rerun)
              (should-not (equal previous-log prowj-runner-log-file))
              (should (eq (process-filter (get-buffer-process buffer)) #'prowj-runner-filter))))
          (prowj-runner-test-wait buffer)
          (with-current-buffer buffer
            (should (eq major-mode 'prowj-run-mode))))
      (when (buffer-live-p buffer)
        (when-let* ((process (get-buffer-process buffer))) (delete-process process))
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest prowj-runner-test-streamed-giant-line ()
  (let* ((directory (make-temp-file "prowj-runner-giant-" t))
         (prowj-runner-log-directory directory)
         (prowj-runner-buffer-limit 8192)
         buffer)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'projectile-save-project-buffers) #'ignore)
                    ((symbol-function 'prowj-reload-dir-locals) #'ignore))
            (setq buffer
                  (prowj--exec-command directory
                    "python3 -u -c 'import sys,time;[(sys.stdout.write(\"z\"*16000),sys.stdout.flush(),time.sleep(.01)) for _ in range(30)]'")))
          (prowj-runner-test-wait buffer)
          (with-current-buffer buffer
            (should (<= (buffer-size) prowj-runner-buffer-limit))
            (should (< (buffer-size) 4096))
            (let ((archive prowj-runner-log-file))
              (with-temp-buffer
                (insert-file-contents archive)
                (should (search-forward (make-string 480000 ?z) nil t))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest prowj-runner-test-archive-failure-keeps-output ()
  (with-temp-buffer
    (let* ((prowj-runner-log-file "/no-such-wj-directory/output.log")
           (process (make-pipe-process :name "prowj-runner-archive-error"
                                       :buffer (current-buffer))))
      (unwind-protect
          (progn
            (process-put process 'prowj-runner-original-filter
                         (lambda (_proc output) (insert output)))
            (prowj-runner-filter process "ERROR: still visible\n")
            (should prowj-runner-log-error)
            (should (equal (buffer-string) "ERROR: still visible\n")))
        (delete-process process)))))

(ert-deftest prowj-runner-test-preview-chunk-boundaries-and-buttons ()
  (with-temp-buffer
    (let* ((prowj-runner-log-file "/tmp/prowj-runner-preview.log")
           (prowj-runner-line-limit 4)
           (output (concat (prowj-runner-preview "abc")
                           (prowj-runner-preview "defgh")
                           (prowj-runner-preview "ijk\nOK\n1234")
                           (prowj-runner-preview "5\nend"))))
      (insert output)
      (goto-char (point-min))
      (should (search-forward "abcd … [full log]\nOK\n1234 … [full log]\nend" nil t))
      (let* ((first (next-button (point-min)))
             (second (next-button (button-end first))))
        (should (= (button-get first 'prowj-runner-line) 1))
        (should (= (button-get second 'prowj-runner-line) 3))
        (should (equal (button-get first 'prowj-runner-file) prowj-runner-log-file))))))

(ert-deftest prowj-runner-test-terminal-colors-and-emphasis ()
  (with-temp-buffer
    (prowj-run-mode)
    (let ((process (make-pipe-process :name "prowj-runner-colors" :buffer (current-buffer))))
      (unwind-protect
          (progn
            (process-put process 'prowj-runner-original-filter
                         (lambda (_proc output)
                           (let ((inhibit-read-only t)) (insert output))))
            (prowj-runner-filter process "\e[38;2;17;34;51mINFO: SUCCESS\e[0m\n")
            (font-lock-ensure)
            (should (equal (plist-get (get-text-property (point-min) 'font-lock-face)
                                     :foreground)
                           "#112233"))
            (goto-char (point-min))
            (search-forward "SUCCESS")
            (should (memq 'prowj-runner-important
                          (ensure-list (get-text-property (- (point) 2) 'face))))
            (dolist (face '(prowj-runner-error prowj-runner-warning prowj-runner-info
                            prowj-runner-debug prowj-runner-important prowj-runner-location))
              (should (eq (face-attribute face :foreground) 'unspecified))
              (should (eq (face-attribute face :background) 'unspecified)))
            (when (featurep 'ghostel-faces)
              (should (equal (assq 'default face-remapping-alist)
                             '(default ghostel-default default)))
              (should (eq (aref ansi-color-normal-colors-vector 1)
                          (aref ghostel-color-palette 1)))))
        (delete-process process)))))



(ert-deftest prowj-runner-test-package-dependencies ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "prowj.el" (file-name-directory (locate-library "prowj"))))
    (let ((description (package-buffer-info)))
      (should (equal (package-desc-version description) '(0 2 0)))
      (should (equal (cdr (assq 'emacs (package-desc-reqs description))) '((29 1))))
      (should (equal (cdr (assq 'projectile (package-desc-reqs description))) '((2 8 0))))
      (should-not (assq 'ghostel (package-desc-reqs description)))
      (should-not (assq 'fancy-compilation (package-desc-reqs description))))))

(ert-deftest prowj-runner-test-directory-after-window-switch ()
  (let* ((directory (make-temp-file "prowj-cwd-" t))
         (prowj-runner-log-directory (expand-file-name "logs/" directory))
         (other (generate-new-buffer " *prowj-other-directory*"))
         buffer)
    (unwind-protect
        (progn
          (with-current-buffer other (setq default-directory "/tmp/"))
          (save-window-excursion
            (cl-letf (((symbol-function 'projectile-save-project-buffers) #'ignore)
                      ((symbol-function 'prowj-reload-dir-locals) #'ignore)
                      ((symbol-function 'prowj-focus-log-window)
                       (lambda () (switch-to-buffer other))))
              (setq buffer (prowj--exec-command directory "pwd" t))))
          (should (= (process-exit-status (prowj-runner-test-wait buffer)) 0))
          (with-current-buffer buffer
            (should (equal default-directory (file-name-as-directory directory)))
            (should (equal prowj-runner-command "pwd"))
            (with-temp-buffer
              (insert-file-contents (buffer-local-value 'prowj-runner-log-file buffer))
              (should (string-prefix-p (concat directory "\n") (buffer-string))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (kill-buffer other)
      (delete-directory directory t))))

(ert-deftest prowj-runner-test-status-and-independent-process ()
  (let* ((directory (make-temp-file "prowj-status-" t))
         (prowj-runner-log-directory directory)
         (original compilation-in-progress)
         buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-save-project-buffers) #'ignore)
                  ((symbol-function 'prowj-reload-dir-locals) #'ignore))
          (setq buffer (prowj--exec-command directory "sleep 0.15; printf done"))
          (with-current-buffer buffer
            (should (derived-mode-p 'special-mode))
            (should-not (derived-mode-p 'compilation-mode))
            (should (equal prowj-runner-status "Running"))
            (should buffer-read-only))
          (should (equal compilation-in-progress original))
          (prowj-runner-test-wait buffer)
          (with-current-buffer buffer
            (should (equal prowj-runner-status "Finished"))
            (should (string-match-p "done" (buffer-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest prowj-runner-test-source-links-and-browsing-position ()
  (with-temp-buffer
    (prowj-run-mode)
    (prowj-runner-insert-output "sample.py:3:4: error: example\n")
    (should (= (point) (point-max)))
    (goto-char (point-min))
    (let ((position (point)))
      (prowj-runner-insert-output "  File \"other.py\", line 8\n")
      (should (= (point) position)))
    (let* ((first (next-button (point-min) t))
           (second (next-button (button-end first))))
      (should (equal (button-get first 'prowj-runner-source) "sample.py"))
      (should (= (button-get first 'prowj-runner-source-line) 3))
      (should (= (button-get first 'prowj-runner-source-column) 4))
      (should (equal (button-get second 'prowj-runner-source) "other.py"))
      (should (= (button-get second 'prowj-runner-source-line) 8))
      (cl-letf (((symbol-function 'prowj-runner-open-source)
                 (lambda (button)
                   (should (equal (button-get button 'prowj-runner-source) "sample.py")))))
        (prowj-runner-next-error 1 nil)))))

(ert-deftest prowj-runner-test-tab-moves-between-links ()
  (with-temp-buffer
    (prowj-run-mode)
    (prowj-runner-insert-output "running\na.py:1:1: error\nb.py:2:1: error\n")
    (goto-char (point-min))
    (let ((first (next-button (point-min) t)))
      (call-interactively (lookup-key prowj-run-mode-map (kbd "TAB")))
      (should (equal (button-get (button-at (point)) 'prowj-runner-source) "a.py"))
      (call-interactively (lookup-key prowj-run-mode-map (kbd "TAB")))
      (should (equal (button-get (button-at (point)) 'prowj-runner-source) "b.py"))
      (call-interactively (lookup-key prowj-run-mode-map (kbd "<backtab>")))
      (should (= (point) (button-start first))))))

(ert-deftest prowj-runner-test-source-location-split-across-chunks ()
  (with-temp-buffer
    (prowj-run-mode)
    (prowj-runner-insert-output "sample.py:3")
    (prowj-runner-insert-output ":14: error\n")
    (let ((button (next-button (point-min) t)))
      (should (= (button-get button 'prowj-runner-source-column) 14)))))

(ert-deftest prowj-runner-test-stop ()
  (let* ((directory (make-temp-file "prowj-stop-" t))
         (prowj-runner-log-directory directory)
         buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-save-project-buffers) #'ignore)
                  ((symbol-function 'prowj-reload-dir-locals) #'ignore))
          (setq buffer (prowj--exec-command directory
                         "python3 -u -c 'print(\"ready\");import time;time.sleep(10)'"))
          (let ((process (get-buffer-process buffer)))
            (accept-process-output process .1)
            (with-current-buffer buffer (prowj-runner-stop))
            (prowj-runner-test-wait buffer)
            (should-not (process-live-p process))
            (with-current-buffer buffer
              (should-not (equal prowj-runner-status "Running")))))
      (when (buffer-live-p buffer)
        (when-let* ((process (get-buffer-process buffer))) (delete-process process))
        (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest prowj-runner-test-project-command-counts ()
  (let* ((directory (make-temp-file "prowj-counts-" t))
         (root (expand-file-name "project/" directory))
         (subdir (expand-file-name "child/" root))
         (other (expand-file-name "other/" directory))
         (prowj-runner-log-directory directory)
         (prowj-runner-processes nil)
         (prowj-runner-project-indicators (make-hash-table :test #'equal))
         buffers)
    (unwind-protect
        (cl-letf (((symbol-function 'projectile-save-project-buffers) #'ignore)
                  ((symbol-function 'prowj-reload-dir-locals) #'ignore)
                  ((symbol-function 'projectile-project-root)
                   (lambda () (if (string-prefix-p root default-directory) root other))))
          (make-directory subdir t)
          (make-directory other t)
          (push (prowj--exec-command root "sleep 10") buffers)
          (push (prowj--exec-command subdir "sleep 11") buffers)
          (push (prowj--exec-command other "sleep 12") buffers)
          (should (equal (gethash root prowj-runner-project-indicators) "Run:2"))
          (should (equal (gethash other prowj-runner-project-indicators) "Run:1"))
          (let ((tooltip (get-text-property 0 'help-echo
                                            (gethash root prowj-runner-project-indicators))))
            (should (string-match-p "sleep 10" tooltip))
            (should (string-match-p "sleep 11" tooltip))
            (should-not (string-match-p "sleep 12" tooltip)))
          (with-current-buffer (cadr buffers)
            (should (equal (prowj-runner-mode-line) " Run:2"))
            (cl-letf (((symbol-function 'projectile-project-root)
                       (lambda () (ert-fail "Project lookup during redisplay"))))
              (dotimes (_ 10) (prowj-runner-mode-line)))
            (delete-process (get-buffer-process (current-buffer)))
            (should (equal (gethash root prowj-runner-project-indicators) "Run:1"))
            (should (equal (prowj-runner-mode-line) " Stopped / Run:1")))
          (let ((process (get-buffer-process (car (last buffers)))))
            ;; Even a process detached from its log must decrement the count.
            (set-process-buffer process nil)
            (delete-process process)
            (should-not (gethash root prowj-runner-project-indicators)))
          (should (equal (gethash other prowj-runner-project-indicators) "Run:1")))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (when-let* ((process (get-buffer-process buffer))) (delete-process process))
          (kill-buffer buffer)))
      (delete-directory directory t))))
