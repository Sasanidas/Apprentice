;;; apprentice-mix.el --- Interface to run Elixir mix tasks inside Emacs

;; Copyright © 2014-2017 Samuel Tonini
;; Copyright © 2022 Fermin MF

;; Author: Samuel Tonini <tonini.samuel@gmail.com
;; Maintainer: Fermin MF <fmfs@posteo.net>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Interface to run Elixir mix tasks inside Emacs.

;;; Code:

(require 'cl-lib)
(require 'apprentice-utils)
(require 'apprentice-project)
(require 'apprentice-test-mode)
(require 'comint)

(defgroup apprentice-mix nil
  "Emacs integration for Elixir's mix."
  :prefix "apprentice-mix-"
  :group 'apprentice)

;; Variables

(defvar apprentice-last-run-test nil)
(defvar apprentice-mix-filter-output nil)
(defvar apprentice-mix-last-task-command nil)

(defcustom apprentice-mix-command "mix"
  "The shell command for mix."
  :type 'string
  :group 'apprentice-mix)

(defcustom apprentice-mix-start-in-umbrella t
  "Start mix command in the umbrella app root or use a subproject."
  :type 'boolean
  :group 'apprentice-mix)

(defcustom apprentice-mix-test-task "test"
  "Default task to run tests."
  :type 'string
  :group 'apprentice-mix)

(defcustom apprentice-mix-test-default-options '()
  "Default options for apprentice test command."
  :type '(repeat string)
  :group 'apprentice-mix)

(defcustom apprentice-mix-env nil
  "The default mix env to run mix commands with.
If nil, the mix env is not set explicitly."
  :type '(string boolean)
  :group 'apprentice-mix)

(defvar apprentice-mix-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    (define-key map "i" #'apprentice-mix-send-input-to-mix-process)
    (define-key map "r" #'apprentice-mix-rerun-last-task)
    map))

(defvar apprentice-mix-buffer-name "*apprentice mix*"
  "Name of the mix output buffer.")

(defvar apprentice-mix--envs '("dev" "prod" "test")
  "The list of mix envs to use as defaults.")

(defconst apprentice-mix-process-name "apprentice-mix-report"
  "Name of the mix process.")

;; Private functions

(defun apprentice-mix--completing-read (prompt cmdlist)
  "Call completion read with PROMPT and CMDLIST."
  (completing-read prompt cmdlist nil t nil nil (car cmdlist)))

(defun apprentice-mix--execute-test (&optional what)
  "Execute \"mix test\" on the given `WHAT'.

`WHAT' could be a filename, a filename:line string or the empty string (meaning
run all tests)."
  (if what
      (setq apprentice-last-run-test what)
    (setq apprentice-last-run-test ""))
  (apprentice-test-execute (list apprentice-mix-command
				 apprentice-mix-test-task
				 what
				 apprentice-mix-test-default-options)))

(defun apprentice-mix--test-file (filename)
  "Run a specific FILENAME as argument for the mix command test."
  (when (not (file-exists-p filename))
    (error "The given file doesn't exist"))
  (apprentice-mix--execute-test (expand-file-name filename)))

;; Public functions

;; (defun apprentice-mix ()
;;   "Prompt for a specific mix task to run.

;; If the command `universal-argument' is called before `apprentice-mix',
;; a prompt for a specific mix environment in which the task will be
;; executed, gets called."
;;   (interactive)
;;   (apprentice-server-info "{ :type, :mixtasks }" #'apprentice-mix-filter))

(defun apprentice-mix-display-mix-buffer ()
  "Display the mix buffer when exists."
  (interactive)
  (when (get-buffer apprentice-mix-buffer-name)
    (display-buffer apprentice-mix-buffer-name)))

(defun apprentice-mix-test ()
  "Run the whole elixir test suite."
  (interactive)
  (apprentice-mix--execute-test))

(defun apprentice-mix-test-stale ()
  "Run stale test (Elixir 1.3+ only)."
  (interactive)
  (if (apprentice-utils-elixir-version-check-p "1.3.0")
      (apprentice-mix--execute-test "--stale")
    (progn (message "Elixir needs to be >= 1.3.0 for mix test --stale")
           (apprentice-mix-test))))

(defun apprentice-mix-test-this-buffer ()
  "Run the current buffer through mix test."
  (interactive)
  (apprentice-mix--test-file buffer-file-name))

(defun apprentice-mix-test-file (filename)
  "Run `apprentice-mix--test-file' with the FILENAME."
  (interactive "Fmix test: ")
  (apprentice-mix--test-file (expand-file-name filename)))

(defun apprentice-mix-test-at-point ()
  "Run the test at point."
  (interactive)
  (let* ((line (line-number-at-pos (point)))
         (file-and-line (format "%s:%s" buffer-file-name line)))
    (apprentice-mix--execute-test file-and-line)))

(defun apprentice-mix-rerun-last-test ()
  "Rerun the last test that was run by apprentice.

When no tests had been run before calling this function, do nothing."
  (interactive)
  (if apprentice-last-run-test
      (apprentice-mix--execute-test apprentice-last-run-test)
    (message "No tests have been run yet")))

(defun apprentice-mix-compile (command &optional prefix)
  "Compile the whole elixir project with COMMAND.
Prompt for the mix env if PREFIX arg is set."
  (interactive "Mmix compile: \nP")
  (apprentice-mix-execute (list "compile" command) prefix))

(defun apprentice-mix-run (command &optional prefix)
  "Run COMMAND on the given file or expression in the context of the application.
Optionally, it accept PREFIX."
  (interactive "Mmix run: \nP")
  (apprentice-mix-execute (list "run" command) prefix))

(defun apprentice-mix-send-input-to-mix-process (input)
  "Send INPUT to the current running mix task process."
  (interactive "MSend to running mix task: ")
  (let* ((buffer (get-buffer apprentice-mix-buffer-name))
         (process (get-buffer-process buffer)))
    (if (and process (eq (process-status process) 'run))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert input "\n\n")
            (set-marker (process-mark process) (point)))
          (comint-send-string process (concat input "\n")))
      (error "No %s process is running" apprentice-mix-buffer-name))))

(defun apprentice-mix-rerun-last-task ()
  "Rerun the last mix task which was run by apprentice.
When no mix task had been run before calling this function, do nothing."
  (interactive)
  (if apprentice-mix-last-task-command
      (apprentice-report-run apprentice-mix-last-task-command
			     apprentice-mix-process-name
			     apprentice-mix-buffer-name 'apprentice-mix-mode)
    (message "No mix task have been run yet")))

(defun apprentice-mix--find-closest-mix-file-dir (path)
  "Find the closest mix file from PATH."
  (let ((root (locate-dominating-file path "mix.exs")))
    (when root
      (file-truename root))))


(defun apprentice-mix--umbrella-apps ()
  "Check if we are in an \"umbrella app\" project."
  (let ((closest-path (locate-dominating-file default-directory "apps")))
    (when closest-path
      (seq-filter
       (lambda (name-path-pair)
	 (let ((path (cdr name-path-pair)))
	   path))
       (cl-loop with potential-umbrella-apps-path = (concat closest-path "/apps")
		with potential-umbrella-apps = (cl-remove-if (lambda (x) (or (string= x "..")
									     (string= x ".")))
							     (directory-files potential-umbrella-apps-path))
		for dir-name in potential-umbrella-apps
		collect (cons dir-name
			      (apprentice-mix--find-closest-mix-file-dir
			       (concat potential-umbrella-apps-path "/" dir-name))))))))

;; (defun apprentice-mix-filter (_process output)
;;   (with-local-quit
;;     (setq apprentice-mix-filter-output (cons output apprentice-mix-filter-output))
;;     (when (apprentice-server-contains-end-marker-p output)
;;       (let* ((output (apprentice-server-prepare-filter-output apprentice-mix-filter-output))
;;              (tasks (split-string output "\n"))
;;              (selected-task (apprentice-mix--completing-read "mix: " (-distinct tasks)))
;;              (command (read-shell-command "mix " (concat selected-task " "))))
;;         (setq apprentice-mix-filter-output nil)
;;         (apprentice-mix-execute (list command) current-prefix-arg)))))

(defun apprentice-mix--filter ()
  "Remove control characters from output."
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

;;TODO: Use `define-compilation-mode'
(define-derived-mode apprentice-mix-mode compilation-mode "Mix"
  "Major mode for presenting Mix tasks.

\\{apprentice-mix-mode-map}"
  (setq buffer-read-only t)
  (setq-local truncate-lines t )
  (add-hook 'compilation-filter-hook #'apprentice-mix--filter))

(defun apprentice-mix-execute (command-list &optional prefix)
  "Run a mix task specified by COMMAND-LIST.

If PREFIX is non-nil, prompt for a mix environment variable."
  (let* ((mix-env (if prefix
                      (completing-read "mix env: " apprentice-mix--envs nil nil apprentice-mix-env)
                    apprentice-mix-env))
         (command (apprentice-utils-build-command
                   (list (when mix-env (concat "MIX_ENV=" mix-env))
                         apprentice-mix-command command-list))))
    (setq apprentice-mix-last-task-command command)
    (apprentice-report-run command apprentice-mix-process-name apprentice-mix-buffer-name 'apprentice-mix-mode)))

(provide 'apprentice-mix)

;;; apprentice-mix.el ends here
