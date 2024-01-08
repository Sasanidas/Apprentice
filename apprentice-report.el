;;; apprentice-report.el --- Run command in a process and handles buffer of it

;; Copyright © 2015 Samuel Tonini
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

;; Run command in a process and handles buffer output and display

;;; Code:

(require 'ansi-color)
(require 'apprentice-project)

(defgroup apprentice-report nil
  "Run command in a process and handles buffer output and display."
  :prefix "apprentice-report-"
  :group 'apprentice)

(defvar apprentice-report-on-exit nil)
(defvar apprentice-report-on-exit-function nil)
(defvar apprentice-report-on-render nil)
(defvar apprentice-report-on-render-function nil)
(defvar apprentice-report--last-run-status nil)
(defvar apprentice-report-mode-name nil)

(defun apprentice-report--kill-process (process)
  "Interrupt and kill the running report PROCESS."
  (when process
    (with-current-buffer (process-buffer process)
      (let* ((mode-name (if (stringp mode-name)
                  (replace-regexp-in-string ":.+$" "" mode-name)
                mode-name)))
        (if (or (not (eq (process-status process) 'run))
                (eq (process-query-on-exit-flag process) nil)
                (yes-or-no-p
                 (format "A %s process already running; kill it? "
                         mode-name)))
            (condition-case ()
                (progn
                  (interrupt-process process)
                  (sit-for 1)
                  (delete-process process))
              (error nil))
          (error "Cannot have two processes in `%s' at once"
                 (buffer-name)))))))

(defun apprentice-report--sentinel (process status)
  "Sentinel for test report buffer.
It checks the PROCESS STATUS."
  (if (memq (process-status process) '(exit signal))
      (let ((buffer (process-buffer process)))
        (if (null (buffer-name buffer))
            (set-process-buffer process nil)
          (progn
            (apprentice-report--render-report buffer)
            (apprentice-report--handle-exit status buffer)
            (apprentice-report-update-mode-name process)
            (delete-process process))))))

(defun apprentice-report--render-report (buffer)
  "Call the defined render functions for the BUFFER."
  (when apprentice-report-on-render-function
    (funcall apprentice-report-on-render-function buffer)))

(defun apprentice-report--handle-exit (status buffer)
  "Call the defined exit function specified in `apprentice-report-on-exit-function'.
Argument for the function is the STATUS and BUFFER of the finished process."
  (apprentice-report--store-process-status status)
  (when apprentice-report-on-exit-function
    (funcall apprentice-report-on-exit-function status buffer)))

(defun apprentice-report--store-process-status (status)
  "Store STATUS of the last finished process."
  (setq apprentice-report--last-run-status status))

(defun apprentice-report--last-run-successful-p ()
  "Return non-nil if the last process successfully finished."
  (when (string-prefix-p "finished" apprentice-report--last-run-status) t))

(defun apprentice-report-filter (process output)
  "Process filter for report buffers.
Checking the PROCESS OUTPUT."
  (with-current-buffer (process-buffer process)
    (let* ((buffer-read-only nil)
           (output (if (string= (process-name process) apprentice-test-report-process-name)
                       (apprentice-test-clean-compilation-output output)
                     output))
           (moving (= (point) (process-mark process))))
      (save-excursion
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point))
        (ansi-color-apply-on-region (point-min) (point-max)))
      (if moving (goto-char (process-mark process))))))

(defun apprentice-report-update-mode-name (process)
  "Update the `mode-name' with the status of PROCESS."
  (with-current-buffer (process-buffer process)
    (when (stringp mode-name)
      (setq-local mode-name
		  (format "%s:%s"
			  (replace-regexp-in-string ":.+$" "" mode-name)
			  (process-status process))))))

(defun apprentice-report-interrupt-current-process ()
  "Interrupt the current running report process."
  (interactive)
  (let ((buffer (current-buffer))
        (name (if (stringp mode-name)
                  (replace-regexp-in-string ":.+" "" mode-name)
                mode-name)))
    (if (get-buffer-process buffer)
        (interrupt-process (get-buffer-process buffer))
      (error "The [%s] process is not running" name))))

(defun apprentice-report-cleanup-process-buffer (buffer)
  "Clean the content BUFFER of process.
If there is already a running process, ask for interrupting it."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (process (get-buffer-process buffer)))
      (erase-buffer))))

(defun apprentice-report-display-buffer (buffer)
  "Display the BUFFER."
  (display-buffer buffer))

(defun apprentice-report-activate-mode (mode buffer)
  "Enable MODE inside BUFFER."
  (with-current-buffer buffer
    (funcall mode)
    (setq-local truncate-lines apprentice-test-truncate-lines
		window-point-insertion-type t)))

(defun apprentice-report-run (command process-name buffer-name mode &optional on-exit hidden)
  "Run COMMAND in a new process called PROCESS-NAME.
The output of PROCESS-NAME will be displayed in BUFFER-NAME.
After displaying BUFFER-NAME, the MODE function will be called within.

Optional ON-EXIT and HIDDEN functions could be defined.
The function ON-EXIT will be called when PROCESS-NAME is finished.
The HIDDEN variable defines if PROCESS-NAME should run in the background."
  (let* ((buffer (get-buffer-create buffer-name))
         (default-directory (apprentice-project-root-or-default-dir)))
    (apprentice-report-cleanup-process-buffer buffer)
    (apprentice-report--kill-process (get-buffer-process buffer))
    (start-process-shell-command process-name buffer command)
    (when on-exit
      (setq apprentice-report-on-exit-function on-exit))
    (set-process-sentinel (get-buffer-process buffer) 'apprentice-report--sentinel)
    (set-process-filter (get-buffer-process buffer) 'apprentice-report-filter)
    (apprentice-report-activate-mode mode buffer)
    (if (not hidden)
        (apprentice-report-display-buffer buffer))
    (apprentice-report-update-mode-name (get-buffer-process buffer))))

(provide 'apprentice-report)

;;; apprentice-report.el ends here
