;;; apprentice-test-mode.el --- Minor mode for Elixir test files.

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

;; Minor mode for Elixir test files.

;;; Code:

(require 'apprentice-project)

(defgroup apprentice-test-mode nil
  "Minor mode for Elixir ExUnit files."
  :prefix "apprentice-test-mode-"
  :group 'apprentice)

;; Variables

(defcustom apprentice-test-mode-highlight-tests t
  "Non-nil means that specific functions for testing will
be highlighted with more significant font faces."
  :type 'boolean
  :group 'apprentice-test-mode)

(defcustom apprentice-test-display-compilation-output nil
  "if Non-nil, compilation informations will be displayed
in the test report buffer."
  :type 'boolean
  :group 'apprentice-test-mode)

(defcustom apprentice-test-truncate-lines t
  "The value of this variable is used to set the value of
truncate-lines in the test report window."
  :type 'boolean
  :group 'apprentice-test-mode)

(defcustom apprentice-test-status-modeline t
  "if Non-nil, the face of local `mode-name' variable will change with test run status.

For example, when `apprentice-mix-test' fails, the `mode-name' will be
formatted with the `apprentice-test--failed-face' face, to symbolize failing tests."
  :type 'boolean
  :group 'apprentice-test)

(defcustom apprentice-test-ask-about-save t
  "Non-nil means 'apprentice-test-excute` asks which buffers to save before running.
Otherwise, it saves all modified buffers without asking."
  :type 'boolean
  :group 'apprentice-test)

(defvar apprentice-test--last-run-status "")

(defconst apprentice-test-report-buffer-name "*apprentice test report*"
  "Name of the test report buffer.")

(defconst apprentice-test-report-process-name "apprentice-test-process"
  "Name of the test report process.")

(defconst apprentice-test--failing-files-regex
  "\\( *[0-9]+).+\n\s+\\)\\([-A-Za-z0-9./_]+:[0-9]+\\)$")
(defconst apprentice-test--stacktrace-files-regex
  "\\( *\\)\\([-A-Za-z0-9./_]+:[0-9]+\\): (test)")

;; Faces

(defface apprentice-test--test-file-and-location-face
  '((t (:inherit font-lock-variable-name-face :weight bold)))
  "Face for the file where the failed test are."
  :group 'apprentice-test)

(defface apprentice-test--stacktrace-file-and-location-face
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for the stacktrace files."
  :group 'apprentice-test)

(defface apprentice-test--success-face
  '((t (:inherit font-lock-variable-name-face :bold t :background "darkgreen" :foreground "white")))
  "Face for successful compilation run."
  :group 'apprentice-test)

(defface apprentice-test--failed-face
  '((t (:inherit font-lock-variable-name-face :bold t :background "red" :foreground "white")))
  "Face for failed compilation run."
  :group 'apprentice-test)

(defvar apprentice-test--mode-name-face 'mode-line)

(defvar apprentice-test-at-point #'apprentice-mix-test-at-point)
(defvar apprentice-test-this-buffer #'apprentice-mix-test-this-buffer)
(defvar apprentice-test #'apprentice-mix-test)
(defvar apprentice-test-file #'apprentice-mix-test-file)
(defvar apprentice-test-jump-to-previous-test #'apprentice-test-mode-jump-to-previous-test)
(defvar apprentice-test-jump-to-next-test #'apprentice-test-mode-jump-to-next-test)
(defvar apprentice-test-list-tests #'apprentice-test-mode-list-tests)

(defvar apprentice-test-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    (define-key map "t" #'toggle-truncate-lines)
    (define-key map "r" #'apprentice-mix-rerun-last-test)
    (define-key map (kbd "M-n") #'apprentice-test-next-result)
    (define-key map (kbd "M-p") #'apprentice-test-previous-result)
    (define-key map (kbd "M-N") #'apprentice-test-next-stacktrace-file)
    (define-key map (kbd "M-P") #'apprentice-test-previous-stacktrace-file)
    (define-key map (kbd "C-c C-k") #'apprentice-report-interrupt-current-process)
    map))

(defvar apprentice-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c , s") apprentice-test-at-point)
    (define-key map (kbd "C-c , v") apprentice-test-this-buffer)
    (define-key map (kbd "C-c , a") apprentice-test)
    (define-key map (kbd "C-c , f") apprentice-test-file)
    (define-key map (kbd "C-c , p") apprentice-test-jump-to-previous-test)
    (define-key map (kbd "C-c , n") apprentice-test-jump-to-next-test)
    (define-key map (kbd "C-c , l") apprentice-test-list-tests)
    map)
  "Keymap for `apprentice-test-mode'.")

(defconst apprentice-test-mode--test-regex
  (let ((whitespace-opt "[[:space:]]*")
        (whitespace "[[:space:]]+"))
    (concat "\\(^" whitespace-opt "test" whitespace "\\(?10:.+\\)" whitespace "do" whitespace-opt "$"
            "\\|"
            whitespace " [0-9]+) test .+\\)")))

;; Private functions

(defun apprentice-test--set-modeline-color (status)
  (setq apprentice-test--mode-name-face
        (if (string-prefix-p "finished" status)
            'apprentice-test--success-face
          'apprentice-test--failed-face)))

(defun apprentice-test--render-report (buffer)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (apprentice-test--render-files))))

(defun apprentice-test--render-files ()
  (apprentice-test--render-test-failing-files)
  (apprentice-test--render-stacktrace-files))

(defun apprentice-test--render-test-failing-files ()
  (apprentice-test--render-file apprentice-test--failing-files-regex
                               'apprentice-test--test-file-and-location-face))

(defun apprentice-test--render-stacktrace-files ()
  (apprentice-test--render-file apprentice-test--stacktrace-files-regex
                               'apprentice-test--stacktrace-file-and-location-face))

(defun apprentice-test--render-file (regex face)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (let ((file (buffer-substring-no-properties (match-beginning 2) (match-end 2))))
        (goto-char (match-beginning 2))
        (replace-match "" nil nil nil 2)
        (insert-text-button file
                            'face face
                            'file file
                            'follow-link t
                            'action #'apprentice-test--open-file
                            'help-echo "visit the source location")))))

(defun apprentice-test--open-file (button)
  (save-match-data
    (string-match "\\([-A-Za-z0-9./_]+\\):\\([0-9]+\\)" (button-get button 'file))
    (let* ((file-with-line (button-get button 'file))
           (file (substring-no-properties file-with-line (match-beginning 1) (match-end 1)))
           (line (string-to-number (substring-no-properties file-with-line (match-beginning 2) (match-end 2))))
           (file-path (if (file-exists-p file)
                          file
                        (expand-file-name (concat (apprentice-project-root) file)))))
      (with-current-buffer (find-file-other-window file-path)
        (goto-char (point-min))
        (forward-line (- line 1))))))

(defun apprentice-test--handle-exit (status buffer)
  (when apprentice-test-status-modeline
    (apprentice-test--set-modeline-color status))
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (apprentice-test--render-files))))

(defun apprentice-test-mode--buffer-contains-tests-p ()
  "Return nil if the current buffer contains no tests, non-nil if it does."
  (apprentice-utils-occur-in-buffer-p (current-buffer) apprentice-test-mode--test-regex))

(defun apprentice-test-mode--tests-in-buffer ()
  "Return an alist of tests in this buffer.

The keys in the list are the test names (e.g., the string passed to the test/2
macro) while the values are the position at which the test matched."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let ((tests '()))
        (while (re-search-forward apprentice-test-mode--test-regex nil t)
          (let* ((position (car (match-data)))
                 (matched-string (match-string 10)))
            (set-text-properties 0 (length matched-string) nil matched-string)
            (add-to-list 'tests (cons matched-string position) t)))
        tests))))

(defun apprentice-test-mode--highlight-syntax ()
  (if apprentice-test-mode-highlight-tests
      (font-lock-add-keywords nil
		      '(("^\s+\\(test\\)\s+" 1
			 font-lock-variable-name-face t)
			("^\s+\\(assert[_a-z]*\\|refute[_a-z]*\\|flunk\\)\s+" 1
			 font-lock-type-face t)
			("^\s+\\(assert[_a-z]*\\|refute[_a-z]*\\|flunk\\)\(" 1
			 font-lock-type-face t)))))

;; Public functions

(define-derived-mode apprentice-test-report-mode fundamental-mode "Apprentice Test Report"
  "Major mode for presenting Elixir test results.

\\{apprentice-test-report-mode-map}"
  (setq buffer-read-only t)
  (setq-local truncate-lines t
	      electric-indent-chars nil))

(defun apprentice-test-save-buffers ()
  "Save some modified file-visiting buffers."
  (save-some-buffers (not apprentice-test-ask-about-save) nil))

(defun apprentice-test-clean-compilation-output (output)
  (if (not apprentice-test-display-compilation-output)
      (with-temp-buffer
        (insert output)
        (delete-matching-lines "^Compiled .+" (point-min) (point-max))
        (delete-matching-lines "^Generated .+" (point-min) (point-max))
        (buffer-substring-no-properties (point-min) (point-max)))
  output))

(defun apprentice-test-execute (command-list)
  (message "Testing...")
  (let* ((command (mapconcat 'concat (flatten-list command-list) " ")))
    (apprentice-test-save-buffers)
    (apprentice-report-run command
			   apprentice-test-report-process-name
			   apprentice-test-report-buffer-name
			   'apprentice-test-report-mode
			   #'apprentice-test--handle-exit)))

(defun apprentice-test-initialize-modeline ()
  "Initialize the mode-line face."
  (when apprentice-test-status-modeline
    (setq mode-name
          '(:eval (propertize "Elixir" 'face apprentice-test--mode-name-face)))))

(defun apprentice-test-reset-modeline ()
  "Reset the current mode-line face to default."
  (setq mode-name "Elixir"))

(defun apprentice-test-mode-jump-to-next-test ()
  "Jump to the next ExUnit test. If there are no tests after the current
position, jump to the first test in the buffer. Do nothing if there are no tests
in this buffer."
  (interactive)
  (apprentice-utils-jump-to-next-matching-line apprentice-test-mode--test-regex 'back-to-indentation))

(defun apprentice-test-mode-jump-to-previous-test ()
  "Jump to the previous ExUnit test. If there are no tests before the current
position, jump to the last test in the buffer. Do nothing if there are no tests
in this buffer."
  (interactive)
  (apprentice-utils-jump-to-previous-matching-line apprentice-test-mode--test-regex 'back-to-indentation))

(defun apprentice-test-next-result ()
  "Jump to the next error in the test report.

If there are no error after the current position,
jump to the first error in the test report.
Do nothing if there are no error in this test report."
  (interactive)
  (apprentice-utils-jump-to-next-matching-line apprentice-test--failing-files-regex
                                               'back-to-indentation))

(defun apprentice-test-previous-result ()
  "Jump to the previous error in the test report.

If there are no error before the current position,
jump to the first error in the test report.
Do nothing if there are no error in this test report."
  (interactive)
  (apprentice-utils-jump-to-previous-matching-line apprentice-test--failing-files-regex
                                                   #'(lambda ()
                                                       (forward-line 1)
                                                       (back-to-indentation))))

(defun apprentice-test-next-stacktrace-file ()
  "Jump to the next stacktrace file in the test report.

If there are no stacktrace file after the current position,
jump to the first stacktrace file in the test report.
Do nothing if there are no stacktrace file in this test report."
  (interactive)
  (apprentice-utils-jump-to-next-matching-line apprentice-test--stacktrace-files-regex
                                               'back-to-indentation))

(defun apprentice-test-previous-stacktrace-file ()
  "Jump to the previous stacktrace file in the test report.

If there are no stacktrace file before the current position,
jump to the first stacktrace file in the test report.
Do nothing if there are no stacktrace file in this test report."
  (interactive)
  (apprentice-utils-jump-to-previous-matching-line apprentice-test--stacktrace-files-regex
                                                   'back-to-indentation))

(defun apprentice-test-mode-list-tests ()
  "List ExUnit tests (calls to the test/2 macro) in the current buffer and jump
to the selected one."
  (interactive)
  (let* ((tests (apprentice-test-mode--tests-in-buffer))
         (selected (completing-read "Test: " tests))
         (position (cdr (assoc selected tests))))
    (goto-char position)
    (back-to-indentation)))

(defun apprentice-test-toggle-test-report-display ()
  "Toggle between display or hidding `apprentice-test-report-buffer-name' buffer."
  (interactive)
  (let* ((buffer (get-buffer apprentice-test-report-buffer-name))
         (window (get-buffer-window buffer)))
    (if buffer
        (if window
            (quit-window nil window)
          (display-buffer buffer))
      (message "No Apprentice test report buffer exists."))))

;;;###autoload
(define-minor-mode apprentice-test-mode
  "Minor mode for Elixir ExUnit files.

The following commands are available:

\\{apprentice-test-mode-map}"
  :lighter ""
  :keymap apprentice-test-mode-map
  :group 'apprentice
  (when apprentice-test-mode
    (apprentice-test-mode--highlight-syntax)))

;;;###autoload
(defun apprentice-test-enable-mode ()
  (if (apprentice-utils-test-file-p)
      (apprentice-test-mode)))

;;;###autoload
(dolist (hook '(apprentice-mode-hook))
  (add-hook hook 'apprentice-test-enable-mode))

(provide 'apprentice-test-mode)

;;; apprentice-test-mode.el ends here
