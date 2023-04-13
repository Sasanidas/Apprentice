;;; apprentice-refcard.el --- Generates a refcard of apprentice functionality

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

;; Generates a refcard of apprentice functionality

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'apprentice)
(require 'apprentice-phoenix)

;; Tell the byte compiler about autoloaded functions from packages
(eval-when-compile
  (declare-function apprentice-mode "apprentice.el")
  (declare-function apprentice-version "apprentice.el"))

(defgroup apprentice-refcard nil
  "Generate a refcard of apprentice functionality."
  :prefix "apprentice-"
  :group 'applications)

(defconst apprentice-refcard--buffer-name "*apprentice-refcard*"
  "Name of Apprentice-Refcard mode buffer.")

(defconst apprentice-refcard-list-format
  [("" 55 t)
   ("" 35 t)]
  "List format.")

(defvar apprentice-refcard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'apprentice-refcard--describe-funtion-at-point)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `apprentice-refcard-mode'.")

(defun apprentice-refcard--get-keybinding (function-name)
  (let* ((keys (where-is-internal (intern function-name)))
         (keys (mapcar (lambda (k)
			 (let ((key (format "%s" k)))
			   (if (string-match-p "menu-bar" key)
			       nil
			     k)))
		       keys))
         (keys (cl-remove 'null keys)))
    (if keys
        (progn
          (mapconcat (lambda (k) (key-description k)) keys " , "))
      "")))

(defun apprentice-refcard--tabulated-list-entries ()
  (apprentice-mode +1) ;; needs to be enabled for fetching current keybindings
  (let ((rows (list (apprentice-refcard--build-empty-tabulated-row)
                    (apprentice-refcard--build-tabulated-refcard-title-row (format "Apprentice Refcard v%s" (apprentice-version)))
                    (apprentice-refcard--build-empty-tabulated-row)
                    (apprentice-refcard--build-tabulated-title-row "Mix")
                    (apprentice-refcard--build-tabulated-row "apprentice-mix")
                    (apprentice-refcard--build-tabulated-row "apprentice-mix-compile")
                    (apprentice-refcard--build-tabulated-row "apprentice-mix-run")
                    (apprentice-refcard--build-empty-tabulated-row)
                    (apprentice-refcard--build-tabulated-title-row "Hex")
                    (apprentice-refcard--build-tabulated-row "apprentice-hex-info-at-point")
                    (apprentice-refcard--build-tabulated-row "apprentice-hex-releases-at-point")
                    (apprentice-refcard--build-tabulated-row "apprentice-hex-releases")
                    (apprentice-refcard--build-tabulated-row "apprentice-hex-info")
                    (apprentice-refcard--build-tabulated-row "apprentice-hex-search")
                    (apprentice-refcard--build-empty-tabulated-row)
                    (apprentice-refcard--build-tabulated-title-row "Testing")
                    (apprentice-refcard--build-tabulated-row "apprentice-mix-test")
                    (apprentice-refcard--build-tabulated-row "apprentice-mix-rerun-last-test")
                    (apprentice-refcard--build-tabulated-row "apprentice-mix-test-file")
                    (apprentice-refcard--build-tabulated-row "apprentice-mix-test-this-buffer")
                    (apprentice-refcard--build-tabulated-row "apprentice-mix-test-at-point")
                    (apprentice-refcard--build-tabulated-row "apprentice-mix-test-stale")
                    (apprentice-refcard--build-tabulated-row "apprentice-test-toggle-test-report-display")
                    (apprentice-refcard--build-empty-tabulated-row)
                    (apprentice-refcard--build-tabulated-title-row "Compilation")
                    (apprentice-refcard--build-tabulated-row "apprentice-compile")
                    (apprentice-refcard--build-tabulated-row "apprentice-compile-file")
                    (apprentice-refcard--build-tabulated-row "apprentice-compile-this-buffer")
                    (apprentice-refcard--build-empty-tabulated-row)
                    (apprentice-refcard--build-tabulated-title-row "Execution")
                    (apprentice-refcard--build-tabulated-row "apprentice-execute")
                    (apprentice-refcard--build-tabulated-row "apprentice-execute-file")
                    (apprentice-refcard--build-tabulated-row "apprentice-execute-this-buffer")
                    (apprentice-refcard--build-empty-tabulated-row)
                    (apprentice-refcard--build-tabulated-title-row "Documentation Lookup")
                    (apprentice-refcard--build-tabulated-row "apprentice-help")
                    (apprentice-refcard--build-tabulated-row "apprentice-help-history")
                    (apprentice-refcard--build-tabulated-row "apprentice-help-search-at-point")
                    (apprentice-refcard--build-tabulated-row "apprentice-refcard")
                    (apprentice-refcard--build-empty-tabulated-row)
                    (apprentice-refcard--build-tabulated-title-row "Definition Lookup")
                    (apprentice-refcard--build-tabulated-row "apprentice-goto-definition-at-point")
                    (apprentice-refcard--build-tabulated-row "apprentice-goto-jump-back")
                    (apprentice-refcard--build-tabulated-row "apprentice-goto-jump-to-previous-def-symbol")
                    (apprentice-refcard--build-tabulated-row "apprentice-goto-jump-to-next-def-symbol")
                    (apprentice-refcard--build-tabulated-row "apprentice-goto-list-symbol-definitions")
                    (apprentice-refcard--build-empty-tabulated-row)
                    (apprentice-refcard--build-tabulated-title-row "Project")
                    (apprentice-refcard--build-tabulated-row "apprentice-project-find-test")
                    (apprentice-refcard--build-tabulated-row "apprentice-project-toggle-file-and-tests")
                    (apprentice-refcard--build-tabulated-row "apprentice-project-toggle-file-and-tests-other-window")
                    (apprentice-refcard--build-tabulated-row "apprentice-project-run-tests-for-current-file")
                    (apprentice-refcard--build-empty-tabulated-row)
                    (apprentice-refcard--build-tabulated-title-row "IEx")
                    (apprentice-refcard--build-tabulated-row "apprentice-iex-run")
                    (apprentice-refcard--build-tabulated-row "apprentice-iex-project-run")
                    (apprentice-refcard--build-tabulated-row "apprentice-iex-send-current-line")
                    (apprentice-refcard--build-tabulated-row "apprentice-iex-send-current-line-and-go")
                    (apprentice-refcard--build-tabulated-row "apprentice-iex-send-region")
                    (apprentice-refcard--build-tabulated-row "apprentice-iex-send-region-and-go")
                    (apprentice-refcard--build-tabulated-row "apprentice-iex-compile-this-buffer")
                    (apprentice-refcard--build-empty-tabulated-row)
                    ;; (apprentice-refcard--build-tabulated-title-row "Eval")
                    ;; (apprentice-refcard--build-tabulated-row "apprentice-eval-current-line")
                    ;; (apprentice-refcard--build-tabulated-row "apprentice-eval-print-current-line")
                    ;; (apprentice-refcard--build-tabulated-row "apprentice-eval-quoted-current-line")
                    ;; (apprentice-refcard--build-tabulated-row "apprentice-eval-print-quoted-current-line")
                    ;; (apprentice-refcard--build-tabulated-row "apprentice-eval-region")
                    ;; (apprentice-refcard--build-tabulated-row "apprentice-eval-print-region")
                    ;; (apprentice-refcard--build-tabulated-row "apprentice-eval-quoted-region")
                    ;; (apprentice-refcard--build-tabulated-row "apprentice-eval-print-quoted-region")
                    ;; (apprentice-refcard--build-tabulated-row "apprentice-eval-buffer")
                    ;; (apprentice-refcard--build-tabulated-row "apprentice-eval-print-buffer")
                    ;; (apprentice-refcard--build-tabulated-row "apprentice-eval-quoted-buffer")
                    ;; (apprentice-refcard--build-tabulated-row "apprentice-eval-print-quoted-buffer")
                    ;; (apprentice-refcard--build-tabulated-row "apprentice-eval-close-popup")
                    ;; (apprentice-refcard--build-empty-tabulated-row)
                    (apprentice-refcard--build-tabulated-title-row "Macroexpand")
                    (apprentice-refcard--build-tabulated-row "apprentice-macroexpand-once-current-line")
                    (apprentice-refcard--build-tabulated-row "apprentice-macroexpand-once-print-current-line")
                    (apprentice-refcard--build-tabulated-row "apprentice-macroexpand-current-line")
                    (apprentice-refcard--build-tabulated-row "apprentice-macroexpand-print-current-line")
                    (apprentice-refcard--build-tabulated-row "apprentice-macroexpand-once-region")
                    (apprentice-refcard--build-tabulated-row "apprentice-macroexpand-once-print-region")
                    (apprentice-refcard--build-tabulated-row "apprentice-macroexpand-region")
                    (apprentice-refcard--build-tabulated-row "apprentice-macroexpand-print-region")
                    (apprentice-refcard--build-tabulated-row "apprentice-macroexpand-close-popup")
                    (apprentice-phoenix-mode) ;; needs to be enabled for fetching current keybindings
                    (apprentice-refcard--build-tabulated-title-row "Phoenix-Mode")
                    (apprentice-refcard--build-tabulated-row "apprentice-phoenix-find-web")
                    (apprentice-refcard--build-tabulated-row "apprentice-phoenix-find-views")
                    (apprentice-refcard--build-tabulated-row "apprentice-phoenix-find-controllers")
                    (apprentice-refcard--build-tabulated-row "apprentice-phoenix-find-channels")
                    (apprentice-refcard--build-tabulated-row "apprentice-phoenix-find-templates")
                    (apprentice-refcard--build-tabulated-row "apprentice-phoenix-find-models")
                    (apprentice-refcard--build-tabulated-row "apprentice-phoenix-find-static")
                    (apprentice-refcard--build-tabulated-row "apprentice-phoenix-find-router")
                    (apprentice-refcard--build-tabulated-row "apprentice-phoenix-find-routes")
                    (apprentice-refcard--build-empty-tabulated-row))))
    ;; disable it after getting the current keybindings
    (apprentice-phoenix-mode -1)
    (apprentice-mode -1)
    rows))

(defun apprentice-refcard--build-empty-tabulated-row ()
  (list "" `[,"" ""]))

(defun apprentice-refcard--build-tabulated-row (function-name)
  (list function-name `[,function-name
                        ,(propertize (apprentice-refcard--get-keybinding function-name) 'face font-lock-builtin-face)]))

(defun apprentice-refcard--build-tabulated-refcard-title-row (title)
  (list "" `[,(propertize title 'face font-lock-variable-name-face) ""]))

(defun apprentice-refcard--build-tabulated-title-row (title)
  (list "" `[,(propertize title 'face font-lock-constant-face) ""]))

(defun apprentice-refcard--describe-funtion-at-point ()
  (interactive)
  (let ((function-name (get-text-property (point) 'tabulated-list-id)))
    (when (not
	   (or (null function-name)
	       (string= "" function-name)))
      (describe-function (intern function-name)))))

(defun apprentice-refcard--buffer ()
  "Return `apprentice-refcard' buffer if it exists."
  (get-buffer apprentice-refcard--buffer-name))

(define-derived-mode apprentice-refcard-mode tabulated-list-mode "Apprentice"
  "Apprentice refcard mode."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t
	mode-name "Apprentice-Refcard")
  (setq-local apprentice-test-status-modeline nil)
  (use-local-map apprentice-refcard-mode-map)
  (setq tabulated-list-format apprentice-refcard-list-format
	tabulated-list-entries 'apprentice-refcard--tabulated-list-entries)
  (tabulated-list-print))

;;;###autoload
(defun apprentice-refcard ()
  "Generate an Apprentice refcard of all the features."
  (interactive)
  (let ((buffer-p (apprentice-refcard--buffer))
        (buffer (get-buffer-create apprentice-refcard--buffer-name)))
    (pop-to-buffer buffer)
    (unless buffer-p
      (apprentice-refcard-mode))))

(provide 'apprentice-refcard)

;;; apprentice-refcard.el ends here
