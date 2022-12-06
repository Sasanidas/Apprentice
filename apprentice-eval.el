;;; apprentice-eval.el --- Elixir code inline evaluation functionality

;; Copyright © 2014-2017 Samuel Tonini

;; Author: Samuel Tonini <tonini.samuel@gmail.com

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

;; Elixir code inline evaluation functionality

;;; Code:

(require 'elixir-mode)
(require 'apprentice-interact)

;; (defgroup apprentice-eval nil
;;   "Elixir code inline evaluation functionality."
;;   :prefix "apprentice-eval-"
;;   :group 'apprentice)

;; (defconst apprentice-eval-buffer-name "*apprentice-eval-mode*"
;;   "Name of the Elixir evaluation buffer.")

;; (defvar apprentice-eval-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "q") #'quit-window)
;;     map)
;;   "Keymap for `apprentice-eval-mode'.")

;; (defvar apprentice-eval-filter-output nil)

;; ;; Private functions

;; (defun apprentice-eval--insert (string)
;;   (let ((lines (split-string string "\n")))
;;     (if (> (length lines) 1)
;;         (progn
;;           (save-excursion
;;             (end-of-line)
;;             (mapc (lambda (s)
;;                     (newline)
;;                     (insert (format "# => %s" s))
;;                     (indent-according-to-mode))
;;                   lines)))
;;       (save-excursion
;;         (end-of-line)
;;         (insert (format "  # => %s" string))))))

;; (defun apprentice-eval--expression (expression)
;;   (let ((file (make-temp-file "apprentice-eval" nil ".exs")))
;;     (with-temp-file file
;;       (insert expression))
;;     (apprentice-server-eval (format "{ :eval, '%s' }" file) #'apprentice-eval-filter)))

;; (defun apprentice-eval--expression-and-print (expression)
;;   (let ((file (make-temp-file "apprentice-eval" nil ".exs")))
;;     (with-temp-file file
;;       (insert expression))
;;     (apprentice-server-eval (format "{ :eval, '%s' }" file) #'apprentice-eval-insert-filter)))

;; (defun apprentice-eval--quote-expression (expression)
;;   (let ((file (make-temp-file "apprentice-eval" nil ".exs")))
;;     (with-temp-file file
;;       (insert expression))
;;     (apprentice-server-eval (format "{ :quote, '%s' }" file) #'apprentice-eval-quoted-filter)))

;; (defun apprentice-eval--quote-expression-and-print (expression)
;;   (let ((file (make-temp-file "apprentice-eval" nil ".exs")))
;;     (with-temp-file file
;;       (insert expression))
;;     (apprentice-server-eval (format "{ :quote, '%s' }" file) #'apprentice-eval-quoted-insert-filter)))

;; (defun apprentice-eval-filter (_process output)
;;   (setq apprentice-eval-filter-output (cons output apprentice-eval-filter-output))
;;   (when (apprentice-server-contains-end-marker-p output)
;;     (apprentice-interact-create-popup apprentice-eval-buffer-name
;;                                      (apprentice-server-prepare-filter-output apprentice-eval-filter-output)
;;                                      #'(lambda ()
;;                                          (elixir-mode)
;;                                          (apprentice-eval-mode)
;;                                          (ansi-color-apply-on-region (point-min) (point-max))))
;;     (setq apprentice-eval-filter-output nil)))

;; (defun apprentice-eval-insert-filter (_process output)
;;   (setq apprentice-eval-filter-output (cons output apprentice-eval-filter-output))
;;   (when (apprentice-server-contains-end-marker-p output)
;;     (apprentice-interact-insert-as-comment
;;      (apprentice-server-prepare-filter-output apprentice-eval-filter-output))
;;     (setq apprentice-eval-filter-output nil)))

;; (defun apprentice-eval-quoted-filter (_process output)
;;   (setq apprentice-eval-filter-output (cons output apprentice-eval-filter-output))
;;   (when (apprentice-server-contains-end-marker-p output)
;;     (apprentice-interact-create-popup apprentice-eval-buffer-name
;;                                      (apprentice-server-prepare-filter-output apprentice-eval-filter-output)
;;                                      #'apprentice-eval-mode)
;;     (setq apprentice-eval-filter-output nil)))

;; (defun apprentice-eval-quoted-insert-filter (_process output)
;;   (setq apprentice-eval-filter-output (cons output apprentice-eval-filter-output))
;;   (when (apprentice-server-contains-end-marker-p output)
;;     (apprentice-interact-insert-as-comment
;;      (apprentice-server-prepare-filter-output apprentice-eval-filter-output))
;;     (setq apprentice-eval-filter-output nil)))

;; ;; Public functions

;; (defun apprentice-eval-current-line ()
;;   "Evaluate the Elixir code on the current line."
;;   (interactive)
;;   (apprentice-eval--expression (thing-at-point 'line)))

;; (defun apprentice-eval-print-current-line ()
;;   "Evaluate the Elixir code on the current line and insert the result."
;;   (interactive)
;;   (apprentice-eval--expression-and-print (thing-at-point 'line)))

;; (defun apprentice-eval-region (beg end)
;;   "Evaluate the Elixir code on marked region."
;;   (interactive (list (point) (mark)))
;;   (unless (and beg end)
;;     (error "The mark is not set now, so there is no region"))
;;   (let ((string (buffer-substring-no-properties beg end)))
;;     (apprentice-eval--expression string)))

;; (defun apprentice-eval-print-region (beg end)
;;   "Evaluate the Elixir code on marked region and insert the result."
;;   (interactive (list (point) (mark)))
;;   (unless (and beg end)
;;     (error "The mark is not set now, so there is no region"))
;;   (let ((string (buffer-substring-no-properties beg end)))
;;     (when (> end beg)
;;       (exchange-point-and-mark))
;;     (apprentice-eval--expression-and-print string)))

;; (defun apprentice-eval-buffer ()
;;   "Evaluate the Elixir code in the current buffer."
;;   (interactive)
;;   (let ((string (buffer-substring-no-properties (point-min) (point-max))))
;;     (apprentice-eval--expression string)))

;; (defun apprentice-eval-print-buffer ()
;;   "Evaluate the Elixir code in the current buffer and insert the result."
;;   (interactive)
;;   (let ((string (buffer-substring-no-properties (point-min) (point-max))))
;;     (goto-char (point-max))
;;     (apprentice-eval--expression-and-print string)))

;; (defun apprentice-eval-quoted-current-line ()
;;   "Get the Elixir code representation of the expression on the current line."
;;   (interactive)
;;   (apprentice-eval--quote-expression (thing-at-point 'line)))

;; (defun apprentice-eval-print-quoted-current-line ()
;;   "Get the Elixir code representation of the expression on the current line and insert the result."
;;   (interactive)
;;   (apprentice-eval--quote-expression-and-print (thing-at-point 'line)))

;; (defun apprentice-eval-quoted-region (beg end)
;;   "Get the Elixir code representation of the expression on marked region."
;;   (interactive (list (point) (mark)))
;;   (unless (and beg end)
;;     (error "The mark is not set now, so there is no region"))
;;   (let ((string (buffer-substring-no-properties beg end)))
;;     (apprentice-eval--quote-expression string)))

;; (defun apprentice-eval-print-quoted-region (beg end)
;;   "Get the Elixir code representation of the expression on marked region and insert the result."
;;   (interactive (list (point) (mark)))
;;   (unless (and beg end)
;;     (error "The mark is not set now, so there is no region"))
;;   (let ((string (buffer-substring-no-properties beg end)))
;;     (when (> end beg)
;;       (exchange-point-and-mark))
;;     (apprentice-eval--quote-expression-and-print string)))

;; (defun apprentice-eval-quoted-buffer ()
;;   "Get the Elixir code representation of the expression in the current buffer."
;;   (interactive)
;;   (let ((string (buffer-substring-no-properties (point-min) (point-max))))
;;     (apprentice-eval--quote-expression string)))
;; (defun apprentice-eval-print-quoted-buffer ()
;;   "Get the Elixir code representation of the expression in the current buffer and insert result."
;;   (interactive)
;;   (let ((string (buffer-substring-no-properties (point-min) (point-max))))
;;     (apprentice-eval--quote-expression-and-print string)))

;; (defun apprentice-eval-close-popup ()
;;   "Quit the evaluation buffer window."
;;   (interactive)
;;   (quit-windows-on apprentice-eval-buffer-name))

;; (define-minor-mode apprentice-eval-mode
;;   "Minor mode for Apprentice Elixir code evaluation.

;; \\{apprentice-eval-mode-map}"
;;   nil
;;   " Apprentice-Eval"
;;   apprentice-eval-mode-map)

(provide 'apprentice-eval)

;;; apprentice-eval.el ends here
