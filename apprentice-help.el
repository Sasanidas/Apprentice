;;; apprentice-help.el --- Functionality for Elixir documentation lookup -*- lexical-binding: t -*-

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

;; Functionality for Elixir documentation lookup.

;;; Code:

(require 'ansi-color)
(require 'apprentice-project)
(require 'apprentice-scope)

;; (defgroup apprentice-help nil
;;   "Functionality for Elixir documentation lookup."
;;   :prefix "apprentice-help-"
;;   :group 'apprentice)

;; (defcustom apprentice-help-buffer-name "*apprentice help*"
;;   "Name of the Elixir help buffer."
;;   :type 'string
;;   :group 'apprentice-help)

;; (defvar apprentice-help-search-history '()
;;   "Storage for the search history.")

;; (defvar apprentice-help-current-search-text '()
;;   "Stores the current search.")

;; (defvar apprentice-help-filter-output nil)

;; (defface apprentice-help-key-face
;;   '((t (:inherit font-lock-variable-name-face :bold t :foreground "red")))
;;   "Face for the letter keys in the summary."
;;   :group 'apprentice-help)

;; (defun apprentice-help-lookup-doc (search)
;;   "Lookup Elixir documentation for SEARCH."
;;   (setq apprentice-help-current-search-text search)
;;   (setq apprentice-help-filter-output nil)
;;   (if (not (s-blank? search))
;;       (apprentice-server-complete-candidates
;;        (apprentice-help--completion-server-arguments search)
;;        #'apprentice-help-complete-filter-output)
;;     (message "No documentation for [%s] found." search)))

;; (defun apprentice-help-no-doc-available-p (string)
;;   "Return non-nil if STRING contains Elixir no documentation message."
;;   (or (string-match-p "No documentation for" string)
;;       (string-match-p "Could not load module" string)
;;       (string-match-p "it does not have Elixir-style docs" string)
;;       (s-blank? string)))

;; (defun apprentice-help-store-search-in-history ()
;;   "Store the last `apprentice-help-current-search-text' in `apprentice-help-search-history'."
;;   (unless (memq 'apprentice-help-current-search-text apprentice-help-search-history)
;;     (add-to-list 'apprentice-help-search-history apprentice-help-current-search-text)))

;; (defun apprentice-help-display-doc (content)
;;   "Initialize the `apprentice-help-buffer-name' and insert CONTENT."
;;   (let ((default-directory (apprentice-project-root-or-default-dir))
;;         (buffer (get-buffer-create apprentice-help-buffer-name)))
;;     (cond
;;      ((apprentice-help-no-doc-available-p content)
;;       (message (format "No documentation for [%s] found."
;;                        apprentice-help-current-search-text)))
;;      (t
;;       (apprentice-help-store-search-in-history)
;;       (with-current-buffer buffer
;;         (let ((inhibit-read-only t))
;;           (goto-char (point-min))
;;           (erase-buffer)
;;           (insert content)
;;           (goto-char (point-min))
;;           (ansi-color-apply-on-region (point-min) (point-max))
;;           (apprentice-help-minor-mode)))
;;       (pop-to-buffer buffer)))))

;; (defun apprentice-help--search-at-point ()
;;   "Search through `apprentice-help' with the expression under the cursor"
;;   (let* ((expr (apprentice-scope-expression)))
;;     (apprentice-help-lookup-doc (apprentice-help--prepare-search-expr expr))))

;; (defun apprentice-help--search-marked-region (begin end)
;;   "Run `apprentice-help' with the marked region.
;; Argument BEGIN where the mark starts.
;; Argument END where the mark ends."
;;   (let ((expr (buffer-substring-no-properties begin end)))
;;     (apprentice-help-lookup-doc (apprentice-help--prepare-search-expr expr))))

;; (defun apprentice-help--prepare-search-expr (expr)
;;   (let* ((module (apprentice-scope-extract-module expr))
;;          (module (apprentice-scope-alias-full-path module))
;;          (module (if module module ""))
;;          (function (apprentice-scope-extract-function expr))
;;          (function (if function function ""))
;;          (expr (cond
;;                 ((and (not (s-blank? module))
;;                       (not (s-blank? function)))
;;                  (format "%s.%s" module function))
;;                 ((not (s-blank? module))
;;                  module)
;;                 (t
;;                  expr))))
;;     expr))

;; (defun apprentice-help--elixir-modules-to-list (str)
;;   (let* ((str (replace-regexp-in-string "^Elixir\\." "" str))
;;          (modules (split-string str))
;;          (modules (delete nil modules))
;;          (modules (cl-sort modules 'string-lessp :key 'downcase))
;;          (modules (-distinct modules)))
;;     modules))

;; (defun apprentice-help-minor-mode-key-binding-summary ()
;;   (interactive)
;;   (message
;;    (concat "[" (propertize "q" 'face 'apprentice-help-key-face)
;;            "]-quit ["
;;            (propertize "e" 'face 'apprentice-help-key-face)
;;            "]-search-at-point ["
;;            (propertize "m" 'face 'apprentice-help-key-face)
;;            "]-search-module ["
;;            (propertize "s" 'face 'apprentice-help-key-face)
;;            "]-search ["
;;            (propertize "h" 'face 'apprentice-help-key-face)
;;            "]-history ["
;;            (propertize "?" 'face 'apprentice-help-key-face)
;;            "]-keys")))

;; (defun apprentice-help--server-arguments (args)
;;   (if (and (not (equal major-mode 'apprentice-iex-mode))
;;            (not (bound-and-true-p apprentice-help-minor-mode)))
;;       (let* ((modules (apprentice-utils-prepare-modules-for-elixir
;;                        (apprentice-scope-all-modules))))
;;         (format "{ \"%s\", [ context: Elixir, imports: %s, aliases: [] ] }" args modules))
;;     (format "{ \"%s\", [ context: Elixir, imports: [], aliases: [] ] }" args)))

;; (defun apprentice-help--completion-server-arguments (args)
;;   "Build informations about the current context."
;;   (if (and (not (equal major-mode 'apprentice-iex-mode))
;;            (not (bound-and-true-p apprentice-help-minor-mode)))
;;       (let* ((modules (apprentice-utils-prepare-modules-for-elixir
;;                        (apprentice-scope-all-modules)))
;;              (aliases (apprentice-utils-prepare-aliases-for-elixir
;;                        (apprentice-scope-aliases))))
;;         (format "{ \"%s\", [ context: Elixir, imports: %s, aliases: %s ] }" args modules aliases))
;;     (format "{ \"%s\", [ context: Elixir, imports: [], aliases: [] ] }" args)))

;; (defun apprentice-help-complete-filter-output (_process output)
;;   (with-local-quit
;;     (setq apprentice-help-filter-output (cons output apprentice-help-filter-output))
;;     (if (apprentice-server-contains-end-marker-p output)
;;         (let* ((string (apprentice-server-prepare-filter-output apprentice-help-filter-output))
;;                (candidates (apprentice-complete--output-to-list
;;                             (ansi-color-filter-apply string)))
;;                (candidates (if (= (length candidates) 2)
;;                                nil
;;                              candidates)))
;;           (setq apprentice-help-filter-output nil)
;;           (if candidates
;;               (let* ((search (apprentice-complete--completing-prompt apprentice-help-current-search-text candidates)))
;;                 (setq apprentice-help-current-search-text search)
;;                 (apprentice-server-help (apprentice-help--server-arguments search) #'apprentice-help-filter-output))
;;             (apprentice-server-help (apprentice-help--server-arguments apprentice-help-current-search-text) #'apprentice-help-filter-output))))))

;; (defun apprentice-help-filter-output (_process output)
;;   (setq apprentice-help-filter-output (cons output apprentice-help-filter-output))
;;   (if (apprentice-server-contains-end-marker-p output)
;;       (let ((string (apprentice-server-prepare-filter-output apprentice-help-filter-output)))
;;         (apprentice-help-display-doc string)
;;         (setq apprentice-help-current-search-text nil)
;;         (setq apprentice-help-filter-output nil))))

;; (defun apprentice-help-modules-filter (_process output)
;;   (with-local-quit
;;     (setq apprentice-help-filter-output (cons output apprentice-help-filter-output))
;;     (if (apprentice-server-contains-end-marker-p output)
;;         (let* ((output (apprentice-server-prepare-filter-output apprentice-help-filter-output))
;;                (modules (apprentice-help--elixir-modules-to-list output))
;;                (search (completing-read
;;                         "Elixir help: "
;;                         modules
;;                         nil
;;                         nil
;;                         nil))
;;                (module (apprentice-scope-extract-module search))
;;                (function (apprentice-scope-extract-function search))
;;                (search (cond
;;                         ((and module function)
;;                          search)
;;                         ((and module
;;                               (not (string-match-p "[\/0-9]+$" module)))
;;                          (concat module "."))
;;                         (t
;;                          search))))
;;           (apprentice-help-lookup-doc (apprentice-utils-remove-dot-at-the-end search))))))

;; ;; Public functions

;; (defun apprentice-help-search-at-point ()
;;   "Search through `apprentice-help' with the expression under the cursor.

;; If the buffer local variable `mark-active' is non-nil,
;; the actively marked region will be used for passing to `apprentice-help'."
;;   (interactive)
;;   (if mark-active
;;       (apprentice-help--search-marked-region (region-beginning) (region-end))
;;       (apprentice-help--search-at-point)))

;; (defun apprentice-help-module ()
;;   "Load Elixir documentation for the module of the most recent SEARCH.

;; This is helpful to jump from the documentation of, say, the String.split/1
;; function to the documetation of the String module."
;;   (interactive)
;;   (let* ((current-search (car apprentice-help-search-history))
;;          (module (apprentice-scope-extract-module current-search))
;;          (module (apprentice-scope-alias-full-path module)))

;;     (if module
;;         (apprentice-help-lookup-doc (apprentice-help--prepare-search-expr module))
;;       (message "No module found"))))

;; (defvar apprentice-help-minor-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "q") #'quit-window)
;;     (define-key map (kbd "e") #'apprentice-help-search-at-point)
;;     (define-key map (kbd "m") #'apprentice-help-module)
;;     (define-key map (kbd "s") #'apprentice-help)
;;     (define-key map (kbd "h") #'apprentice-help-history)
;;     (define-key map (kbd "M-.") #'apprentice-goto-definition-at-point)
;;     (define-key map (kbd "?") #'apprentice-help-minor-mode-key-binding-summary)
;;     map)
;;   "Keymap for `apprentice-help-minor-mode'.")

;; (define-minor-mode apprentice-help-minor-mode
;;   "Minor mode for displaying elixir help."
;;   :group 'apprentice-help
;;   :keymap apprentice-help-minor-mode-map
;;   (cond (apprentice-help-minor-mode
;;          (setq buffer-read-only t))
;;         (t
;;          (setq buffer-read-only nil))))

;; (defun apprentice-help ()
;;   "Load Elixir documentation for SEARCH."
;;   (interactive)
;;   (setq apprentice-help-filter-output nil)
;;   (apprentice-server-info "{ :type, :modules }" #'apprentice-help-modules-filter))

;; (defun apprentice-help-history (search)
;;   "Load Elixir from the documentation history for SEARCH."
;;   (interactive
;;    (list
;;     (completing-read "Elixir help history: " apprentice-help-search-history nil nil "")))
;;   (apprentice-help-lookup-doc search))

(provide 'apprentice-help)

;;; apprentice-help.el ends here
