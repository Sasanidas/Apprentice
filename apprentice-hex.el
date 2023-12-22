;;; apprentice-hex.el --- Interface to the Hex package manager API. -*- lexical-binding: t -*-

;; Copyright © 2014-2016 Samuel Tonini
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

;;  Interface to the Hex package manager API.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'apprentice-project)
(require 'apprentice-interact)
(require 'elixir-mode)

(declare-function string-split "subr")

(defgroup apprentice-hex nil
  "Interface to the Hex package manager API."
  :prefix "apprentice-test-mode-"
  :group 'apprentice)

(defconst apprentice-hex-api-url "https://hex.pm/api/packages/"
  "Hex package manager API url.")

(defconst apprentice-hexdoc-url "https://hexdocs.pm"
  "Hexdocs url.")

(defconst apprentice-hex-buffer-name "*apprentice-hex*"
  "Name of the hex output buffer.")

(defvar apprentice-hex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    map))

(defun apprentice-hex--fetch-package-info (pkg-name)
  (let* ((inhibit-message t)
         (url (concat apprentice-hex-api-url pkg-name))
         (string
          (with-current-buffer (url-retrieve-synchronously url t)
	    (goto-char (point-min))
	    (search-forward "\n\n")
	    (delete-region (point-min) (point))
            (buffer-string))))
    (when (string-match-p "\"status\":404" string)
      (error "There is no hex package [%s] available" pkg-name))
    (json-read-from-string string)))

(defun apprentice-hex--fetch-search-packages (pkg-name)
  (let* ((inhibit-message t)
         (url (concat apprentice-hex-api-url "?search=" pkg-name))
         (string
          (with-current-buffer (url-retrieve-synchronously url t)
            (goto-char (point-min))
            (search-forward "\n\n")
            (delete-region (point-min) (point))
            (buffer-string))))
    (json-read-from-string string)))

(defun apprentice-hex--deps-name-at-point ()
  "Return the dependency name under the cursor."
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "-_a-z0-9")
      (setq p1 (point))
      (skip-chars-forward "-_a-z0-9")
      (setq p2 (point))
      (buffer-substring-no-properties p1 p2))))

(defun apprentice-hex--display-releases-for (package-name)
  (let* ((infos (apprentice-hex--fetch-package-info package-name))
         (releases (cdr (assoc 'releases infos)))
         (latest-version (cdr (assoc 'version (aref releases 0))))
         (latest-version-url (cdr (assoc 'url (aref releases 0))))
         (buffer (get-buffer-create apprentice-hex-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (erase-buffer)
        (insert (propertize (format "%s versions  (latest version " package-name)
                            'face font-lock-variable-name-face))
        (insert-button latest-version
                       'action (lambda (x) (browse-url (button-get x 'url)))
                       'url (replace-regexp-in-string "\\(api/\\|releases/\\)" "" latest-version-url))
        (insert ")\n\n")
        (mapc (lambda (release)
		(let ((version (cdr (assoc 'version release)))
		      (url (cdr (assoc 'url release)))
		      (date (date-to-time (cdr (assoc 'inserted_at release)))))
		  (insert-button version
				 'action (lambda (x) (browse-url (button-get x 'url)))
				 'url (replace-regexp-in-string "\\(api/\\|releases/\\)" "" url))
		  (insert (format "     (%s %s)"
				  (propertize "released on" 'face font-lock-string-face)
				  (format-time-string "%F" date)))
		  (insert "   (")
		  (insert-button "docs"
				 'face font-lock-constant-face
				 'action (lambda (x) (browse-url (button-get x 'url)))
				 'url (format "%s/%s/%s" apprentice-hexdoc-url package-name version))
		  (insert ")\n")))
	      releases)
        (align-regexp (point-min) (point-max) (concat "\\(\\s-*\\)" "    (") 1 1 t)
        (goto-char (point-min))
        (apprentice-hex-mode)))
    (pop-to-buffer buffer)))

(defun apprentice-hex--display-info-for (package-name)
  (let* ((infos (apprentice-hex--fetch-package-info package-name))
         (releases (cdr (assoc 'releases infos)))
         (latest-release (cdr (assoc 'version (aref releases 0))))
         (meta (assoc 'meta infos))
         (buffer (get-buffer-create apprentice-hex-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (latest-version (cdr (assoc 'version (aref releases 0))))
            (latest-version-url (cdr (assoc 'url (aref releases 0)))))
        (goto-char (point-min))
        (erase-buffer)
        (insert (cdr (assoc 'description meta)))
        (insert "\n\n")
        (insert (propertize "Config: "
                            'face font-lock-string-face))
        (insert (format "{:%s, => \"~> %s\"}" package-name latest-release))
        (insert "\n")
        (insert (propertize "Latest release: " 'face font-lock-string-face))
        (insert-button latest-version
                       'action (lambda (x) (browse-url (button-get x 'url)))
                       'url (replace-regexp-in-string "\\(api/\\|releases/\\)" "" latest-version-url))
        (insert "\n\n")
        (insert (propertize "Maintainers: " 'face font-lock-string-face))
        (mapc (lambda (maintainer)
		(insert (format "\n  - %s" (decode-coding-string maintainer 'utf-8-auto))))
	      (cdr (assoc 'maintainers meta)))
        (insert "\n")
        (insert (propertize "Licenses: " 'face font-lock-string-face))
        (mapc (lambda (license)
		(insert (format "%s" (decode-coding-string license 'utf-8-auto))))
	      (cdr (assoc 'licenses meta)))
        (insert "\n")
        (insert (propertize "Links: " 'face font-lock-string-face))
        (mapc (lambda (link)
		(let ((link-name (car link))
		      (url (decode-coding-string (cdr link) 'utf-8-auto)))
		  (insert (format "\n  %s: " link-name))
		  (insert-button url
				 'action (lambda (x) (browse-url (button-get x 'url)))
				 'url url)))
	      (cdr (assoc 'links meta)))
        (insert "\n")
        (insert (propertize "Releases: \n" 'face font-lock-string-face))
        (mapc (lambda (release)
		(let ((version (cdr (assoc 'version release)))
		      (url (cdr (assoc 'url release))))
		  (insert "  - ")
		  (insert-button version
				 'action (lambda (x) (browse-url (button-get x 'url)))
				 'url (replace-regexp-in-string "\\(api/\\|releases/\\)" "" url))
		  (insert "     (")
		  (insert-button "docs"
				 'face font-lock-constant-face
				 'action (lambda (x) (browse-url (button-get x 'url)))
				 'url (format "%s/%s/%s" apprentice-hexdoc-url package-name version))
		  (insert ")\n")))
	      releases)
        (align-regexp (point-min) (point-max) (concat "\\(\\s-*\\)" "     (") 1 1 t)
        (goto-char (point-min))
        (apprentice-hex-mode)))
    (pop-to-buffer buffer)))

(defun apprentice-hex-search (package-name)
  "Search for Hex package with name PACKAGE-NAME."
  (interactive "Mhex search: \n")
  (let* ((packages (apprentice-hex--fetch-search-packages package-name))
         (buffer (get-buffer-create apprentice-hex-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (erase-buffer)
        (insert (propertize "search results for: "
                            'face font-lock-variable-name-face))
        (insert (propertize (format "%s \n\n" package-name)
                            'face font-lock-builtin-face))
	(cl-loop for package in packages
		 do
		 (let ((name (cdr (assoc 'name package)))
		       (date (date-to-time (cdr (assoc 'inserted_at package))))
		       (version (cdr (assoc 'version (aref (cdr (assoc 'releases package)) 0))))
		       (url (cdr (assoc 'url package)))
		       (latest-release-url (cdr (assoc 'url (aref (cdr (assoc 'releases package)) 0)))))
		   (when (string-match-p package-name name)
		     (insert-button name
				    'action (lambda (x) (browse-url (button-get x 'url)))
				    'url url)
		     (insert "   ")
		     (insert-button version
				    'action (lambda (x) (browse-url (button-get x 'url)))
				    'url (replace-regexp-in-string "\\(api/\\|releases/\\)" "" latest-release-url))

		     (insert (format "     (%s %s)"
				     (propertize "released on" 'face font-lock-string-face)
				     (format-time-string "%F" date)))
		     (insert "   (")
		     (insert-button "docs"
				    'face font-lock-constant-face
				    'action (lambda (x) (browse-url (button-get x 'url)))
				    'url (format "%s/%s/%s" apprentice-hexdoc-url name version))
		     (insert ")\n")
		     (align-regexp (point-min) (point-max) (concat "\\(\\s-*\\)" "  ") 1 1 t))))
        (goto-char (point-min))
        (apprentice-hex-mode)))
    (pop-to-buffer buffer)))

(defun apprentice-hex--get-current-dependencies ()
  (unless (apprentice-project-p)
    (error "No 'mix.exs' file exists"))
  (let* ((mix-content
          (with-temp-buffer
            (insert-file-contents (concat (apprentice-project-root) "mix.exs"))
            (goto-char (point-min))
            (delete-matching-lines "#")
            (buffer-string)))
         (deps-start (with-temp-buffer
                       (insert mix-content)
                       (goto-char (point-min))
                       (search-forward "defp deps do")))
         (deps-end (with-temp-buffer
                     (insert mix-content)
                     (goto-char deps-start)
                     (search-forward "end")))
         (deps (substring mix-content deps-start (- deps-end 4)))
         (deps (replace-regexp-in-string "\\(\\[\\|\\]\\)" "" deps))
         (deps (split-string deps "}\s*,"))
         (deps (mapcar (lambda (dep)
			 (replace-regexp-in-string "\\(\{\\|\}\\|\n\\|^\s*\\)" "" dep))
		       deps))
         (deps (sort (copy-sequence deps) 'equal)))
    (unless (string-match-p "\s*defp? deps do" mix-content)
      (error "No dependency information available in 'mix.exs'"))
    (mapcar (lambda (x) (string-split x "," t " ")) deps)))

(defun apprentice-hex-all-dependencies ()
  "Display Hex package dependencies for the current Mix project."
  (interactive)
  (let* ((deps (apprentice-hex--get-current-dependencies))
	 (content (with-temp-buffer
		    (goto-char (point-min))
		    (erase-buffer)
		    (mapc (lambda (dep)
			    (insert (car dep))
			    (insert (format " %s"(cadr dep)))
			    (insert "\n"))
			  deps)
		    (goto-char (point-min))
		    (align-regexp (point-min) (point-max) (concat "\\(\\s-*\\)" ", ") 1 1 t)
		    (while (search-forward ", " nil t)
		      (replace-match " " nil t))
		    (sort-lines nil (point-min) (point-max))
		    (buffer-string))))
    (apprentice-interact-create-popup apprentice-hex-buffer-name
				      content
				      (lambda ()
					(elixir-mode)
					(apprentice-hex-mode)))))

(defun apprentice-hex-dependency-info ()
  "Display Hex information from dependency."
  (interactive)
  (let* ((dependencies (mapcar (lambda (x)
				 (let ((dep (car x)))
				   (substring (format "%s" dep) 1)))
			       (apprentice-hex--get-current-dependencies)))
	 (dep (completing-read "Dependency: " dependencies)))
    (apprentice-hex-info dep)))

(defun apprentice-hex-info-at-point ()
  "Display Hex package information for the package at point."
  (interactive)
  (apprentice-hex--display-info-for (apprentice-hex--deps-name-at-point)))

(defun apprentice-hex-releases-at-point ()
  "Display Hex package releases for the package at point."
  (interactive)
  (apprentice-hex--display-releases-for (apprentice-hex--deps-name-at-point)))

(defun apprentice-hex-releases (package-name)
  "Display Hex package releases for a PACKAGE-NAME."
  (interactive "Mhex releases: \n")
  (apprentice-hex--display-releases-for package-name))

(defun apprentice-hex-info (package-name)
  "Display Hex package info for PACKAGE-NAME."
  (interactive "Mhex info: \n")
  (apprentice-hex--display-info-for package-name))

(define-minor-mode apprentice-hex-mode
  "Minor mode for displaying Hex package manager information.

\\{apprentice-hex-mode-map}"
  :keymap apprentice-hex-mode-map
  :lighter " Apprentice-Hex"
  (setq buffer-read-only t))

(provide 'apprentice-hex)

;;; apprentice-hex.el ends here
