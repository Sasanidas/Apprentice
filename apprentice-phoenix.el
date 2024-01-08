;;; apprentice-phoenix.el --- Minor mode for the Phoenix web framework

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

;; Minor mode for the Phoenix web framework

;;; Code:

(require 'apprentice-project)
(require 'apprentice-mix)

(defgroup apprentice-phoenix nil
  "Minor mode for the Phoenix web framework."
  :prefix "apprentice-phoenix-"
  :group 'apprentice)

(defun apprentice-phoenix-web-directory ()
  (concat
   (file-name-as-directory "lib")
   (apprentice-project-name) "_web"))

(defun apprentice-phoenix-app-directory ()
  (concat
   (file-name-as-directory "lib")
   (apprentice-project-name)))

;;;###autoload
(defun apprentice-phoenix-project-p ()
  "Return non-nil if `default-directory' is inside a Phoenix project."
  (and (apprentice-project-p)
       (file-directory-p (concat (apprentice-project-root)
				 (apprentice-phoenix-web-directory)))))


(defun apprentice-phoenix-find-dir (directory)
  (unless (apprentice-phoenix-project-p)
    (error "Could not find a Phoenix Mix project root"))
  (apprentice-project-find-files (apprentice-project-root) directory))

(defun apprentice-phoenix-find-web ()
  (interactive)
  (apprentice-phoenix-find-dir (file-name-as-directory (apprentice-phoenix-web-directory))))

(defun apprentice-phoenix-find-views ()
  (interactive)
  (apprentice-phoenix-find-dir
   (concat (file-name-as-directory (apprentice-phoenix-web-directory)) "views")))

(defun apprentice-phoenix-find-controllers ()
  (interactive)
  (apprentice-phoenix-find-dir
   (concat (file-name-as-directory (apprentice-phoenix-web-directory)) "controllers")))

(defun apprentice-phoenix-find-channels ()
  (interactive)
  (apprentice-phoenix-find-dir
   (concat (file-name-as-directory (apprentice-phoenix-web-directory)) "channels")))

(defun apprentice-phoenix-find-templates ()
  (interactive)
  (apprentice-phoenix-find-dir
   (concat (file-name-as-directory (apprentice-phoenix-web-directory)) "templates")))

(defun apprentice-phoenix-find-models ()
  (interactive)
  (apprentice-phoenix-find-dir (apprentice-phoenix-app-directory)))

(defun apprentice-phoenix-find-static ()
  (interactive)
  (apprentice-phoenix-find-dir (concat
	      (file-name-as-directory
	       (concat (file-name-as-directory (apprentice-project-root)) "priv"))
	      "static")))

(defun apprentice-phoenix-routes (&optional prefix)
  "Run the Mix task \"phoenix.routes\" and list all available Phoenix routes.
It can optionally be called with PREFIX."
  (interactive)
  (apprentice-mix-execute '("phx.routes") prefix))

(defun apprentice-phoenix-router ()
  "Open the \"router.ex\" file from \"PROJECTNAME_web\" directory."
  (interactive)
  (unless (apprentice-phoenix-project-p)
    (error "Could not find an Phoenix Mix project root"))
  (find-file (concat
	      (apprentice-project-root)
	      (file-name-as-directory (apprentice-phoenix-web-directory)) "router.ex")))

(defvar apprentice-phoenix-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n w") #'apprentice-phoenix-find-web)
    (define-key map (kbd "n v") #'apprentice-phoenix-find-views)
    (define-key map (kbd "n c") #'apprentice-phoenix-find-controllers)
    (define-key map (kbd "n l") #'apprentice-phoenix-find-channels)
    (define-key map (kbd "n t") #'apprentice-phoenix-find-templates)
    (define-key map (kbd "n m") #'apprentice-phoenix-find-models)
    (define-key map (kbd "n s") #'apprentice-phoenix-find-static)
    (define-key map (kbd "n r") #'apprentice-phoenix-router)
    (define-key map (kbd "n R") #'apprentice-phoenix-routes)
    map)
  "Keymap for Apprentice Phoenix commands after `apprentice-key-command-prefix'.")

(defvar apprentice-key-command-prefix )

(defvar apprentice-phoenix-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map apprentice-key-command-prefix apprentice-phoenix-command-map)
    map)
  "Keymap for Apprentice Phoenix minor mode.")

(easy-menu-define apprentice-mode-menu apprentice-phoenix-mode-map
  "Menu for Apprentice-Phoenix mode."
  '("Phoenix"
    ("Directory lookup"
     ["Lookup 'app_web' " apprentice-phoenix-find-web]
     ["Lookup 'app_web/views' " apprentice-phoenix-find-views]
     ["Lookup 'app_web/controllers' " apprentice-phoenix-find-controllers]
     ["Lookup 'app_web/channels' " apprentice-phoenix-find-channels]
     ["Lookup 'app_web/templates' " apprentice-phoenix-find-templates]
     ["Lookup 'app' " apprentice-phoenix-find-models]
     ["Lookup 'priv/static'" apprentice-phoenix-find-static])
    ("Mix tasks"
     ["Run 'phx.routes'" apprentice-phoenix-routes])
    ["Open the 'router.ex' file" apprentice-phoenix-router]))

;;;###autoload
(define-minor-mode apprentice-phoenix-mode
  "Minor mode for Elixir Phoenix web framework projects.

The following commands are available:

\\{apprentice-phoenix-mode-map}"
  :lighter " apprentice-phoenix"
  :keymap apprentice-phoenix-mode-map
  :group 'apprentice)

;;;###autoload
(defun apprentice-phoenix-enable-mode ()
  (when (apprentice-phoenix-project-p)
    (apprentice-phoenix-mode)))

;;;###autoload
(dolist (hook '(apprentice-mode-hook))
  (add-hook hook 'apprentice-phoenix-enable-mode))

(provide 'apprentice-phoenix)

;;; apprentice-phoenix.el ends here
