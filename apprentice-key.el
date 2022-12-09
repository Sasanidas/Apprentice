;;; apprentice-key.el --- Key prefix setup for Apprentice related key commands

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

;; Key prefix setup for Apprentice related key commands.

;;; Code:

(defgroup apprentice-key nil
  "Key prefix setup for Apprentice related key commands."
  :prefix "apprentice-key-"
  :group 'apprentice)

(defcustom apprentice-key-command-prefix (kbd "C-c a")
  "The prefix for Apprentice related key commands."
  :type 'string
  :group 'apprentice)

(provide 'apprentice-key)

;;; apprentice-key.el ends here
