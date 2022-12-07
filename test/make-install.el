;; List of the all the dependencies, including the dev dependencies
(defconst dev-packages '(ert package-lint
			     f dash s
			     company elixir-mode))

;; Initialize package.el
(setq package-user-dir
      (expand-file-name (format ".elpa/%s/elpa" emacs-version)))

(message "Installing in %s ...\n" package-user-dir)
(package-initialize)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(package-refresh-contents)

;; Install dependencies
(mapc (lambda (package)
	(unless (package-installed-p package)
	  (ignore-errors
	    (package-install package))))
      dev-packages)

;; Upgrade dependencies
(save-window-excursion
  (package-list-packages t)
  (condition-case nil
      (progn
        (package-menu-mark-upgrades)
        (package-menu-execute t))
    (error
     (message "All packages up to date"))))
