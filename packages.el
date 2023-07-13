;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)
(package! flycheck-pycheckers :recipe (:host github :repo "msherry/flycheck-pycheckers") :disable t)
(package! clang-format+)
(package! groovy-mode)
(package! company-tabnine :disable t)
(package! gdb-mi :recipe (:host github :repo "weirdNox/emacs-gdb" :files ("*.el" "*.c" "*.h" "Makefile")))

(package! gitconfig-mode
	  :recipe (:host github :repo "magit/git-modes"
			 :files ("gitconfig-mode.el")))
(package! gitignore-mode
	  :recipe (:host github :repo "magit/git-modes"
			 :files ("gitignore-mode.el")))

;; (package! undo-io :recipe (:local-repo "undo-io") :files)
