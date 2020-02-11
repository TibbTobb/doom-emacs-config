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
