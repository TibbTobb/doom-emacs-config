;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;lsp settings
;; (setq  lsp-ui-sideline-enable nil
;;        lsp-ui-doc-enable nil
;;        lsp-enable-on-type-formatting nil
;; lsp-enable-symbol-highlighting t
;; lsp-enable-file-watchers nil
;; lsp-auto-guess-root t
;; lsp-file-watch-threshold 20000
;; )

(setq mode-line-default-help-echo nil
      show-help-function nil
      doom-modeline-vcs-max-length 24
      doom-modeline-env-version nil
      doom-modeline-lsp nil
      doom-modeline-buffer-encoding nil
      pyenv-mode-mode-line-format nil
      gdb-many-windows 1
      magit-save-repository-buffers nil
      magit-inhibit-save-previous-winconf t
      ;; ccls-args '("-log-file=/tmp/ccls.log" "-v=1")
      ;; projectile-require-project-root t
      )

(setq ;doom-theme 'doom-dracula
 doom-font (font-spec :family "JetBrainsMono" :size 12 :weight 'light)
 doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 13))

;; (add-hook! python-mode
;;  (setq flycheck-check-syntax-automatically '(mode-enabled save))
;;  python-shell-interpreter "python")

;; (after! 'lsp-pyright
;;   (setq lsp-pyright-multi-root nil))

(after! ivy
  ;; I prefer search matching to be ordered; it's more precise
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Implicit /g flay on evil ex substitution, because I less often want the
;; default behaviour.
(setq evil-ex-substitute-global t)



;; (setq-default indent-tabs-mode nil)
;; (defun indent-tabs-mode-nil-hook ()
;;  (setq-default indent-tabs-mode nil))
;; (add-hook 'c++-mode-hook 'indent-tabs-mode-nil-hook)
;; (add-hook 'c-mode-hook 'indent-tabs-mode-nil-hook)
;; (add-hook 'python-mode-hook 'indent-tabs-mode-nil-hook)

(c-add-style "undo" '("linux"
                      (c-basic-offset . 4)
                      (c-offsets-alist
                       (statement-cont . ++)
                       (case-label . +))))
(setf (alist-get 'c-mode c-default-style) "undo")
(setf (alist-get 'c++-mode c-default-style) "undo")
(setf (alist-get 'groovy-mode c-default-style) "undo")

;; (appendq! doom-projectile-cache-blacklist '("~/undo.?/3rd_party"))
;; (appendq! projectile-globally-ignored-directories '("~/undo.?/3rd_party"))
;; (setq projectile-globally-ignored-file-suffixes '(".d", ".po", ".ccache*", ".o", ".a"))
;; (after! lsp-mode
;;   (appendq! lsp-file-watch-ignored-directories '("undo.?/3rd_party"
;;                                                  "undo.?/artifacts"
;;                                                  "undo.?/users"
;;                                                  "undo.?/tests/test"
;;                                                  "undo.?/tests/cases"
;;                                                  "undo.?/tests/customer_applications"
;;                                                  "undo.*test_artifacts"
;;                                                  "undo.?/tests/pytest"
;;                                                  "undo.?/release.*"
;;                                                  "undo.?/python-test-deps"
;;                                                  "undo.?/release"
;;                                                  "undo.?/oldrelease"
;;                                                  "undo.?/infra"
;;                                                  "undo.?/examples" "undo.?/docs"
;;                                                  "undo.?/build-tmp.*"
;;                                                  "undo.?/\\.mypy_cache"
;;                                                  "undo.?/\\.pytest_cache"
;;                                                  "undo.?/scripts" "undo.?/_yabs"
;;                                                  "undo.?/build"
;;                                                  "undo.?/private"
;;                                                  "undo.?/aws"
;;                                                  "gcc.?/.*/testsuite"
;;                                                  "undo.?/plugins"
;;                                                  "undo.?/src/experimental"
;;                                                  "undo.?/package_gdb"
;;                                                  "undo.?/logs" ".ccls-cache"
;;                                                  ".git"
;;                                                  ".mypy_cache"
;;                                                  "_yabs")))



;; (map! :after lsp-ui-peek
;;       :map lsp-ui-peek-mode-map
;;       "h" #'lsp-ui-peek--select-prev-file
;;       "C-h" #'lsp-ui-peek--select-prev-file
;;       "j" #'lsp-ui-peek--select-next
;;       "C-j" #'lsp-ui-peek--select-next
;;       "k" #'lsp-ui-peek--select-prev
;;       "C-k" #'lsp-ui-peek--select-prev
;;       "l" #'lsp-ui-peek--select-next-file
;;       "C-l" #'lsp-ui-peek--select-next-file
;;       )

;; (setq lsp-clients-clangd-args '("-j=3"
;;                                 "--background-index"
;;                                 "--clang-tidy"
;;                                 "--completion-style=detailed"
;;                                 "--header-insertion=iwyu"
;;                                 ))
;; (after! lsp-clangd (set-lsp-priority! 'clangd 2))

;; (after! cc-mode (set-eglot-client! 'cc-mode '("clangd" "-j=3" "--background-index" "--clang-tidy" "--completion-style=detailed" "--header-insertion=iwyu")))

(defun magit-process-apply-force-or-cancel-prompt-hook (proc str)
  "Hook method to handle STR in magit process filter with PROC."
  (when-let ((regex "\\[a\\]: \\(?:Apply\\).*\n.*\\[f\\]: Force.*\n.*\\[c\\]: Cancel.*\n.*\\[\\?\\]: Show help.*\n")
             (beg (string-match regex str))
             (choices '(?a ?f ?c))
             (resize-mini-windows t))
    (process-send-string
     proc
     (downcase
      (concat
       (string (magit-process-kill-on-abort proc
                 (let* ((prompt-str nil)
                        (prompt-start-regex "What would you like to do?")
                        (prompt-beg (string-match prompt-start-regex str)))
                   (if prompt-beg
                       (setq prompt-str (substring str prompt-beg))
                     (with-current-buffer (process-buffer proc)
                       (save-excursion
                         (goto-char (point-max))
                         (search-backward prompt-start-regex)
                         (setq prompt-str
                               (concat (string-trim-right
                                        (buffer-substring (point) (point-max)))
                                       "\n" str)))))
                   (read-char-choice prompt-str choices t))))
       "\n")))))

(add-hook 'magit-process-prompt-functions
	  #'magit-process-apply-force-or-cancel-prompt-hook)

;; (setq flycheck-cppcheck-standards '("c99")
;;      flycheck-cppcheck-suppressions-file "/home/tlloyddavies/undo/scripts/static_analysis/c/cppcheck-suppress.xml"
;;      flycheck-cppcheck-checks nil)

;; (setq-default flycheck-disabled-checkers '(groovy))

;; (defun disable-flycheck-in-groovy ()
;;  (flycheck-mode -1))

;; (add-hook 'groovy-mode-hook 'disable-flycheck-in-groovy)

;; (fset 'c-indent-region 'clang-format-region)

(after! format
  (setq +format-on-save-disabled-modes
        '(c++-mode c-mode emacs-lisp-mode sql-mode tex-mode latex-mode yaml-mode json-mode rst-mode)))
(use-package! apheleia)

(defun run-projectile-invalidate-cache (&rest _args)
  ;; We ignore the args to `magit-checkout'.
  (projectile-invalidate-cache nil))
(advice-add 'magit-checkout
            :after #'run-projectile-invalidate-cache)
(advice-add 'magit-branch-and-checkout ; This is `b c'.
            :after #'run-projectile-invalidate-cache)

;; (use-package! eglot-booster
;;   :after eglot
;;   :config (eglot-booster-mode))

(after! eglot
  (add-to-list 'eglot-server-programs
               '(meson-mode . ("mesonlsp" "--lsp"))))

(add-hook 'meson-mode-hook 'eglot-ensure)

;; (after! eglot
;;   (add-to-list 'eglot-server-programs
;;     '(js-mode
;;      . ("vtsls" "--stdio"))))

;; (after! eglot
;;   (add-to-list 'eglot-server-programs
;;       '(js-ts-mode
;;      . ("vtsls" "--stdio"))))

;; (after! eglot
;;   (add-to-list 'eglot-server-programs
;;       '(tsx-ts-mode
;;      . ("vtsls" "--stdio"))))

;; (after! eglot
;;   (add-to-list 'eglot-server-programs
;;       '(typescript-ts-mode
;;      . ("vtsls" "--stdio"))))

;; (after! eglot
;;   (add-to-list 'eglot-server-programs
;;       '(typescript-mode
;;      . ("vtsls" "--stdio"))))

;; (use-package! eglot
;;   :ensure t
;;   :hook ((python-mode python-ts-mode) . eglot-ensure)
;;   :config
;;   (add-to-list 'eglot-server-programs
;;     `((python-ts-mode python-mode) . ("pyrefly" "lsp"))))

;; (after! eglot
;;   (add-to-list 'eglot-server-programs
;;       '(python-ts-mode python-mode eglot-semtok-server "basedpyright-langserver" "--stdio")))

(use-package! eglot
  :ensure t
  :hook ((python-mode python-ts-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
    `((python-ts-mode python-mode) . (eglot-semtok-server "basedpyright-langserver" "--stdio"))))

(use-package! eglot
  :ensure t
  :hook ((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
    `((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)
     . (eglot-semtok-server "clangd" "-j=3" "--background-index" "--clang-tidy" "--completion-style=detailed" "--header-insertion=iwyu"))))

(defface lsp-face-semhl-constant
  '((t :inherit font-lock-constant-face))
  "Face used for semantic highlighting scopes matching constant scopes."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-variable
  '((t :inherit font-lock-variable-name-face))
  "Face used for semantic highlighting scopes matching variable.*.
Unless overridden by a more specific face association."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-function
  '((t :inherit font-lock-function-name-face))
  "Face used for semantic highlighting scopes matching entity.name.function.*.
Unless overridden by a more specific face association."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-method
  '((t :inherit lsp-face-semhl-function))
  "Face used for semantic highlighting scopes matching entity.name.method.*.
Unless overridden by a more specific face association."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-namespace
  '((t :inherit font-lock-type-face :weight bold))
  "Face used for semantic highlighting scopes matching entity.name.namespace.*.
Unless overridden by a more specific face association."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-comment
  '((t (:inherit font-lock-comment-face)))
  "Face used for comments."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-keyword
  '((t (:inherit font-lock-keyword-face)))
  "Face used for keywords."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-string
  '((t (:inherit font-lock-string-face)))
  "Face used for keywords."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-number
  '((t (:inherit font-lock-constant-face)))
  "Face used for numbers."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-regexp
  '((t (:inherit font-lock-string-face :slant italic)))
  "Face used for regexps."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-operator
  '((t (:inherit font-lock-function-name-face)))
  "Face used for operators."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-namespace
  '((t (:inherit font-lock-keyword-face)))
  "Face used for namespaces."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-type
  '((t (:inherit font-lock-type-face)))
  "Face used for types."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-struct
  '((t (:inherit font-lock-type-face)))
  "Face used for structs."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-class
  '((t (:inherit font-lock-type-face)))
  "Face used for classes."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-interface
  '((t (:inherit font-lock-type-face)))
  "Face used for interfaces."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-enum
  '((t (:inherit font-lock-type-face)))
  "Face used for enums."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-type-parameter
  '((t (:inherit font-lock-type-face)))
  "Face used for type parameters."
  :group 'lsp-semantic-tokens)

;; function face already defined, move here when support
;; for theia highlighting gets removed
(defface lsp-face-semhl-member
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for members."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-property
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for properties."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-event
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for event properties."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-macro
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used for macros."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-variable
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for variables."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-parameter
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for parameters."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-label
  '((t (:inherit font-lock-comment-face)))
  "Face used for labels."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-deprecated
  '((t :strike-through t))
  "Face used for semantic highlighting scopes matching constant scopes."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-definition
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face used for definition modifier."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-implementation
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face used for implementation modifier."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-default-library
  '((t :inherit font-lock-builtin-face))
  "Face used for defaultLibrary modifier."
  :group 'lsp-semantic-tokens)

(defface lsp-face-semhl-static
  '((t :inherit font-lock-keyword-face))
  "Face used for static modifier."
  :group 'lsp-semantic-tokens)

(after! eglot-semtok (setq eglot-semtok-faces
  '(("comment" . lsp-face-semhl-comment)
    ("keyword" . lsp-face-semhl-keyword)
    ("string" . lsp-face-semhl-string)
    ("number" . lsp-face-semhl-number)
    ("regexp" . lsp-face-semhl-regexp)
    ("operator" . lsp-face-semhl-operator)
    ("namespace" . lsp-face-semhl-namespace)
    ("type" . lsp-face-semhl-type)
    ("struct" . lsp-face-semhl-struct)
    ("class" . lsp-face-semhl-class)
    ("interface" . lsp-face-semhl-interface)
    ("enum" . lsp-face-semhl-enum)
    ("typeParameter" . lsp-face-semhl-type-parameter)
    ("function" . lsp-face-semhl-function)
    ("method" . lsp-face-semhl-method)
    ("member" . lsp-face-semhl-member)
    ("property" . lsp-face-semhl-property)
    ("event" . lsp-face-semhl-event)
    ("macro" . lsp-face-semhl-macro)
    ("variable" . lsp-face-semhl-variable)
    ("parameter" . lsp-face-semhl-parameter)
    ("label" . lsp-face-semhl-label)
    ("enumConstant" . lsp-face-semhl-constant)
    ("enumMember" . lsp-face-semhl-constant)
    ("dependent" . lsp-face-semhl-type)
    ("concept" . lsp-face-semhl-interface))))

(after! eglot-semtok (setq eglot-semtok-modifier-faces
  '(("declaration" . lsp-face-semhl-interface)
    ("definition" . lsp-face-semhl-definition)
    ("implementation" . lsp-face-semhl-implementation)
    ("readonly" . lsp-face-semhl-constant)
    ("static" . lsp-face-semhl-static)
    ("deprecated" . lsp-face-semhl-deprecated)
    ("abstract" . lsp-face-semhl-keyword)
    ("async" . lsp-face-semhl-macro)
    ("modification" . lsp-face-semhl-operator)
    ("documentation" . lsp-face-semhl-comment)
    ("defaultLibrary" . lsp-face-semhl-default-library))))

;; (after! eglot-semtok (add-to-list 'eglot-semtok-modifier-faces '("builtin" . font-lock-builtin-face)))
;; (after! eglot-semtok (add-to-list 'eglot-semtok-faces '("clsParameter" . font-lock-type-face)))
;; (after! eglot-semtok (add-to-list 'eglot-semtok-faces '("selfParameter" . font-lock-type-face)))
;; (after! eglot-semtok (add-to-list 'eglot-semtok-faces '("concept" . font-lock-type-face)))
;; (after! eglot-semtok (add-to-list 'eglot-semtok-faces '("label" . font-lock-comment-face)))

;; (after! eglot-semtok (add-to-list 'eglot-semtok-faces '("namespace" . font-lock-variable-name-face)))

(after! magit
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(setq auth-sources '("~/.authinfo"))

(after! forge (push '("git.undoers.io"               ; GITHOST
                      "git.undoers.io/api/v3"         ; APIHOST
                      "git.undoers.io"               ; WEBHOST and INSTANCE-ID
                      forge-github-repository)    ; CLASS
                    forge-alist))
