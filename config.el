;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;;
;;lsp settings
(setq lsp-ui-sideline-enable nil
      lsp-ui-doc-enable nil
      lsp-enable-on-type-formatting nil
      lsp-enable-symbol-highlighting t
      ;; lsp-enable-file-watchers nil
      ;; lsp-auto-guess-root t
      lsp-file-watch-threshold 20000
      display-line-numbers-type nil
      mode-line-default-help-echo nil
      show-help-function nil
      doom-modeline-vcs-max-length 24
      doom-modeline-env-version nil
      doom-modeline-lsp nil
      doom-modeline-buffer-encoding nil
      pyenv-mode-mode-line-format nil
      gdb-many-windows 1
      magit-save-repository-buffers nil
      magit-inhibit-save-previous-winconf t
      ccls-args '("-log-file=/tmp/ccls.log" "-v=1")
      ;; projectile-require-project-root t
      )


;; (which-function-mode 1)

(after! ivy
  ;; I prefer search matching to be ordered; it's more precise
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Implicit /g flag on evil ex substitution, because I less often want the
;; default behavior.
(setq evil-ex-substitute-global t)

;; UI
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans"))
;; (setq doom-theme 'doom-dracula)

; (setq doom-font (font-spec :family "JetBrainsMono" :size 12 :weight 'light)
;       doom-variable-pitch-font (font-spec :family "Noto Serif" :size 13)
;       ivy-posframe-font (font-spec :family "JetBrainsMono" :size 15))

(setq ;doom-theme 'doom-dracula
      doom-font (font-spec :family "JetBrainsMono" :size 12 :weight 'light)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 13))

(add-hook! python-mode
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  python-shell-interpreter "python")

(require 'ccls)
(setq ccls-executable "/usr/local/bin/ccls"
      ccls-sem-highlight-method 'font-lock)
;; (setq ccls-initialization-options `(:index (:multiVersion 1 :threads 2)))
;; (setq ccls-initialization-options (append ccls-initialization-options `(:index (:initialBlacklist [".*"]))))
;; (setq ccls-initialization-options `(:index (:threads 1 :initialBlacklist ["."])))
;; (setq ccls-initialization-options `(:index (:trackDependency 1 :threads 2 :initialBlacklist [".*gcc.*testsuite.*"])))
(setq ccls-initialization-options `(:clang (:resourceDir "/usr/lib/llvm-11/lib/clang/11.1.0/") :index (:multiVersion 1 :multiVersionBlacklist ["^/usr/include"] :trackDependency 2 :threads 0 :initialBlacklist [".*gcc.*testsuite.*"  ".*undo.?/release.*" ".*undo.?/tests.*" ".*undo.*build-tmp.*" ".*undo.?/examples" ".*undo.*yabs"])))
;; (setq ccls-initialization-options `(:index (:multiVersion 1 :multiVersionBlacklist ["^/usr/include"] :trackDependency 2 :threads 0 :initialBlacklist [".*gcc.*testsuite.*"  ".*undo.?/release.*" ".*undo.?/tests.*" ".*undo.*build-tmp.*" ".*undo.?/examples" ".*undo.*yabs"])))
;; (setq ccls-initialization-options `(:index (:multiVersion 1 :multiVersionBlacklist ["^/usr/include"] :threads 2 :initialWhitelist ["undo/src" "gcc"] :initialBlacklist [".*gcc.*testsuite.*"])))
;;
(require 'lsp-pyright)
(setq lsp-pyright-multi-root nil)

;; (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))


;; (require 'ccls)
;; (ccls-use-default-rainbow-sem-highlight)

;; (require 'flycheck)
;; (flycheck-define-checker undo-python
;;   "Pylint wrapper to run Undo python linters"
;;   :command ("/home/tlloyddavies/undo/users/mbarisione/scripts/pylint-wrap" ;"--msg-template='{path}:{line}:{msg}'")
;;                                         ;:command ("scripts/run-pylint"
;;                                         ;
;;             "--no-black"
;;             ;; "--reports=n"
;;             ;; "--output-format=text"
;;             ;; "--msg-template={path}:{line}:{column}: {msg_id} ({symbol}) {msg}"
;;                                         ;"--msg-template={abspath}:{line}:{msg}"
;;             ;; "--msg-template={abspath}:{line}:{C}:{msg_id}:{msg}"
;;                                         source-inplace)
;;             ;; source-)
;;   :predicate flycheck-buffer-saved-p
;;   ;; :error-patterns
;;   ;; ((error line-start (file-name) ":" line ":" (message) line-end))
;;   ;; :error-filter
;;   ;; (lambda (errors)
;;   ;;   (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
;;   :error-patterns
;;   ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
;;   ;; ((error line-start (file-name) ":" line ":"
;;   ;;         (or "E" "F") ":"
;;   ;;         (id (one-or-more (not (any ":")))) ":"
;;   ;;         (message) line-end)
;;   ;;  (warning line-start (file-name) ":" line ":"
;;   ;;           (or "W" "R") ":"
;;   ;;           (id (one-or-more (not (any ":")))) ":"
;;   ;;           (message) line-end)
;;   ;;  (info line-start (file-name) ":" line ":"
;;   ;;        (or "C" "I") ":"
;;   ;;        (id (one-or-more (not (any ":")))) ":"
;;   ;;        (message) line-end))
;;   :modes python-mode
;;   :next-checkers (lsp)
;;   )

(require 'dap-python)
(setq dap-python-debugger `debugpy)
(require 'dap-cpptools)


(setq flycheck-cppcheck-standards '("c99")
      flycheck-cppcheck-suppressions-file "/home/tlloyddavies/undo/scripts/static_analysis/c/cppcheck-suppress.xml"
      flycheck-cppcheck-checks nil)

;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; (require 'lsp-ui)
;; (defun my-flycheck-setup ()
;;   (when (equal major-mode 'python-mode)
;;     (add-to-list 'flycheck-checkers 'undo-python)
;;     (setq-local flycheck-checker 'undo-python)))

;; These MODE-local-vars-hook hooks are a Doom thing. They're executed after
;; MODE-hook, on hack-local-variables-hook. Although `lsp!` is attached to
;; python-mode-local-vars-hook, it should occur earlier than my-flycheck-setup
;; this way:
;; (add-hook 'hack-local-variables-hook #'my-flycheck-setup 1)

(setq-default indent-tabs-mode nil)

(defun indent-tabs-mode-nil-hook ()
    (setq-default indent-tabs-mode nil))

(add-hook 'c++-mode-hook 'indent-tabs-mode-nil-hook)
(add-hook 'c-mode-hook 'indent-tabs-mode-nil-hook)
(add-hook 'python-mode-hook 'indent-tabs-mode-nil-hook)

(c-add-style "undo" '("linux"
                      (c-basic-offset . 4)
                      (c-offsets-alist
                       (statement-cont . ++)
                       (case-label . +))))
(setf (alist-get 'c-mode c-default-style) "undo")
(setf (alist-get 'c++-mode c-default-style) "undo")
(setf (alist-get 'groovy-mode c-default-style) "undo")

(setq gdb-non-stop-setting nil)

(setq-default flycheck-disabled-checkers '(groovy))

(defun disable-flycheck-in-groovy ()
  (flycheck-mode -1))

(add-hook 'groovy-mode-hook 'disable-flycheck-in-groovy)

(appendq! doom-projectile-cache-blacklist '("~/undo/3rd_party" "~/undo2/3rd_party"))
(appendq! projectile-globally-ignored-directories '("~/undo/3rd_party" "~/undo2/3rd_party"))
;; (appendq! lsp-file-watch-ignored '("undo.?/3rd_party" "undo.?/artifacts" "undo.?/users" "undo.?/tests/test" "undo.?/release.*" "undo.?/python-test-deps" "undo.?/oldrelease" "undo.?/infra" "undo.?/examples" "undo.?/docs" "undo.?/build-tmp.*" "undo.?/\\.mypy_cache" "undo.?/\\.pytest_cache" "undo.?/scripts" "undo.?/_yabs" "undo.?/build" "gcc.?/.*/testsuite"))
(appendq! lsp-file-watch-ignored-directories '("undo.*3rd_party" "undo.?/artifacts" "undo.?/users" "undo.?/tests/test" "undo.?/tests/cases" "undo.?/tests/customer_applications" "undo.*test_artifacts" "undo.?/tests/pytest" "undo.?/release.*" "undo.?/python-test-deps" "undo.?/release" "undo.?/oldrelease" "undo.?/infra" "undo.?/examples" "undo.?/docs" "undo.?/build-tmp.*" "undo.?/\\.mypy_cache" "undo.?/\\.pytest_cache" "undo.?/scripts" "undo.?/_yabs" "undo.?/build" "gcc.?/.*/testsuite" "undo.?/plugins" "undo.?/src/experimental" "undo.?/package_gdb" "undo.?/logs" ".ccls-cache" ".git" ".mypy_cache" "_yabs"))

;; (require 'forge)
;; (add-to-list 'forge-alist '("git.undoers.io" "git.undoers.io/api/v3" "git.undoers.io" forge-github-repository))

(require 'so-long)
(add-to-list 'so-long-minor-modes 'clang-format+-mode)

;; (add-hook 'c-mode-common-hook #'clang-format+-mode)
;; (setq clang-format+-context 'modification)

(fset 'c-indent-region 'clang-format-region)

(require 'company)
(setq company-global-modes t)

;; (cl-pushnew 'company-tabnine (default-value 'company-backends))
(map! :after lsp-ui-peek
      :map lsp-ui-peek-mode-map
      "h" #'lsp-ui-peek--select-prev-file
      "C-h" #'lsp-ui-peek--select-prev-file
      "j" #'lsp-ui-peek--select-next
      "C-j" #'lsp-ui-peek--select-next
      "k" #'lsp-ui-peek--select-prev
      "C-k" #'lsp-ui-peek--select-prev
      "l" #'lsp-ui-peek--select-next-file
      "C-l" #'lsp-ui-peek--select-next-file
      )

;; (defvar gud-overlay
;;   (let* ((ov (make-overlay (point-min) (point-min))))
;;     (overlay-put ov 'face 'secondary-selection)
;;     ov)
;;   "Overlay variable for GUD highlighting.")

;; (defadvice gud-display-line (after my-gud-highlight act)
;;   "Highlight current line."
;;   (let* ((ov gud-overlay)
;;          (bf (gud-find-file true-file)))
;;     (with-current-buffer bf
;;       (move-overlay ov (line-beginning-position) (line-beginning-position 2)
;;                     ;;(move-overlay ov (line-beginning-position) (line-end-position)
;;                     (current-buffer)))))
;; (defun gud-kill-buffer ()
;;   (if (derived-mode-p 'gud-mode)
;;       (delete-overlay gud-overlay)))

;; (add-hook 'kill-buffer-hook 'gud-kill-buffer)

(setq +format-on-save-enabled-modes
'(not c++-mode c-mode emacs-lisp-mode sql-mode tex-mode latex-mode yaml-mode json-mode))

(fmakunbound 'gdb)
(fmakunbound 'gdb-enable-debug)


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

;; (add-hook `git-commit-setup-hook
;;       git-commit-summary-max-length 72)

(use-package undo-io :load-path "undo-io")
(use-package undo-io-cloud :load-path "undo-io/cloud")

(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=iwyu"
                                ))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

(after! magit (remove-hook `magit-status-sections-hook `magit-insert-stashes) (remove-hook `magit-status-sections-hook `magit-tags-header))

(after! dap-mode
  (setq dap-python-debugger 'debugpy))
