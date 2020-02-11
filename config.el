;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;;
(setq lsp-ui-sideline-enable nil
                                        ;lsp-enable-on-type-formatting nil
      lsp-enable-symbol-highlighting nil
      lsp-enable-file-watchers nil
                                        ;display-line-numbers-type 'relative
      display-line-numbers-type nil
      mode-line-default-help-echo nil
      show-help-function nil
      python-shell-interpreter "python")
;; doom-modeline-vcs-max-length 24)

;; (which-function-mode 1)

(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans"))

(add-hook! python-mode
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  python-shell-interpreter "python")

(setq ccls-executable "/home/tlloyddavies/ccls/Release/ccls")

;(require 'lsp-python-ms)
;(setq lsp-python-ms-executable "~/python-language-server/output/bin/Release/ubuntu.18.04-x64/publish/Microsoft.Python.LanguageServer")

 (require 'flycheck)
(flycheck-define-checker undo-python
  "Pylint wrapper to run Undo python linters"
  :command ("/home/tlloyddavies/undo/users/mbarisione/scripts/pylint-wrap" ;"--msg-template='{path}:{line}:{msg}'")
                                        ;:command ("scripts/run-pylint"
            "--reports=n"
            "--output-format=text"
                                        ;"--msg-template={abspath}:{line}:{msg}"
            "--msg-template={abspath}:{line}:{C}:{msg_id}:{msg}"
                                        ;source-inplace)
            source-original)
  ;; :predicate flycheck-buffer-saved-p
  ;; :error-patterns
  ;; ((error line-start (file-name) ":" line ":" (message) line-end))
  ;; :error-filter
  ;; (lambda (errors)
  ;;   (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
  :error-patterns
  ((error line-start (file-name) ":" line ":"
          (or "E" "F") ":"
          (id (one-or-more (not (any ":")))) ":"
          (message) line-end)
   (warning line-start (file-name) ":" line ":"
            (or "W" "R") ":"
            (id (one-or-more (not (any ":")))) ":"
            (message) line-end)
   (info line-start (file-name) ":" line ":"
         (or "C" "I") ":"
         (id (one-or-more (not (any ":")))) ":"
         (message) line-end))
  :modes python-mode
  )


(setq flycheck-cppcheck-standards '("c99")
      flycheck-cppcheck-suppressions-file "scripts/static_analysis/c/cppcheck-suppress.xml"
      flycheck-cppcheck-checks nil)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(require 'lsp-ui)
(defun my-flycheck-setup ()
  (when (equal major-mode 'python-mode)
    (add-to-list 'flycheck-checkers 'undo-python)
    (setq-local flycheck-checker 'undo-python)))

;; These MODE-local-vars-hook hooks are a Doom thing. They're executed after
;; MODE-hook, on hack-local-variables-hook. Although `lsp!` is attached to
;; python-mode-local-vars-hook, it should occur earlier than my-flycheck-setup
;; this way:
(add-hook 'hack-local-variables-hook #'my-flycheck-setup 1)

(setq-default indent-tabs-mode nil)
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

(require 'forge)
(add-to-list 'forge-alist '("git.undoers.io" "git.undoers.io/api/v3" "git.undoers.io" forge-github-repository))

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
      "j" #'lsp-ui-peek--select-next
      "k" #'lsp-ui-peek--select-prev
      "l" #'lsp-ui-peek--select-next-file
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
